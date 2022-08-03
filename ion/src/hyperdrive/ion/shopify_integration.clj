(ns hyperdrive.ion.shopify-integration
  (:require [clojure.set :refer [rename-keys]]
            [clojure.string :as str]
            [clojure.core.async :refer [chan go-loop alt!]]
            [clojure.data.json :as json]
            [datascript.core :as d]
            [org.httpkit.client :as http]
            [hyperdrive.db :as db :refer [conn shopify-sync-queue]]
            [hyperdrive.queries :as q]
            [hyperdrive.util :as u :refer [fmap no-nils new-entity gen-code parse-date http-error-code? fixture]]
            [hyperdrive.ion.config :as config]
            [hyperdrive.ion.logging :as log]))


(defn- money-string [x]
  (format "%.2f" (float x))) ; Also rounds, which is nice since that's the behavior of `currency` on client-side.


(def ^:private item-id-tag-prefix "hd-")

(def ^:private inventory-tracked? true)
(def ^:private inventory-policy "deny")
(def ^:private fulfillment-service nil)
(def ^:private taxable? true)
(def ^:private requires-shipping? true)
(def ^:private option1-name "Color")
(def ^:private option2-name "Size")

(def ^:private product-title :item/name)
(def ^:private product-vendor #(q/fact (:item/manufacturer %) :manufacturer/name))
(def ^:private product-type #(q/fact (:item/category %) :category/name))
(def ^:private product-tags (fn [item]
                              (concat
                               [(str item-id-tag-prefix (:e/id item))]
                               (:item/tags item)
                               ;; TODO Nearer the end fix this with (hopefully) a better general category recursion solution I've come up with. Even if no such solution exists still rewrite this so it works with categories more than four levels deep.
                               (remove
                                nil?
                                [(q/fact (:item/category item) :category/name)
                                 (some-> (q/fact (:item/category item) :category/parent) (q/fact :category/name))
                                 (some-> (q/fact (:item/category item) :category/parent) (q/fact :category/parent) (q/fact :category/name))
                                 (some-> (q/fact (:item/category item) :category/parent) (q/fact :category/parent) (q/fact :category/parent) (q/fact :category/name))]))))
(def ^:private variant-barcode :sku/upc)
(def ^:private variant-price (fn [item sku]
                               (money-string (or (:sku/online-price sku)
                                                 (:sku/price sku)
                                                 (:item/online-price item)
                                                 (:item/price item)))))
(def ^:private variant-sku :e/id)
(def ^:private option1-value :sku/color)
(def ^:private option2-value :sku/size)
(def ^:private available-quantity #(db/stock (:e/id %)))




;;;; Sync queue. Exit channel and reified processing-loop allows starting/stopping in dev (see repl.clj).

(declare processing-loop)
(declare sync-tx)

(defn create-processing-loop! []
  (let [exit-ch (chan)]
    (go-loop []
      (alt!
        shopify-sync-queue ([v]
                            (try
                              (let [[env tx-report] v]
                                ;; TODO
                                #_(config/bind-env
                                   env
                                   (sync-tx tx-report)))
                              (catch Exception e
                                (log/error "Shopify sync loop exception." e)))
                            (recur))
        exit-ch nil))
    exit-ch))

;; TODO also the commented out `put!` in db.cljc: (defonce processing-loop (atom (create-processing-loop!)))
;; TODO also if I'm keeping this it should be dynamically rebound like conn, stock-cache, etc.
(defonce processing-loop nil)




;;;; Queue processing

(defn- api-req* [store access-token data]
  (let [res @(http/request {:url (str "https://" store ".myshopify.com/admin/api/2019-10/graphql.json")
                            :method :post
                            :headers {"Content-Type" "application/json"
                                      "X-Shopify-Access-Token" access-token}
                            :body (json/write-str data)})
        body (json/read-str (:body res) :key-fn keyword)]
    (if-let [e (or (:error res)
                   (:errors body)
                   (seq (keep #(seq (:userErrors (val %))) (:data body)))
                   (when (http-error-code? (:status res))
                     (str "HTTP Status " (:status res))))]
      (log/error "Shopify API error." e)
      body)))

(defn- api-req [data]
  (let [store (q/get-setting :settings/shopify-store)
        access-token (q/get-setting :settings/shopify-access-token)]
    (when (and store access-token)
      (api-req* store access-token data))))

(def ^:private product-id-query "query productId($q: String) {
  products(query: $q, first: 1) {
    edges {
      node {
        id
      }
    }
  }
}")

(defn- get-product-id [id]
  (get-in
   (api-req {:query product-id-query
             :variables {:q (str "tag:" item-id-tag-prefix id)}})
   [:data :products :edges 0 :node :id]))

(def ^:private product-create-mutation "mutation productCreate($input: ProductInput!) {
  productCreate(input: $input) {
    userErrors {
      field
      message
    }
  }
}")

(def ^:private location-id-query "query locationId {
  locations(first: 1) {
    edges {
      node {
        id
      }
    }
  }
}")

;; Just in case the api doesn't return correctly (possibly because of some transient error) don't cache a nil result.
(defn- memoize-some [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (when-let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

(def ^:private get-location
  (let [get-location* (memoize-some (fn [_]
                                      (get-in
                                       (api-req {:query location-id-query})
                                       [:data :locations :edges 0 :node :id])))]
    (fn []
      (get-location* (config/get-env)))))

(defn- cpv-input [sku-id item]
  (let [sku (q/entity sku-id)]
    (merge {;; TODO :productId ...
            :barcode (variant-barcode sku)
            :inventoryPolicy inventory-policy
            :inventoryQuantities {:availableQuantity (available-quantity sku)
                                  :locationId (get-location)}
            :options [(option1-value sku) (option2-value sku)]
            :price (variant-price item sku)
            :requiresShipping requires-shipping?
            :sku (variant-sku sku)
            :taxable taxable?}
           (when inventory-tracked?
             {:inventoryItem {:tracked true}})
           (when-let [fs fulfillment-service]
             {:fulfillmentServiceId fs}))))

(defn- cp-input []
  (let [item (q/entity "xns3rk47")
        skus (map q/entity (d/q '[:find [?eid ...]
                                  :in $ ?item
                                  :where
                                  [?e :sku/item ?item]
                                  [?e :e/id ?eid]]
                                @conn (:e/id item)))]
    {:images (->> (cons (:item/images item) (map :sku/images skus))
                  (apply concat)
                  (map (fn [image]
                         {:src (u/image-url image)})))
     :options [option1-name option2-name]
     :productType (product-type item)
     :tags (product-tags item)
     :title (product-title item)
     :vendor (product-vendor item)
     :variants (map #(cpv-input (:e/id %) item) skus)}))

(defn- create-product! [input]
  (api-req {:query product-create-mutation
            :variables {:input input}}))

(def ^:private product-update-mutation "mutation productUpdate($input: ProductInput!) {
  productUpdate(input: $input) {
    userErrors {
      field
      message
    }
  }
}")

(defn- update-product! [id fields]
  (api-req {:query product-update-mutation
            :variables {:input (merge {:id id} fields)}}))

(def ^:private customer-id-query "query customerId($q: String) {
  customers(query: $q, first: 1) {
    edges {
      node {
        id
      }
    }
  }
}")

(defn- get-customer-id [email]
  (get-in
   (api-req {:query customer-id-query
             :variables {:q (str "email:" email)}})
   [:data :customers :edges 0 :node :id]))

(def ^:private customer-update-mutation "mutation customerUpdate($input: CustomerInput!) {
  customerUpdate(input: $input) {
    userErrors {
      field
      message
    }
  }
}")

(defn- update-customer! [id fields]
  (api-req {:query customer-update-mutation
            :variables {:input (merge {:id id} fields)}}))

(def ^:private inventory-item-id-query "query inventoryItemId($q: String) {
  inventoryItems(query: $q, first: 1) {
    edges {
      node {
        id
      }
    }
  }
}")

(defn- get-inventory-item-id [sku]
  (get-in
   (api-req {:query inventory-item-id-query
             :variables {:q (str "sku:" sku)}})
   [:data :inventoryItems :edges 0 :node :id]))

(def ^:private inventory-activate-mutation "mutation inventoryActivate($inventoryItemId: ID!, $locationId: ID!, $available: Int) {
  inventoryActivate(inventoryItemId: $inventoryItemId, locationId: $locationId, available: $available) {
    userErrors {
      field
      message
    }
  }
}")

(defn- set-available-inventory [sku available]
  (api-req {:query inventory-activate-mutation
            :variables {:inventoryItemId (get-inventory-item-id sku)
                        :locationId (get-location)
                        :available available}}))

(def ^:private mappings
  {:item/name :title
   :customer/first-name :firstName
   :customer/last-name :lastName
   :customer/mobile-phone :phone
   :customer/notes :note})

(defn- sync-tx [{db :db-after, :keys [tx-data]}]
  (when (q/get-setting :settings/shopify-enabled?)
    ;; TODO remove 300-count guard and println
    (when (< (count tx-data) 300)
      (println "sync tx: " tx-data)
      (as-> tx-data $
        (filter #(some #{(:a %)} (keys mappings)) $)
        (let [assertions (filter :added $)
              retractions (remove :added $)]
          (->> retractions
               (remove (fn [retraction]
                         (let [about-fact (juxt :e :a)]
                           (some #(= (about-fact %) (about-fact retraction)) assertions))))
               (concat assertions)))
        (group-by :e $)
        (fmap (fn [datoms]
                (->> datoms
                     (map (fn [d]
                            [(get mappings (:a d)) (when (:added d) (:v d))]))
                     (into {}))) $)
        (doseq [[id fields] $]
          (case (:e/type (d/pull db [:e/type] id))
            :type/item
            (if-let [shopify-id (get-product-id (:e/id (d/pull db [:e/id] id)))]
              (update-product! shopify-id fields)
              ;; TODO  (create-product! )
              )
            :type/sku
            nil ; TODO
            #_(if-let [shopify-id (get-variant-id (:e/id (d/pull db [:e/id] id)))]
                (update-variant! shopify-id fields)
                ;; TODO (create-variant! ...)
                )
            :type/customer
            (when-let [email (:customer/email (d/pull db [:customer/email] id))]
              (when-let [shopify-id (get-customer-id email)]
                (update-customer! shopify-id fields)))))))))




;;;; Webhooks

(defn- create-statement [m]
  [(no-nils m)])

(defn- update-statements [m id retract-omitted?]
  ;; TODO I can get rid of lookup-ref and replace usages with just id after the datomic rewrite.
  (let [lookup-ref [:e/id (:e/id (d/pull @conn '[:e/id] id))]]
    (keep (fn [[a v]]
            (if v
              [:db/add lookup-ref a v]
              (when retract-omitted?
                [:db.fn/retractAttribute lookup-ref a])))
          m)))

(defn- existing-entity [attribute value]
  (ffirst
   (d/q '[:find ?e
          :in $ ?attr ?v
          :where [?e ?attr ?v]]
        @conn attribute value)))

(defn- select-and-rename-keys [m kmap]
  (as-> m $
    (select-keys $ (keys kmap))
    (rename-keys $ kmap)
    (fmap #(when-not (= % "") %) $)))

(defn- create-customer [customer]
  (merge
   (dissoc (new-entity :type/customer) :e/date)
   {:e/date (if-let [created-at (:created_at customer)]
              (parse-date created-at)
              (:e/date (new-entity :type/customer)))
    :customer/email (:email customer)
    :customer/employee (fixture :fixture/employee-ecom)}))

(defn- update-customer [customer]
  (merge
   (select-and-rename-keys customer
                           {:first_name :customer/first-name
                            :last_name :customer/last-name
                            :note :customer/notes
                            :phone :customer/mobile-phone})
   (select-and-rename-keys (let [addresses (:addresses customer)]
                             (or (first (filter #(:default %) addresses))
                                 (first addresses)))
                           {:company :customer/company
                            :address1 :customer/address
                            :address2 :customer/address2
                            :city :customer/city
                            :province :customer/state
                            :zip :customer/zipcode
                            :country_code :customer/country})))

(defn- webhook-customer [customer]
  (when-let [email (:email customer)]
    (if-let [id (existing-entity :customer/email email)]
      (update-statements (update-customer customer) id false)
      (create-statement (merge
                         (create-customer customer)
                         (update-customer customer))))))

(defn- parse-money [x]
  (try
    (Double/parseDouble x)
    (catch Exception _)))

(defn- discount-per-item [order]
  (let [total (or (parse-money (:total_discounts order)) 0)
        num-items (apply + (map :quantity (:line_items order)))]
    (double (/ total num-items))))

(defn- create-order [order]
  (merge
   (dissoc (new-entity :type/sale) :e/date)
   {:e/date (if-let [created-at (:created_at order)]
              (parse-date created-at)
              (:e/date (new-entity :type/sale)))
    :sale/sale-type :sale.type/ecom
    :sale/shopify-order-id (:id order)
    ;; :sale/transactions (and maybe :sale/lines) could change after initial order creation but making the changes would be difficult so just assume they don't for now.
    :sale/lines (vec (mapcat (fn [{:keys [sku price quantity]}]
                               (repeat quantity {:id (u/gen-id)
                                                 :line-type :sku
                                                 :sku sku
                                                 :price (- (parse-money price) (* (discount-per-item order) quantity))
                                                 :employee (fixture :fixture/employee-ecom)}))
                             (:line_items order)))
    :sale/items (vec (->> (:line_items order)
                          (map #(q/fact (:sku %) :sku/item))
                          distinct))
    :sale/code (gen-code @conn)
    :sale/transactions [(merge (new-entity :type/transaction)
                               {:transaction/line-type :type/payment-type
                                :transaction/source (fixture :fixture/payment-type-paypal) ; TODO just do paypal for now since that's all they're taking and I might be getting rid of fixtures.
                                :transaction/amount (parse-money (:total_price order))
                                :transaction/register (fixture :fixture/register-ecom)
                                :transaction/employee (fixture :fixture/employee-ecom)})]
    :sale/register (fixture :fixture/register-ecom)
    :sale/employee (fixture :fixture/employee-ecom)}))

(defn- update-order [order]
  {:sale/ecom-status (case (:fulfillment_status order)
                       "fulfilled" :shipped
                       "restocked" :canceled
                       :not-shipped)
   :sale/customer (when-let [email (:email order)]
                    (ffirst
                     (d/q '[:find ?eid
                            :in $ ?email
                            :where
                            [?e :customer/email ?email]
                            [?e :e/id ?eid]]
                          @conn email)))
   :sale/no-tax? (when (zero? (or (parse-money (:total_tax order)) 0))
                   true)
   :sale/shipping-cost (let [shipping (parse-money (get-in order [:total_shipping_price_set :shop_money :amount]))]
                         (when-not (zero? shipping)
                           shipping))})

(defn- webhook-order [order]
  (if-let [id (existing-entity :sale/shopify-order-id (:id order))]
    (update-statements (update-order order) id true)
    (create-statement (merge
                       (create-order order)
                       (update-order order)))))

(defn handle-webhook! [topic data]
  ;; TODO remove both prints
  (println "webhook triggered!" topic)
  (clojure.pprint/pprint data)
  (when-let [tx (cond
                  (some #{topic} ["customers/create"
                                  "customers/update"])
                  (webhook-customer data)
                  (some #{topic} ["orders/create"
                                  "orders/updated"
                                  "orders/fulfilled"
                                  "orders/cancelled"])
                  (webhook-order data))]
    (db/persist! tx)))




;;;; Scripts

;;; Generate product csv

(defn- t-or-f-string [x]
  (if x "TRUE" "FALSE"))

(defn- section [lines]
  (mapcat (fn [lines]
            (->> lines
                 (map-indexed vector)
                 (map (fn [[index line]]
                        (cons (zero? index) line)))))
          lines))

;; TODO When rewriting with datomic rewrite usages of `q/fact` as nested pull selections. Use ":sku/_item".
(defn- product-csv [{:keys [addendum manufacturer]}]
  (cons
   ["Handle" "Title" "Body (HTML)" "Vendor" "Type" "Tags" "Published" "Option1 Name" "Option1 Value" "Option2 Name" "Option2 Value" "Option3 Name" "Option3 Value" "Variant SKU" "Variant Grams" "Variant Inventory Tracker" "Variant Inventory Qty" "Variant Inventory Policy" "Variant Fulfillment Service" "Variant Price" "Variant Compare At Price" "Variant Requires Shipping" "Variant Taxable" "Variant Barcode" "Image Src" "Image Position" "Image Alt Text" "Gift Card" "SEO Title" "SEO Description" "Google Shopping / Google Product Category" "Google Shopping / Gender" "Google Shopping / Age Group" "Google Shopping / MPN" "Google Shopping / AdWords Grouping" "Google Shopping / AdWords Labels" "Google Shopping / Condition" "Google Shopping / Custom Product" "Google Shopping / Custom Label 0" "Google Shopping / Custom Label 1" "Google Shopping / Custom Label 2" "Google Shopping / Custom Label 3" "Google Shopping / Custom Label 4" "Variant Image" "Variant Weight Unit" "Variant Tax Code" "Cost per item"]
   (let [initial? (some? addendum)
         lines (section
                (for [item (apply concat (d/q (concat
                                               '[:find (pull ?e [:db/id
                                                                 :e/id
                                                                 :item/name
                                                                 :item/manufacturer
                                                                 ;; {:item/manufacturer [:manufacturer/name]}
                                                                 :item/category
                                                                 ;; {:item/category [:category/name]}
                                                                 :item/tags
                                                                 :item/online-price
                                                                 :item/price
                                                                 :item/default-cost
                                                                 :item/images
                                                                 ;; :sku/_item [:sku/color
                                                                 ;;             :sku/size
                                                                 ;;             etc]
                                                                 ])
                                                 :in $ ?manufacturer
                                                 :where
                                                 [?e :e/type :type/item]]
                                               (when manufacturer
                                                 '[[?e :item/manufacturer ?manufacturer]]))
                                              @conn manufacturer))] ; TODO datomic `d/q` doesn't seem to accept nil :in inputs, hopefully finda a better way to do conditional clauses. Doing `(or manufacturer "")` here would word too.
                  (section
                   (for [sku (apply concat (d/q '[:find (pull ?e [:e/id
                                                                  :sku/color
                                                                  :sku/size
                                                                  :sku/style-number
                                                                  :sku/online-price
                                                                  :sku/price
                                                                  :sku/upc
                                                                  :sku/default-cost
                                                                  :sku/images])
                                                  :in $ ?item
                                                  :where [?e :sku/item ?item]]
                                                @conn (:e/id item)))]
                     (for [image (or (seq (map u/image-url
                                               (or (seq (:sku/images sku))
                                                   (:item/images item))))
                                     [nil])]
                       [item sku image])))))]
     (map (fn [[product-line? variant-line? item sku image]]
            (map
             #(if (nil? %) "" (str %))
             (concat
              [(:db/id item)]
              (if product-line?
                [(product-title item)
                 (when initial?
                   (let [item-addendum (get addendum (:e/id item))]
                     (str (:item/short-description item-addendum) (:item/long-description item-addendum))))
                 (product-vendor item)
                 (product-type item)
                 (str/join ", " (product-tags item))]
                (repeat 5 nil))
              (if variant-line?
                [(when initial?
                   ;; TODO Docs say leaving this blank will publish so I don't think this works for updating the way I want. In fact check if I can really use the csv file to update after initial import the way I'm imagining. Maybe I have to omit the column from the csv file entirely?
                   (t-or-f-string (get-in addendum [(:e/id item) :item/webstore?])))
                 option1-name
                 (option1-value sku)
                 option2-name
                 (option2-value sku)
                 nil
                 nil
                 (variant-sku sku)
                 nil
                 (when inventory-tracked? "shopify")
                 (available-quantity sku)
                 inventory-policy
                 (or fulfillment-service "manual")
                 (variant-price item sku)
                 nil
                 (t-or-f-string requires-shipping?)
                 (t-or-f-string taxable?)
                 (variant-barcode sku)
                 image
                 nil
                 nil
                 (t-or-f-string false)
                 nil
                 nil
                 nil nil nil nil nil nil nil nil nil nil nil nil nil
                 (when-let [image (or (first (:sku/images sku)) (first (:item/images item)))]
                   (u/image-url image))
                 nil
                 nil
                 nil]
                (concat (repeat 18 nil) [image] (repeat 22 nil))))))
          lines))))

(defn product-csv-initial [addendum opts]
  (product-csv (assoc opts :addendum addendum)))

(def product-csv-update product-csv)



;;; Set up webhooks

(def ^:private webhooks-query "query {
  webhookSubscriptions(first: 100) {
    edges {
      node {
        id
      }
    }
  }
}")

(defn- get-webhooks [store access-token]
  (as-> (api-req*
         store access-token
         {:query webhooks-query}) $
    (get-in $ [:data :webhookSubscriptions :edges])
    (map #(get-in % [:node :id]) $)))

(def ^:private webhook-create-mutation "mutation webhookSubscriptionCreate($topic: WebhookSubscriptionTopic!, $webhookSubscription: WebhookSubscriptionInput!) {
  webhookSubscriptionCreate(topic: $topic, webhookSubscription: $webhookSubscription) {
    userErrors {
      field
      message
    }
  }
}")

(defn- create-webhook [store access-token callback-url-base topic]
  (api-req*
   store access-token
   {:query webhook-create-mutation
    :variables {:topic topic
                :webhookSubscription {:callbackUrl (str "https://" callback-url-base "/shopify-webhooks")
                                      :format "JSON"}}}))

(def ^:private delete-webhook-mutation "mutation webhookSubscriptionDelete($id: ID!) {
  webhookSubscriptionDelete(id: $id) {
    userErrors {
      field
      message
    }
  }
}")

(defn- delete-webhook [store access-token id]
  (api-req*
   store access-token
   {:query delete-webhook-mutation
    :variables {:id id}}))

(defn ensure-webhooks! [store access-token callback-url-base]
  (doseq [id (get-webhooks store access-token)]
    (delete-webhook store access-token id))
  (doseq [topic ["CUSTOMERS_CREATE"
                 "CUSTOMERS_UPDATE"
                 "ORDERS_CREATE"
                 "ORDERS_UPDATED"
                 "ORDERS_FULFILLED"
                 "ORDERS_CANCELLED"]]    
    (create-webhook store access-token callback-url-base topic)))
