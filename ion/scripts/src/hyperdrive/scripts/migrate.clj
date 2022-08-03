(ns hyperdrive.scripts.migrate
  (:require [clojure.edn :as edn]
            [clojure.java.io :refer [writer make-parents]]
            [clojure.pprint :refer [pprint]]
            [clojure.set :refer [map-invert rename-keys]]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [org.httpkit.client :as http]
            ;; [hyperdrive.spec :as spec]
            [hyperdrive.util :refer [seqify gen-id new-entity parse-date filestack-api-key http-error-code?]]))


(defn output-dump-dir [output-to]
  output-to)

(defn dump-file [output-to type]
  (str (output-dump-dir output-to) "/" type ".edn"))

(defn migrate-file [file output-to]
  (str output-to "/migrate/" file))

(def db-file (partial migrate-file "db.edn"))
(def bots-file (partial migrate-file "bots.edn"))
(def addendum-file (partial migrate-file "addendum.edn"))
(def db-1-file (partial migrate-file "db_1.edn"))
(def hashes-and-images-file (partial migrate-file "hashes-and-images.edn"))
(def mappings-file (partial migrate-file "mappings.edn"))

(def hashes-and-handles-file ".migration-external/hashes-and-handles.edn")



;;;; Util

(def one (comp first filter))

(defn safe-read-edn-file [filename]
  (when-let [s (try
                 (slurp filename)
                 (catch java.io.FileNotFoundException _))]
    (edn/read-string s)))

(defn pretty-write [file content]
  (make-parents file)
  (binding [*print-namespace-maps* false]
    (pprint content (writer file))))



;;;; Dump

(def access-token (atom nil))

(defn refresh-token! [params]
  (println "Refreshing token.")
  (let [res @(http/request
              {:url "https://cloud.merchantos.com/oauth/access_token.php"
               :method :post
               :form-params {:refresh_token (:refresh-token params)
                             :client_id (:client-id params)
                             :client_secret (:client-secret params)
                             :grant_type "refresh_token"}})]
    (if-let [e (or (:error res)
                   (when (http-error-code? (:status res))
                     (str "HTTP Status " (:status res))))]
      (do
        (println "Refreshing token failed:" e)
        (Thread/sleep 10000)
        (refresh-token! params))
      (reset! access-token (-> (:body res)
                               (json/read-str :key-fn keyword)
                               :access_token)))))


(def batch-size 100)

(defn to-int-safe [s]
  (when s
    (Integer/parseInt s)))

(defn fetch [{:keys [access-token-params subset]} type offset]
  (let [raw-body (loop []
                   (let [res @(http/request
                               {:url (str "https://api.merchantos.com/API/Account/57195/" type ".json")
                                :method :get
                                :headers {"Authorization" (str "OAuth " @access-token)}
                                :query-params (merge
                                               {:load_relations "all"
                                                ;; :orderby "createTime"
                                                ;; :orderby_desc 1
                                                :limit batch-size}
                                               (when offset
                                                 {:offset offset})
                                               (when (and subset (= type "Customer"))
                                                 {:customerTypeID (:customer-type-id subset)})
                                               (when (and subset (some #{type} ["Manufacturer" "ItemMatrix" "Item"]))
                                                 {:manufacturerID (:manufacturer-id subset)}))})]
                     (if-let [e (or (:error res)
                                    (when (http-error-code? (:status res))
                                      (str "HTTP Status " (:status res))))]
                       (case (:status res)
                         401 (do
                               (println "Token needs a refresh.")
                               (refresh-token! access-token-params)
                               (recur))
                         429 (do
                               (println "Rate limited, waiting a bit before continuing.")
                               (Thread/sleep 10000)
                               (recur))
                         (do
                           ;; (throw (ex-info "Fetching failed." {:error e}))
                           (println "Fetching failed, waiting before trying to continue. Error was:" e)
                           (Thread/sleep 10000)
                           (recur)))
                       (:body res))))
        body (json/read-str raw-body :key-fn (fn [s]
                                               (if (= s "@attributes")
                                                 :meta-attributes (keyword s))))
        attrs (:meta-attributes body)
        objects (->> (get body (keyword type))
                     seqify
                     (map #(dissoc % :Items :Manufacturer :Category)))]
    {:total (to-int-safe (:count attrs))
     :current (to-int-safe (:offset attrs))
     :objects objects}))

(defn fetch-all [opts type]
  (loop [results (transient [])
         offset 0]
    (let [res (fetch opts type offset)]
      (if-let [objects (seq (:objects res))]
        (do
          (println (str (:current res) " / " (:total res)))
          (Thread/sleep 1000) ; The api request to lightspeed itself takes some time, even 1s is probably more than necessary.
          (recur (loop [rs results
                        os objects]
                   (if-let [o (first os)]
                     (recur (conj! rs o)
                            (rest os))
                     rs))
                 (+ offset batch-size)))
        (persistent! results)))))

(defn save [opts type]
  (let [out-file (dump-file (:output-to opts) type)
        results (fetch-all opts type)]
    (pretty-write out-file results)))

(defn dump* [opts types]
  (refresh-token! (:access-token-params opts))
  (doseq [type types]
    (println (str "Starting " type " dump..."))
    (save opts type)
    (newline)))

(defn dump [opts]
  (dump* opts (concat ["Shop"
                       "Manufacturer"
                       "Category"
                       "ItemMatrix"
                       "Item"
                       "Customer"
                       "CustomerType"]
                      (when-not (:subset opts)
                        ["CreditAccount"]))))

(defn dump-extra [opts]
  (dump* opts ["Sale"
               "SpecialOrder"
               "Employee"
               "Vendor"]))



;;;; Migrate

(defn date-part [s]
  (first (str/split s #"T")))


;;; Lightspeed puts zeroes and empty strings and stuff to mean there is no value so filter those out and convert the raw data to a different data type if necessary.

(defn to-str [s]
  (when-not (str/blank? s)
    s))

(defn to-id [s]
  (when (and s (not= s "0")) ; nil check just in case.
    s))

(defn to-date [s]
  (when (and (not (str/blank? s)) (not= (date-part s) "1970-01-01")) ; I've never seen a blank date from the lightspeed api, but just in case.
    ;; Lightspeed DateTime has a similar enough format to a clojure instance that edn can parse it (just missing milliseconds).
    (parse-date s)))

(defn to-number [s & {:keys [nonzero-only?]}]
  (when-not (str/blank? s) ; I've Never seen a blank number (usually zero as the default), but just in case.
    (let [n (edn/read-string s)]
      (assert (number? n))
      (when-not (and nonzero-only? (zero? n))
        n))))


(def id-mappings (atom {}))
(def bots (atom []))
(def items-with-stock (atom #{}))

(defn new-id! [type old-id]
  (when (to-id old-id)
    (if-let [existing (get @id-mappings [type old-id])]
      existing
      (let [new-id (gen-id)]
        (assert (not-any? #{new-id} (vals @id-mappings))) ; TODO might not be necessary if I end up doing UUIDs or some variation
        (swap! id-mappings assoc [type old-id] new-id)
        new-id))))

(defn stock-and-bots-for-sku! [o]
  (if-let [item-shops (seqify (get-in o [:ItemShops :ItemShop]))]
    (let [has-stock? (atom false)]
      (doseq [is item-shops]
        (let [qoh (Integer/parseInt (:qoh is))]
          (when (and (some #{(:shopID is)} ["1" "3" "5" "8"])
                     (not (zero? qoh)))
            (swap! bots conj {:shop (new-id! :type/shop (:shopID is))
                              :sku (new-id! :type/sku (:itemID is))
                              :qty qoh})
            (swap! items-with-stock conj (:itemMatrixID o))
            (reset! has-stock? true))))
      @has-stock?)))

(defn migrate-item? [o]
  (or (contains? @items-with-stock (:itemMatrixID o))
      ;; (when-let [d (:timeStamp o)]
      ;;   ...)
      ))



(defn get-use-type [o ks leaf use-type]
  (->> (get-in o ks)
       seqify
       (one #(= use-type (:useType %)))
       (#(get % leaf))))

(defn shop [o]
  {:e/id (new-id! :type/shop (:shopID o))
   :shop/name (:name o)})

(defn price [o type]
  (get-use-type o [:Prices :ItemPrice] :amount type))

(defn inventory [o]
  {:inventory/price (to-number (price o "Default"))
   :inventory/msrp-price (to-number (price o "MSRP") :nonzero-only? true)
   :inventory/online-price (to-number (price o "Online") :nonzero-only? true)
   :inventory/default-cost (to-number (:defaultCost o))
   :inventory/images (vec (->> (get-in o [:Images :Image])
                               seqify
                               (sort-by :ordering)
                               (map (fn [o]
                                      {:image-url (str (:baseImageURL o) (:publicID o))
                                       :hash [(:filename o) (:size o)]}))))})

(defn attributes [o type]
  (when-let [a (get o (case type
                        :colors :attribute1Values
                        :sizes :attribute2Values))]
    (when (or (sequential? a) (and (string? a) (to-str a))) ; Sometimes it's an empty string to mean nothing, a single string meaning one value, or a list of strings.
      (vec (seqify a)))))

(defn item [o]
  (merge
   (rename-keys (inventory o)
                {:inventory/price :item/price
                 :inventory/msrp-price :item/msrp-price
                 :inventory/online-price :item/online-price
                 :inventory/default-cost :item/default-cost
                 :inventory/images :item/images})
   {:e/id (new-id! :type/item (:itemMatrixID o))
    :item/name (when-let [description (:description o)]
                 (let [name (str/replace description "\n" "")] ; Newlines in descriptions somestimes.
                   (if-let [i (str/index-of name ":")]
                     (do
                       ;; (println "Name with colon:" name)
                       (str/trim (subs name (inc i))))
                     name)))
    :item/manufacturer (new-id! :type/manufacturer (:manufacturerID o))
    :item/category (new-id! :type/category (:categoryID o))
    :item/colors (attributes o :colors)
    :item/sizes (attributes o :sizes)
    :item/tags (vec (->> (get-in o [:Tags :tag])
                         seqify
                         (remove #{"webstore" "web"})))
    :item/webstore? (->> (get-in o [:Tags :tag])
                         seqify
                         (some #{"webstore" "web"})
                         boolean)
    :item/short-description (to-str (get-in o [:ItemECommerce :shortDescription]))
    :item/long-description (to-str (get-in o [:ItemECommerce :longDescription]))}))

(defn sku [o]
  (merge
   (rename-keys (inventory o)
                {:inventory/price :sku/price
                 :inventory/msrp-price :sku/msrp-price
                 :inventory/online-price :sku/online-price
                 :inventory/default-cost :sku/default-cost
                 :inventory/images :sku/images})
   {:e/id (new-id! :type/sku (:itemID o))
    :sku/item (new-id! :type/item (:itemMatrixID o))
    :sku/color (or (to-str (get-in o [:ItemAttributes :attribute1])) "Single-color")
    :sku/size (or (to-str (get-in o [:ItemAttributes :attribute2])) "Single-size")
    :sku/upc (to-str (:upc o))
    :sku/ean (to-str (:ean o))
    :sku/code (to-str (:systemSku o))
    :sku/style-number (to-str (:manufacturerSku o))
    :sku/custom-sku-number (to-str (:customSku o))}))

(defn category [o]
  {:e/id (new-id! :type/category (:categoryID o))
   :category/name (:name o)
   :category/parent (new-id! :type/category (:parentID o))})

(defn manufacturer [o]
  {:e/id (new-id! :type/manufacturer (:manufacturerID o))
   :manufacturer/name (:name o)})

(defn address [o leaf]
  (to-str (get-in o [:Contact :Addresses :ContactAddress leaf])))

(defn phone [o use-type]
  (to-str (get-use-type o [:Contact :Phones :ContactPhone] :number use-type)))

(defn email [o use-type]
  (to-str (get-use-type o [:Contact :Emails :ContactEmail] :address use-type)))

(defn customer [o]
  {:e/id (new-id! :type/customer (:customerID o))
   :customer/type (new-id! :type/customer-type (:customerTypeID o))
   :customer/first-name (to-str (:firstName o))
   :customer/last-name (to-str (:lastName o))
   :customer/title (to-str (:title o))
   :customer/company (to-str (:company o))
   :customer/birthday (when-let [dob (:dob o)]
                        (date-part dob))
   :customer/address (address o :address1)
   :customer/address2 (address o :address2)
   :customer/city (address o :city)
   :customer/state (address o :state)
   :customer/zipcode (address o :zip)
   :customer/country (address o :country)
   :customer/home-phone (phone o "Home")
   :customer/work-phone (phone o "Work")
   :customer/mobile-phone (phone o "Mobile")
   :customer/email (email o "Primary")
   :customer/email2 (email o "Secondary")
   :customer/website (to-str (get-in o [:Contact :Websites :ContactWebsite :url]))
   :customer/notes (to-str (get-in o [:Note :note]))})

(defn customer-type [o]
  {:e/id (new-id! :type/customer-type (:customerTypeID o))
   :customer-type/name (to-str (:name o))
   ;; Discount is another kind of object and there are only like ten customer types anyway so don't bother migrating it.
   })

(defn account [o]
  {:account/bot-balance (when-let [balance (to-number (:balance o) :nonzero-only? true)]
                          (- balance))
   :account/customer (new-id! :type/customer (:customerID o))})

(defn credit-account [o]
  (merge
   (rename-keys (account o)
                {:account/bot-balance :credit-account/bot-balance
                 :account/customer :credit-account/customer})
   {:e/id (new-id! :type/credit-account (:creditAccountID o))
    :credit-account/limit (to-number (:creditLimit o))}))

(defn gift-card [o]
  (merge
   (rename-keys (account o)
                {:account/bot-balance :gift-card/bot-balance
                 :account/customer :gift-card/customer})
   {:e/id (new-id! :type/gift-card (:creditAccountID o)) ; I make a distinction between gift cards and credit accounts (and I prefer them separate in the mappings output file) but lightspeed does not. If ever anything references a gift card foreign key I'll need to change this to :credit-account.
    :gift-card/code (:code o)
    ;; From API docs and my previous dumps it doesn't look like the gift card notes come through.
    }))


(defn index-seq [s keyfn]
  (into {} (for [i s]
             [(keyfn i) i])))

(defn no-empties [m]
  (into {} (remove (comp
                    (fn [x]
                      (or (nil? x)
                          (false? x)
                          (and (sequential? x) (empty? x))))
                    val)
                   m)))

(defn process [process-fn o]
  (no-empties (merge (process-fn o)
                     {:e/date (or (to-date (:createTime o))
                                  (to-date (:timeStamp o)))}))) ; Items in particular don't ever have a createTime, others maybe too.

(defn index-bots [all]
  (reduce (fn [aggr {:keys [sku shop] :as row}]
            (update-in aggr
                       [sku shop]
                       (fn [a b] b)
                       (:qty row)))
          {} all))

(defn read-dump [output-to file]
  (safe-read-edn-file (dump-file output-to file)))

(defn migrate-1 [output-to]
  (println "Starting migrate...")
  (let [db (atom nil)]
    (reset!
     db
     (into
      {}
      (doall
       (for [[file doc-type process-fn]
             [["Shop" :type/shop shop]
              ["Manufacturer" :type/manufacturer manufacturer]
              ["Category" :type/category category]
              ["Item" :type/sku sku] ; Do skus before items to get a list of the ones with stock.
              ["ItemMatrix" :type/item item]
              ["Customer" :type/customer customer]
              ["CustomerType" :type/customer-type customer-type]
              ["CreditAccount" :type/credit-account credit-account]
              ["CreditAccount" :type/gift-card gift-card]]]
         (do
           (println (str "Starting " (name doc-type) " migrate."))
           [doc-type
            (doall
             (keep (fn [o]
                     (when (case doc-type
                             :type/sku (stock-and-bots-for-sku! o)
                             :type/item (migrate-item? o)
                             :type/credit-account (and (= (:giftCard o) "false")
                                                       ;; Get rid of credit accounts with no balance or credit limit not only because they're superfluous but there's a bunch of duplicates with these characteristics in lightspeed and my schema requires that :credit-account/customer be unique.
                                                       (not (and (= (:creditLimit o) "0")
                                                                 (= (:balance o) "0"))))
                             :type/gift-card (= (:giftCard o) "true")
                             true)
                       (process process-fn o)))
                   (read-dump output-to file)))])))))
    (println "Creating items for matrix-less skus...")
    (let [indexed-ls-skus (index-seq (read-dump output-to "Item") :itemID)
          new-to-old-ids (map-invert @id-mappings)
          single-sku-items (atom [])
          skus (doall
                (for [sku (:type/sku @db)]
                  (if (:sku/item sku)
                    sku
                    (let [item-id (gen-id)]
                      (swap! single-sku-items conj (assoc (process item
                                                                   (let [old-id (second (get new-to-old-ids (:e/id sku)))]
                                                                     (get indexed-ls-skus old-id)))
                                                          :e/id item-id))
                      (assoc sku :sku/item item-id)))))]
      (swap! db assoc :type/sku skus)
      (swap! db update :type/item concat @single-sku-items))
    (println "Building image hash lookup and normalizing item and sku image properties to their hashes...")
    (let [hash-to-image-url (atom {})]
      (doseq [type [:type/item :type/sku]]
        (swap! db update type
               (fn [prev]
                 (doall
                  (for [inventory prev]
                    (update inventory (case type
                                        :type/item :item/images
                                        :type/sku :sku/images)
                            (fn [images]
                              (vec (doall
                                    (for [{:keys [image-url hash]} images]
                                      (do
                                        (swap! hash-to-image-url assoc hash image-url)
                                        hash)))))))))))
      (println "Writing image hash lookup file...")
      (pretty-write (hashes-and-images-file output-to) @hash-to-image-url))
    (println "Removing sku override attributes that are the same as the parent item...")
    (let [indexed-items (index-seq (:type/item @db) :e/id)]
      (swap! db update :type/sku
             (fn [prev]
               (for [sku prev]
                 (let [item (get indexed-items (:sku/item sku))
                       attrs (keep (fn [[item-attr sku-attr]]
                                     (when (= (get item item-attr) (get sku sku-attr))
                                       sku-attr))
                                   [[:item/price :sku/price]
                                    [:item/msrp-price :sku/msrp-price]
                                    [:item/online-price :sku/online-price]
                                    [:item/default-cost :sku/default-cost]
                                    [:item/images :sku/images]])]
                   (apply dissoc sku attrs))))))
    (println "Removing MSRP and Online prices when they match the Default.")
    (doseq [type [:type/item :type/sku]]
      (swap! db update type
             (fn [prev]
               (for [inventory prev]
                 (let [prices (keep (fn [p]
                                      (when (= (get inventory (case type
                                                                :type/item :item/price
                                                                :type/sku :sku/price))
                                               (get inventory p))
                                        p))
                                    (case type
                                      :type/item [:item/msrp-price :item/online-price]
                                      :type/sku [:sku/msrp-price :sku/online-price]))]
                   (apply dissoc inventory prices))))))
    (let [addendum-item-attrs [:item/webstore?
                               :item/short-description
                               :item/long-description]]
      (let [addendum (->> (:type/item @db)
                          (keep (fn [item]
                                  (let [as (select-keys item addendum-item-attrs)]
                                    (when (seq as)
                                      [(:e/id item) as]))))
                          (into {}))]
        (println "Writing addendum...")
        (pretty-write (addendum-file output-to) addendum))
      (println "Removing non-db migration attributes...")
      (swap! db update :type/item
             (fn [prev]
               (for [item prev]
                 (apply dissoc item addendum-item-attrs)))))
    (println "Writing db_1...")
    (pretty-write (db-1-file output-to) @db))
  (println "Writing bots...")
  (pretty-write (bots-file output-to) (index-bots @bots))
  (println "Writing mappings...")
  (pretty-write (mappings-file output-to) @id-mappings))

(defn upload-image [url]
  (http/request
   {:url "https://www.filestackapi.com/api/store/S3"
    :method :post
    :headers {"Content-Type" "application/x-www-form-urlencoded"}
    :query-params {:key filestack-api-key}
    :body (str "url=" url)}))

(defn process-upload [{:keys [body] :as res} url]
  (if-let [e (or (:error res)
                 (when (str/includes? body "File not found")
                   body)
                 (when (not= (:status res) 200)
                   {:status (:status res)
                    :body body}))]
    (do
      (println (str "Failed to upload image: " url "\nError: " e))
      nil)
    (-> (json/read-str body :key-fn keyword)
        :url
        (str/split #"/")
        last)))

(defn images-to-cloud [output-to]
  (println "Staring images upload...")
  (let [hash-to-handle (atom (or (safe-read-edn-file hashes-and-handles-file) {}))
        upload (apply dissoc (safe-read-edn-file (hashes-and-images-file output-to)) (keys @hash-to-handle))
        batch-size 200]
    (println (str (count @hash-to-handle) " images already uploaded, " (count upload) " new images."))
    (doseq [batch (partition-all batch-size upload)
            :let [futures (doall
                           (map (fn [[hash url]]
                                  [hash url (upload-image url)])
                                batch))]]
      (doseq [[hash url res] futures]
        (when-let [handle (process-upload @res url)]
          (swap! hash-to-handle assoc hash handle)
          (println (str "Hash and handle: " hash " " handle))))
      (println (str "Done with batch of " batch-size))
      (newline)
      (pretty-write hashes-and-handles-file @hash-to-handle))))

(defn migrate-2 [output-to]
  (let [hash-to-handle (safe-read-edn-file hashes-and-handles-file)
        db (atom (safe-read-edn-file (db-1-file output-to)))]
    (doseq [type [:type/item :type/sku]]
      (swap! db update type
             (fn [prev]
               (for [inventory prev]
                 (no-empties (update inventory (case type
                                                 :type/item :item/images
                                                 :type/sku :sku/images)
                                     (fn [hashes]
                                       (vec (keep #(get hash-to-handle %) hashes)))))))))
    (println "Writing db_2...")
    (pretty-write (db-file output-to) @db)
    #_(as-> @db $
        (mapcat (fn [[type docs]]
                  (map #(assoc % :e/type type) docs)) $) ; TODO probably getting rid of :e/type, then I can just do the commented-out version below
        (if (every? spec/valid-entity? $)
          (println "DB passed validation!")
          (println "Invalid DB!")))
    #_(if (every? spec/valid-entity? (->> @db vals (apply concat)))
        (println "DB passed validation!")
        (println "Invalid DB!"))))
