(ns hyperdrive.singularity.events
  (:require [clojure.string :as str]
            [clojure.set :refer [rename-keys]]
            [cljs.reader :refer [read-string]]
            [datascript.core :as d]
            [re-frame.core :refer [reg-event-db reg-event-fx inject-cofx path trim-v]]
            [re-frame.interceptor :refer [->interceptor get-effect get-coeffect assoc-coeffect assoc-effect]]
            [re-frame.utils :refer [dissoc-in]]
            [datascript.transit :as dt]
            [testdouble.cljs.csv :as csv]
            [hyperdrive.db :as db]
            [hyperdrive.spec :as spec]
            [hyperdrive.queries :as q]
            [hyperdrive.util :as u :refer [fetch fmap no-nils currency humanize new-entity]]
            [hyperdrive.singularity.emails :as emails]
            [hyperdrive.singularity.models :as m]
            [hyperdrive.singularity.routes :as routes]
            [hyperdrive.singularity.realtime :as realtime]
            [hyperdrive.singularity.webusb :as webusb]
            hyperdrive.singularity.cofx
            hyperdrive.singularity.fx))


;;;; "Session"

(reg-event-db
 :set-current-register
 (fn [db [_ register]]
   (assoc db :current-register register)))

(reg-event-db
 :unset-current-register
 (fn [db]
   (dissoc db :current-register)))


;;; Modal

(reg-event-fx
 :modal/show
 (fn [{db :db} [_ name & args]]
   {:db (assoc db :modal/name name :modal/args args)
    :modal "show"}))

(reg-event-fx
 :modal/close
 (fn [{db :db}]
   {:modal "hide"}))

(reg-event-db
 :modal/cleanup
 (fn [db]
   (dissoc db :modal/name :modal/args)))



;;;; Models

(defn change-effect [id type k v]
  {:dispatch [:store/save* id type k v]})

(doseq [type [:item :category :manufacturer :transfer :count :sale :refund :sku :order :shop :register :register-adjustment :payment-type :customer :customer-type :credit-account :employee :timesheet]]
  (reg-event-fx
   (keyword type :change)
   (fn [_ [_ id k v]]
     (change-effect id (keyword :type type) k v))))

;; Use this instead of the type-specific :change event above from now on.
(reg-event-fx
 :change
 (fn [_ [_ id type k v]]
   (change-effect id type k v)))


;; Register handlers for simple create doc events all at once.
(doseq [type [:category :manufacturer :customer-type :payment-type :shop]]
  (reg-event-fx
   (keyword type :create)
   (fn [cofx]
     {:dispatch [:store/create (keyword :type type)]})))

;; This could take an id too if I ever have more than a single new entity of a single type at a time like I might do for held receipts. Update: I'm putting held receipts in the app-db for simplicity.
(reg-event-fx 
 :clear
 (inject-cofx :new-conn)
 (fn [{:keys [new-conn]} [_ type]]
   {:db/transact-new [(when-let [eid (d/q q/new-id-query new-conn type)]
                        [:db/retractEntity [:e/id eid]])
                      (update (new-entity type) :e/id (fn [eid]
                                                        [:id/new eid]))]}))



(reg-event-fx
 :register/create
 (fn [_ [_ shop]]
   {:dispatch [:store/create :type/register :additional {:register/shop shop}]}))

(reg-event-fx
 :register-count/change-amount
 (fn [_ [_ id payment-type amount]]
   {:dispatch [:store/update* id :type/register-count :register-count/amounts #(assoc % payment-type amount)]}))

(reg-event-fx
 :register-adjustment/create
 (inject-cofx :id)
 (fn [{:keys [db id]} [_ type register]]
   {:dispatch-n (list [:store/create
                       :type/register-adjustment
                       :id id
                       :additional {:register-adjustment/type type
                                    :register-adjustment/register register
                                    :register-adjustment/employee (:user db)}]
                      [:navigate :page/register-adjustment-complete id])}))


;;; Shared item/sku events

(doseq [model [:item :sku]]
  (reg-event-fx
   (keyword model :upload-images)
   (fn [_ [_ id]]
     {:filestack/pick [(keyword model :add-images) id]
      :dispatch [:set-filepicker-working? true]}))
  
  (reg-event-fx
   (keyword model :add-images)
   (fn [_ [_ id urls]]
     {:dispatch [:store/update* id (keyword :type model) (keyword model :images) #(vec (concat % urls))]}))

  (reg-event-fx
   (keyword model :remove-image)
   (fn [_ [_ id url]]
     {:dispatch [:store/update* id (keyword :type model) (keyword model :images) #(vec (remove #{url} %))]}))

  (reg-event-fx
   (keyword model :set-primary-image)
   (fn [_ [_ id url]]
     {:dispatch [:store/update* id (keyword :type model) (keyword model :images) #(->> (remove #{url} %)
                                                                                       (cons url)
                                                                                       vec)]})))

(reg-event-fx
 :item/create
 (inject-cofx :id)
 (fn [{:keys [db id]} [_ create-all? on-create]]
   {:dispatch-n (concat (list [:store/create :type/item
                               :id id
                               :additional {:item/employee (:user db)}]
                              (conj on-create id))
                        (when create-all?
                          (let [item (fetch [:item/new])]
                            (for [color (:colors item)
                                  size (:sizes item)]
                              [:sku/create id color size])))
                        (list (if create-all?
                                [:modal/show :upcs id]
                                [:modal/close])))}))

(reg-event-fx
 :item/duplicate
 [(inject-cofx :conn) (inject-cofx :id)]
 (fn [{:keys [conn id]} [_ from-id]]
   (let [from (d/pull conn '[:item/name
                             :item/manufacturer
                             :item/category
                             :item/price
                             :item/msrp-price
                             :item/online-price
                             :item/wholesale-price
                             :item/default-cost
                             :item/colors
                             :item/sizes
                             :item/notes]
                      [:e/id from-id])]
     {:dispatch-n (list [:store/create
                         :type/item
                         :id id
                         :doc (update from :item/name #(str "Copy of " %))]
                        [:notify {:text "Item duplicated."
                                  :type :info
                                  :delay 2000}]
                        [:navigate :page/item id])})))

(reg-event-fx
 :sku/create
 (inject-cofx :code)
 (fn [{:keys [code]} [_ item color size]]
   {:dispatch [:store/create :type/sku :doc {:sku/item item
                                             :sku/code code
                                             :sku/color color
                                             :sku/size size}]}))

(reg-event-fx
 :sku/apply-to-all
 (fn [_ [_ id color-or-size attribute]]
   (when (js/confirm "Are you sure?")
     (let [sku (fetch [:sku id])
           skus (->> (fetch [:item/skus (:item sku)])
                     (u/find {color-or-size (get sku color-or-size)})
                     (map :id))]
       {:dispatch-n (for [other-sku skus]
                      [:sku/change other-sku attribute (get sku attribute)])}))))

(defn stock-management-add [id type items-attr item]
  [:store/update* id type items-attr (fn [items]
                                       (->> (or items [])
                                            (remove #{item})
                                            (cons item)
                                            vec))])

(defn stock-management-remove-item [id type items-attr item]
  [:store/update* id type items-attr #(vec (remove #{item} %))])

(defn stock-management-remove [id type items-attr skus-attr item & {:keys [additional]}]
  {:dispatch-n (list [:store/update* id type items-attr #(vec (remove #{item} %))]
                     (let [sku-ids (q/item:skus item)]
                       [:store/update* id type skus-attr #(apply dissoc % sku-ids)])
                     additional)})

(defn stock-management-add-sku [id type skus-attr add-event sku]
  {:dispatch-n (list [add-event id (:item (fetch [:sku sku]))]
                      [:store/update* id type skus-attr #(update % sku (fnil inc 0))])})

(reg-event-fx
 :order/add
 (fn [_ [_ id item]]
   {:dispatch-n (list (stock-management-add id :type/order :order/items item)
                      (when-not (fetch [:order/unit-cost id item])
                        [:order/change-unit-cost id item (q/fact item :item/default-cost)]))}))

(reg-event-fx
 :order/remove
 (fn [_ [_ id item]]
   (stock-management-remove id :type/order :order/items :order/skus item
                            :additional [:store/update* id :type/order :order/costs #(dissoc % item)])))

(reg-event-fx
 :order/change-sku-qty
 (fn [_ [_ id sku qty]]
   {:dispatch [:store/update* id :type/order :order/skus #(assoc % sku qty)]}))

(reg-event-fx
 :order/change-received-qty
 (fn [_ [_ id sku qty]]
   {:dispatch [:store/update* id :type/order :order/received-skus #(assoc % sku qty)]}))

(reg-event-fx
 :order/change-unit-cost
 (fn [_ [_ id item cost]]
   {:dispatch [:store/update* id :type/order :order/costs #(assoc % item cost)]}))

(reg-event-fx
 :order/place-order
 (inject-cofx :id)
 (fn [{:keys [db id]}]
   {:dispatch-n (list [:store/create
                       :type/order
                       :id id
                       :additional {:order/status :open
                                    :order/employee (:user db)}]
                      [:navigate :page/order id])}))

(reg-event-fx
 :order/receive-all
 (fn [_ [_ id]]
   {:dispatch [:store/save* id :type/order :order/received-skus (:skus (fetch [:order id]))]})) ; TODO ick direct extraction

(reg-event-fx
 :order/mark-finished
 (fn [_ [_ id finished?]]
   {:dispatch-n (list [:store/save* id :type/order :order/status (if finished?
                                                                   :finished
                                                                   :open)]
                      [:store/save* id :type/order :order/received-on (if finished?
                                                                        (u/now)
                                                                        nil)])}))

(reg-event-fx
 :order/add-sku
 (fn [_ [_ id sku]]
   (stock-management-add-sku id :type/order :order/skus :order/add sku)))

(reg-event-fx
 :transfer/add
 (fn [_ [_ id item]]
   {:dispatch (stock-management-add id :type/transfer :transfer/items item)}))

(reg-event-fx
 :transfer/remove
 (fn [_ [_ id item]]
   (stock-management-remove id :type/transfer :transfer/items :transfer/skus item)))

(reg-event-fx
 :transfer/change-sku-qty
 (fn [_ [_ id sku qty]]
   {:dispatch [:store/update* id :type/transfer :transfer/skus #(assoc % sku qty)]}))

(reg-event-fx
 :transfer/change-received-qty
 (fn [_ [_ id sku qty]]
   {:dispatch [:store/update* id :type/transfer :transfer/received-skus #(assoc % sku qty)]}))

(reg-event-fx
 :transfer/initiate
 (inject-cofx :id)
 (fn [{:keys [db id]}]
   {:dispatch-n (list [:store/create
                       :type/transfer
                       :id id
                       :additional {:transfer/status :sent
                                    :transfer/employee (:user db)}]
                      [:navigate :page/transfer id])}))

(reg-event-fx
 :transfer/receive-all
 (fn [_ [_ id]]
   {:dispatch [:store/save* id :type/transfer :transfer/received-skus (:skus (fetch [:transfer id]))]})) ; TODO ick direct extraction

(reg-event-fx
 :transfer/mark-finished
 (fn [_ [_ id finished?]]
   {:dispatch-n (list [:store/save* id :type/transfer :transfer/status (if finished?
                                                                         :received
                                                                         :sent)]
                      [:store/save* id :type/transfer :transfer/received-on (if finished?
                                                                              (u/now)
                                                                              nil)])}))

(reg-event-fx
 :transfer/add-sku
 (fn [_ [_ id sku]]
   (stock-management-add-sku id :type/transfer :transfer/skus :transfer/add sku)))

(reg-event-fx
 :count/add
 (fn [_ [_ id item]]
   {:dispatch (stock-management-add id :type/count :count/items item)}))

(reg-event-fx
 :count/remove
 (fn [_ [_ id item]]
   (stock-management-remove id :type/count :count/items :count/counted item)))

;; TODO Assumes a new count (id :new and :count/new), ideally clean this up when rewriting `fetch` uses though currently this is in fact only used for new counts.
(reg-event-fx
 :count/set-expected
 (fn []
   (let [id :new
         count (fetch [:count/new])
         skus (if (m/count:manual-adjustment? count)
                (keys (:counted count))
                (mapcat #(q/item:skus (:id %)) (fetch [:count/against-items id])))
         expected (zipmap skus (map #(db/stock % (:shop count)) skus))]
     {:dispatch [:store/save* id :type/count :count/expected expected]})))

;; TODO dispatch-later here and in :count/set-category and set-manufacturer are hacks. Don't have :count/set-expected be aseparate event then.
(reg-event-fx
 :count/change-sku-qty
 (fn [_ [_ id sku qty]]
   {:dispatch-n (list [:store/update* id :type/count :count/counted #(assoc % sku qty)]
                      ;; (set-expected)
                      ;; TODO Hacky way of making sure item gets added when the sku quantity gets changed from the summary pane. Mostly copypasta from stock-management-add.
                      [:store/update* id :type/count :count/items (fn [items]
                                                                    (let [item (q/fact sku :sku/item)]
                                                                      (if (some #{item} items)
                                                                        items
                                                                        (->> (or items [])
                                                                             (remove #{item})
                                                                             (cons item)
                                                                             vec))))])
    :dispatch-later [{:dispatch [:count/set-expected]
                      :ms 500}]}))

(doseq [[event-id attribute] [[:count/set-shop :count/shop]
                              [:count/set-category :count/category]
                              [:count/set-manufacturer :count/manufacturer]]]
  (reg-event-fx
   event-id
   (fn [_ [_ id v]]
     {:dispatch-n (list [:store/save* id :type/count attribute v]
                        ;; (set-expected)
                        )
      :dispatch-later [{:dispatch [:count/set-expected]
                        :ms 500}]})))

(reg-event-fx
 :count/reconcile
 (inject-cofx :id)
 (fn [{:keys [db id]}]
   {:dispatch-n (list [:store/create
                       :type/count
                       :id id
                       :additional {:count/employee (:user db)}]
                      [:navigate :page/count id])}))

(reg-event-fx
 :count/add-sku
 (fn [_ [_ id sku]]
   (stock-management-add-sku id :type/count :count/counted :count/add sku)))

(reg-event-fx
 :customer/create
 (inject-cofx :id)
 (fn [{:keys [db id]} [_ on-create]]
   {:dispatch-n (list [:store/create :type/customer
                       :id id
                       :additional {:customer/employee (:user db)}]
                      (when on-create
                        (conj on-create id)))}))

#_(doseq [attr [:customer/first-name
                :customer/last-name]]
    (reg-event-entity :type/customer attr))

(reg-event-fx
 :credit-account/create
 (fn [{:keys [db]} [_ customer]]
   {:dispatch [:store/create :type/credit-account :doc {:credit-account/customer customer
                                                        :credit-account/limit 0
                                                        :credit-account/employee (:user db)}]}))

(reg-event-fx
 :timesheet/create
 (fn [_ [_ employee]]
   {:dispatch [:store/create :type/timesheet :additional {:timesheet/employee employee}]}))









(defn amount-no-more-than-available [source amount]
  (min amount
       (case (q/fact source :e/type)
         :type/credit-account (fetch [:credit-account/available source])
         ;; :type/gift-card nil
         amount)))

(reg-event-db
 :payment/set
 (fn [db [_ id source raw-amount]]
   (if (nil? raw-amount)
     (dissoc-in db [:new-payments id source])
     (let [amount (u/parse-number raw-amount)]
       (if (and (number? amount) (pos? amount))
         (assoc-in db [:new-payments id source] (amount-no-more-than-available source amount))
         db)))))

(defn remaining-to-fill [id fill-total]
  (max 0 (fetch [:payment/balance id fill-total]))) ; "max 0" might not be necessary here, see usages

(reg-event-db
 :payment/max
 (fn [db [_ id source fill-total]]
   (let [remaining (remaining-to-fill id fill-total)]
     (if (pos? remaining)
       (let [amount (+ (or (get-in db [:new-payments id source]) 0)
                       remaining)]
         (assoc-in db [:new-payments id source] (amount-no-more-than-available source amount)))
       db))))

(reg-event-db
 :payment/add
 (fn [db [_ id source amount]]
   (update-in db [:new-payments id source] (fnil + 0) amount)))

(reg-event-fx
 :payment/use-gift-card-next
 (fn [_ [_ sale-id code fill-total]]
   (if-let [gift-card-id (first (q/find-gift-cards-by-code (str/trim code)))] ; Trim just and to be consistent with gift card activation/recharging logic.
     {:dispatch [:modal/show :use-gift-card-2
                 sale-id
                 gift-card-id
                 (min (remaining-to-fill sale-id fill-total)
                      (fetch [:gift-card/balance gift-card-id]))
                 fill-total]}
     {:dispatch [:notify {:text "No gift card with that code."
                          :delay 2000}]})))









(defn- make-sale-line [db sku price]
  {:id (u/gen-id)
   :line-type :sku
   :sku sku
   :price price
   :employee (:user db)})

(reg-event-fx
 :sale/add
 (fn [{db :db} [_ id sku]]
   {:dispatch-n (list (stock-management-add id :type/sale :sale/items (q/fact sku :sku/item))
                      [:store/update* id :type/sale :sale/lines (fn [lines]
                                                                  (->> (or lines [])
                                                                       (cons (make-sale-line db sku (q/sku:price sku)))
                                                                       vec))]
                      [:customer-views/send :add sku])}))

(reg-event-fx
 :sale/remove
 (fn [_ [_ id index]]
   {:dispatch [:store/update* id :type/sale :sale/lines #(u/drop-nth-v index %)]}))

(reg-event-fx
 :sale/change-line-price
 (fn [_ [_ id index price]]
   {:dispatch [:store/update* id :type/sale :sale/lines #(assoc-in % [index :price] price)]}))

(reg-event-fx
 :sale/change-line-employee
 (fn [_ [_ sale-id other-id employee]]
   {:dispatch [:store/update* sale-id :type/sale :sale/lines (fn [xs]
                                                               (let [x (first (filter #(= (:id %) other-id) xs))
                                                                     index (.indexOf xs x)]
                                                                 (update xs index assoc :employee employee)))]}))

(reg-event-fx
 :sale/change-line-employee*
 (fn [_ [_ id index employee]]
   {:dispatch [:store/update* id :type/sale :sale/lines #(assoc-in % [index :employee] employee)]}))

(reg-event-fx
 :sale/set-line-discount
 (fn [_ [_ id index raw-discount]]
   (when-let [discount (some-> (or raw-discount 0)
                               u/parse-number
                               int
                               (-> (max 0) (min 100)))]
     {:dispatch [:store/update* id :type/sale :sale/lines #(update % index (fn [line]
                                                                             (let [price (fetch [:sku/price (:sku line) :inherit? true])]
                                                                               (assoc line :price (- price (* price (/ discount 100)))))))]})))

(reg-event-fx
 :sale/add-gift-card-charge
 (fn [{db :db} [_ id amount]]
   {:dispatch-n (list (let [new-gift-card (fetch [:gift-card/new])]
                        [:store/update* id :type/sale :sale/lines (fn [lines]
                                                                    (->> (or lines [])
                                                                         (cons {:id (u/gen-id)
                                                                                :line-type :gift-card
                                                                                :code (str/trim (:code new-gift-card))
                                                                                :price amount
                                                                                :employee (:user db)})
                                                                         vec))])
                      [:clear :type/gift-card])}))

(reg-event-fx
 :sale/add-credit-account-deposit
 (fn [{db :db} [_ sale-id credit-account-id]]
   {:dispatch [:store/update* sale-id :type/sale :sale/lines (fn [lines]
                                                               (->> (or lines [])
                                                                    (cons {:id (u/gen-id)
                                                                           :line-type :credit-account
                                                                           :credit-account credit-account-id
                                                                           :price 0
                                                                           :employee (:user db)})
                                                                    vec))]}))

(reg-event-fx
 :sale/convert-or-unconvert-to-layaway
 (fn [_ [_ id convert-or-unconvert]]
   {:dispatch [:store/save* id :type/sale :sale/layaway? (or convert-or-unconvert nil)]})) ; I like removing the attribute a bit better than saving it as false. Doens't matter for anything.

(defn new-payments-to-transactions [db sale-id]
  (seq (for [[source amount] (get-in db [:new-payments sale-id])]
         (merge (new-entity :type/transaction) ; HACK: Impure.
                {:transaction/line-type (q/fact source :e/type)
                 :transaction/source source
                 :transaction/amount amount
                 :transaction/register (:current-register db)
                 :transaction/employee (:user db)}))))

(defn clear-new-payments [db sale-id]
  (dissoc-in db [:new-payments sale-id]))

;; TODO don't create the refund if it's bogus (doesn't have any lines in it / has a :refund/amount of zero / whatever) because the cashier started a refund but didn't use it (make a :new.sale/refund attribute that here gets converted into :sale/refund to make the fact that having a refund set on the new sale is just work-in-progress stuff but has to be determined later [here] explicit). Or just don't let a sale with a refund be completed (make it invalid) if the refund isn't being used, that'd probably be easier than doing all that logic (I could even add a quick "Cancel refund" button to make this easier for the user). Update: my current logic for calculating sale-payments/refund-payments and using refund-payments' existence to determine if a refund gets made WILL prevent a refund from being created but not the sale from still having a :sale/refund attribute with the in-progress new id on it which may(?) cause validation to fail.
(reg-event-fx
 :sale/finish
 [(inject-cofx :id) (inject-cofx :code)]
 (fn [{:keys [db id code]}]
   (let [new-sale (fetch [:sale/new])
         [sale-payments refund-payments] (let [exchange-amount (+ (fetch [:refund/amount (:refund new-sale)])
                                                                  (if (fetch [:sale-is-refund-with-refundable-money (:id new-sale)])
                                                                    (fetch [:sale/total (:id new-sale)])
                                                                    0))
                                               naive-payments (let [payments (vec (new-payments-to-transactions db (:id new-sale)))]
                                                                (if (zero? exchange-amount)
                                                                  [payments nil]
                                                                  (let [exchange (merge (new-entity :type/transaction) ; HACK: Impure.
                                                                                        {:transaction/line-type :type/exchange
                                                                                         :transaction/amount exchange-amount
                                                                                         :transaction/register (:current-register db)
                                                                                         :transaction/employee (:user db)})]
                                                                    [(vec (conj payments exchange)) [exchange]])))]
                                           (if (fetch [:sale-is-refund-with-refundable-money (:id new-sale)])
                                             (vec (reverse naive-payments))
                                             naive-payments))
         refund-id (u/gen-id)] ; Ick.
     {:db (clear-new-payments db (:id new-sale))
      :dispatch-n (concat
                   (for [{:keys [code]}
                         (filter (fn [line]
                                   (and (= (:line-type line) :gift-card)
                                        (not (first (q/find-gift-cards-by-code (:code line))))))
                                 (:lines new-sale))]
                     [:store/create
                      :type/gift-card
                      :id (u/gen-id) ; HACK I don't feel like injecting an arbitrary number of new ids with coeffects.
                      :doc (merge {:gift-card/code code
                                   :gift-card/employee (:user db)}
                                  (if-let [customer (:customer new-sale)]
                                    {:gift-card/customer customer}))])
                   (list (when refund-payments
                           ;; TODO should be dissoc'ing my temporary :refund/sale
                           [:store/create
                            :type/refund
                            :id refund-id
                            :additional {:refund/transactions refund-payments
                                         :refund/register (:current-register db)
                                         :refund/employee (:user db)}])
                         [:customer-views/send :thanks]
                         [:store/create
                          :type/sale
                          :id id
                          :additional (merge {:sale/sale-type :sale.type/in-store
                                              :sale/code code
                                              :sale/register (:current-register db)
                                              :sale/employee (:user db)}
                                             (when sale-payments
                                               {:sale/transactions sale-payments})
                                             (when (q/is-sale-a-refund? new-sale)
                                               {:sale/refund refund-id}))]
                         [:navigate :page/sale-complete id]))})))

(reg-event-fx
 :sale/change-payment
 (fn [_ [_ sale-id payment-id new-payment-type-id]]
   {:dispatch [:store/update* sale-id :type/sale :sale/transactions (fn [payments]
                                                                      (let [payment (first (filter #(= (:e/id %) payment-id) payments))
                                                                            index (.indexOf payments payment)]
                                                                        (update payments index assoc :transaction/source new-payment-type-id)))]}))

(reg-event-fx
 :sale.new/set-customer
 (fn [_ [_ id customer]] ; TODO passing `id` no longer necessary right now (but maybe keep it for when I eventually have multiple new sales / held receipts)
   {:dispatch-n (list [:store/save* id :type/sale :sale/customer customer]
                      (let [discount (q/customer:discount customer)]
                        (when-not (zero? discount)
                          [:store/save* id :type/sale :sale/discount discount]))
                      (when-not customer
                        [:sale/convert-or-unconvert-to-layaway id false]))}))

(reg-event-fx
 :new-sale/transfer-stock
 (fn [_ [_ item sku shop]]
   {:dispatch-n (list [:transfer/add-sku :new sku]
                      [:transfer/change :new :to shop])}))

(reg-event-fx
 :new-sale.bulk/toggle-bulk-view
 (fn [_ [_ id bulk?]]
   (let [lossy? (fn []
                  (seq (:lines (fetch [:sale/new]))))
         proceed #(js/confirm "Converting to a bulk sale will set every item to it's wholesale price and remove any non-item lines (like gift cards). Proceed?")
         change-event [:sale/change :new :bulk? bulk?]
         convert-event [:store/update* id :type/sale :sale/lines
                        (fn [lines]
                          (->> lines
                               (remove #(not= (:line-type %) :sku))
                               (map (fn [{:keys [sku] :as line}]
                                      ;; Might be best to change the :employee to the current one since the custom line employees become hidden and this might be deceptive but whatever.
                                      (assoc line :price (q/sku:wholesale-price sku)))
                                    lines)
                               vec))]]
     (if (and bulk? (lossy?))
       (when (proceed)
         {:dispatch-n (list change-event
                            convert-event)})
       {:dispatch change-event}))))

(reg-event-fx
 :sale.bulk/add
 (fn [_ [_ id item]]
   {:dispatch (stock-management-add id :type/sale :sale/items item)}))

(defn remove-skus-sale-lines [skus lines]
  (vec (remove #(some #{(:sku %)} skus) lines)))

(reg-event-fx
 :sale.bulk/remove
 (fn [_ [_ id item]]
   {:dispatch-n (list (stock-management-remove-item id :type/sale :sale/items item)
                      [:store/update* id :type/sale :sale/lines (fn [lines]
                                                                  (remove-skus-sale-lines (q/item:skus item) lines))])}))

(reg-event-fx
 :sale.bulk/change-sku-qty
 (fn [{db :db} [_ id sku raw-qty]]
   (let [qty (or (u/parse-int raw-qty) 0)]
     {:dispatch [:store/update* id :type/sale :sale/lines (fn [lines]
                                                            (->> lines
                                                                 (remove-skus-sale-lines [sku])
                                                                 (concat (repeat qty (make-sale-line db sku (q/sku:wholesale-price sku))))
                                                                 vec))]})))

(reg-event-fx
 :sale.layaway/take-payment
 (fn [{:keys [db]} [_ id]]
   (if (:current-register db)
     {:db (clear-new-payments db id)
      :dispatch-n (list [:store/update* id :sale/transactions (fn [payments]
                                                                (->> payments
                                                                     (concat (new-payments-to-transactions db id))
                                                                     vec))]
                        ;; TODO provide a way to complete layaways or do it automatically, just not like this (:new.sale/sufficient-payment here is confusing, generify the logic)
                        (when (fetch [:new.sale/sufficient-payment id])
                          [:store/save* id :type/sale :sale/layaway? nil]))}
     {:dispatch [:alert "You must be signed into a shop to take payments."]})))

(reg-event-fx
 :refund/add-sale-line
 (fn [_ [_ id sale-line]]
   {:dispatch-n (list [:store/update* id :type/refund :refund/sale-lines (fn [lines]
                                                                           (vec (conj (or lines []) sale-line)))])}))

(defn remove-one [pred coll]
  ((fn inner [coll]
     (lazy-seq
      (when-let [[x & xs] (seq coll)]
        (if (pred x)
          xs
          (cons x (inner xs))))))
   coll))
(def remove-one-v (comp vec remove-one))

(reg-event-fx
 :refund/remove-sale-line
 (fn [_ [_ id sale-line]]
   {:dispatch [:store/update* id :type/refund :refund/sale-lines #(remove-one-v #{sale-line} %)]}))



;;;; Reports

(reg-event-db
 :grouped-report/set-by
 (fn [db [_ report by]]
   (assoc-in db [:reports report] by)))



;;;; Table

;;; Example schema
#_{:tables
   {[:things] {:params {:bob "whatever"}
               :page 1
               :sort [identity :asc]
               :search "snapb hat"
               :filters {:manufacturer {:value "9"
                                        :fn #(= (:manufacturer %) "9")}}
               :extra-shown? true}}}

;; TODO get rid of this and just have tables be namespaced? Update example schema then. Except that table IDs are typically subscription vectors, so maybe just [:table [:whatever]] as db keys? Also how would I do the `path` interception I have built into this? I guess just another path middleware to the immediate right of it
;; TODO Copypasta, remove or refactor this eventually
;; Note: no flattening the `ks` argument, one will be a query-v which is a vector
;; Is there a way to get the context outside of :before/:after and wrap this?
(let [table-ic
      (fn [path]
        (let [db-store-key :re-frame-path/db-store]
          (->interceptor
           :id :table-path
           :before (fn [context]
                     (let [original-db (get-coeffect context :db)]
                       (-> context
                           (update db-store-key conj original-db)
                           (assoc-coeffect :db (get-in original-db (path context))))))
           :after (fn [context]
                    (let [db-store (db-store-key context)
                          original-db (peek db-store)
                          new-db-store (pop db-store)
                          context' (-> (assoc context db-store-key new-db-store)
                                       (assoc-coeffect :db original-db))
                          db (get-effect context :db ::not-found)]
                      (if (= db ::not-found)
                        context'
                        (->> (assoc-in original-db (path context) db)
                             (assoc-effect context' :db))))))))]
  (defn table-path [& ks]
    (table-ic (fn [context]
                (let [table-id (second (get-coeffect context :event))]
                  (concat [:tables table-id] ks))))))

(reg-event-db
 :table/set-page
 (table-path :page)
 (fn [_ [_ _ page]]
   page))

(reg-event-db
 :table/reset-page
 (table-path)
 (fn [table]
   (dissoc table :page)))

(reg-event-fx
 :table/set-sort
 [(table-path :sort) trim-v]
 (fn [_ [id sorting]]
   {:db sorting
    :dispatch [:table/reset-page id]}))

(reg-event-fx
 :table/set-search
 [(table-path) trim-v]
 (fn [{table :db} [id value]]
   {:db (if (str/blank? value)
          (dissoc table :search)
          (assoc table :search value))
    :dispatch [:table/reset-page id]}))

(reg-event-db
 :table/toggle-extra-shown?
 (table-path :extra-shown?)
 (fn [extra-shown?]
   (not extra-shown?)))

(reg-event-fx
 :table/set-param
 [(table-path :params) trim-v]
 (fn [{params :db} [id key value]]
   {:db (assoc params key value)
    :dispatch [:table/reset-page id]}))

(reg-event-fx
 :table/remove-param
 [(table-path) trim-v]
 (fn [{table :db} [id key]]
   {:db (dissoc-in table [:params key])
    :dispatch [:table/reset-page id]}))

(reg-event-fx
 :table/set-filter
 [(table-path :filters) trim-v]
 (fn [{filters :db} [id filter-fn value & {:keys [wrap?]}]]
   ;; Only reset the page if this event would change something. Technically this won't work if `wrap?` changes but that never happens.
   (let [old-filter-fn (get filters filter-fn)
         old-value (:value old-filter-fn)]
     (when-not (and (= filter-fn old-filter-fn) (= value old-value))
       {:db (assoc filters filter-fn {:value value
                                      :fn (if wrap?
                                            #(= (filter-fn %) value)
                                            #(filter-fn % value))})
        :dispatch [:table/reset-page id]}))))

(reg-event-fx
 :table/remove-filter
 [(table-path) trim-v]
 (fn [{table :db} [id filter-fn]]
   (when (get (:filters table) filter-fn)
     {:db (dissoc-in table [:filters filter-fn])
      :dispatch [:table/reset-page id]})))

(reg-event-fx
 :table/set-limit
 [(table-path :limit) trim-v]
 (fn [{limit :db} [id]]
   (let [raw-n (js/prompt "How many results per page (limit 100)?")]
     (when-let [n (u/parse-int raw-n)]
       {:db (-> n (max 1) (min 100))
        :dispatch [:table/reset-page id]}))))

(reg-event-db
 :table/clear
 (table-path)
 (fn [table]
   (dissoc table :params :page :sort :search :filters)))



;;;; Choose Shop page and flow

(reg-event-fx
 :choose-shop/register-chosen
 (inject-cofx :conn)
 (fn [cofx [_ register]]
   {:dispatch (if (d/q q/active-register:open? (:conn cofx) register)
                [:choose-shop/continue register]
                [:modal/show :open-register register])}))

(reg-event-db
 :choose-shop/set-continue
 (fn [db [_ continue-to]]
   (assoc db :choose-shop-continue-to continue-to)))

(reg-event-fx
 :choose-shop/continue
 (fn [{db :db} [_ register]]
   {:db (dissoc db :choose-shop-continue-to)
    :dispatch-n (list [:set-current-register register]
                      (if-let [continue-to (:choose-shop-continue-to db)]
                        [:navigate-path continue-to]
                        [:navigate :page/sales-landing]))}))



;;;; Barcode Scanner

;;; - If you're trying to use the scanner to enter the barcode as text in some field: scanners often put a \r at the end, this isn't handled in any way and might submit forms and whatnot. If you're using the scanner for one of the navigation funtctions: it seems easy to have your cursor on a text field and have the barcode get entered into it before navigation (you have to know to unfocus inputs).

(reg-event-fx
 :scanner/keystroke
 (fn [{db :db} [_ char-code]]
   {:db (update db :scanner/chars (fnil conj []) (js/String.fromCharCode char-code))
    :dispatch-later (when-not (:scanner/chars db)
                      [{:dispatch [:scanner/collated]
                        :ms 300}])})) ; On my testing scanner it took 74ms to read in 13 characters so this should be plenty of time. Keep it tight so quick scans in succession work.

(defn on-barcode [db barcode]
  ;; I don't know if the generated barcodes from lightspeed are unique across sales and skus and there's no guarantee at all that they're unique across gift cards. This only affects the navigate-to-individual-thing part so whatever. ; TODO just before launch: is this comment still applicable?
  (let [page (q/page:name db)
        modal (:modal/name db)]
    (if (= modal :upcs)
      {:tab nil}
      (when-not (some #{js/document.activeElement.tagName} ["INPUT" "TEXTAREA"]) ; This should be a coeffect.
        (let [skus (q/find-skus-by-code barcode)
              sales (q/find-sales-by-code barcode)
              gift-cards (q/find-gift-cards-by-code barcode)
              multiple? (fn [xs]
                          (< 1 (count xs)))
              notify-no (fn notify-no
                          ([] (notify-no nil))
                          ([type]
                           {:dispatch [:notify {:text (str (if type (str "No " type) "Nothing") " found for scan " barcode) :type :notice}]}))
              notify-multiple (fn notify-multiple
                                ([] (notify-multiple nil))
                                ([type]
                                 {:dispatch [:notify {:text (str "Multiple " (or type "things") " found for scan " barcode) :type :notice}]}))]
          (if (contains? #{:page/new-sale :page/new-order :page/new-transfer :page/new-count} page)
            (cond
              (not skus) (notify-no "sku")
              (multiple? skus) (notify-multiple "skus")
              :else (let [sku (first skus)
                          confirmation-event (fn [qty-sub-query]
                                               [:notify {:text (str "<em>" (fetch [:sku/name sku :full? true]) "</em>\n"
                                                                    (when qty-sub-query
                                                                      ;; The add-sku events dispatch other events and end up completing after the dispatch of the confirmation event, so just hack it here and manually `inc` to make the notificiation show the right count assuming add-sku is successful.
                                                                      (str "Total: " (inc (or (fetch [qty-sub-query :new sku]) 0)))))
                                                         :textTrusted true
                                                         :type :info
                                                         :delay 2000}])]
                      (case page
                        :page/new-sale {:dispatch-n (list [:sale/add :new sku]
                                                          (confirmation-event nil))}
                        :page/new-order {:dispatch-n (list [:order/add-sku :new sku]
                                                           (confirmation-event :order/sku-qty))}
                        :page/new-transfer {:dispatch-n (list [:transfer/add-sku :new sku]
                                                              (confirmation-event :transfer/sku-qty))}
                        :page/new-count {:dispatch-n (list [:count/add-sku :new sku]
                                                           (confirmation-event :count/sku-qty))})))
            (cond
              (not (or skus sales gift-cards)) (notify-no)
              (multiple? (concat skus sales gift-cards)) (notify-multiple)
              :else (let [sku (first skus)
                          sale (first sales)
                          gift-card (first gift-cards)]
                      (cond
                        ;; Close any modals that might be open except when doing :navigate-sku since that opens the sku modal.
                        sale {:dispatch-n (list [:modal/close]
                                                [:navigate :page/sale sale])}
                        gift-card {:dispatch-n (list [:modal/close]
                                                     [:navigate :page/gift-card gift-card])}
                        sku {:dispatch [:navigate-sku sku]})))))))))

(reg-event-fx
 :scanner/collated
 (fn [{db :db}]
   (merge {:db (dissoc db :scanner/chars)}
          (let [barcode (-> (:scanner/chars db) str/join (str/replace #"\s" ""))] ; My test scanner put a \r at the end, maybe others do newlines and whatnot so clear all whitespace.
            (when (>= (count barcode) 8) ; Arbitrary minimum for the number of characters that can be in a barcode. Possibly not short enough for some existing barcodes?
              (on-barcode db barcode))))))



;;;; Web USB

;; I changed this to just set the device names since the actual device objects aren't equal between calls so the app-db keeps thinking it's been updated and the view rerenders a lot.
(reg-event-db
 :webusb/set-devices
 (fn [db [_ devices]]
   (assoc db :devices (webusb/device-names devices))))

(reg-event-db
 :webusb/clear-devices
 (fn [db]
   (dissoc db :devices)))

;; Call with `dispatch-sync` to prevent the browser "Must be handling a user gesture to show a permission request" error.
(reg-event-fx
 :webusb/connect
 (fn []
   {:webusb/connect nil}))

;; HACK I guess I should inject a devices coeffect but `get-devices` is async so that's annoying to do.
(reg-event-fx
 :webusb/send
 (fn [_ [_ type sku]]
   {:webusb/send-to-devices
    (case type
      :add {:line1 (fetch [:sku/name sku :full? true])
            :line2 (u/currency (fetch [:sku/price sku :inherit? true]))}
      :total {:line1 (str "Total: " (u/currency (fetch [:sale/total :new])))}
      :thanks {:line1 "Thank you!"})}))



;;;; Services (initialization, login/logout, clocks, active registers, etc.)

(defn on-failure [message error-type]
  (let [[heading text bad?]
        (case error-type
          :not-authenticated ["You've been logged out" (str message " Please log in again.") false]
          :network-error ["Network error" (str message " Is your internet connection down?") true]
          :internal-server-error ["Internal Server Error" message true])]
    [:modal/show :showstopper {:heading heading :text message :bad? bad?}]))

(reg-event-fx
 :default-failure
 (fn [_ [_ message error-type]]
   {:dispatch (on-failure message error-type)}))

(reg-event-fx
 :authenticate
 (fn [{:keys [db]}]
   (when-not (:user db)
     {:auth/login-current-user nil})))

(reg-event-fx
 :initialize
 (inject-cofx :server-flags)
 (fn [{:keys [db customer-display?]} [_ id]]
   (merge {:db (assoc db :user id)}
          (if customer-display?
            {:dispatch [:complete-initialization]}
            {:service {:name :service/initial-data
                       :compressed? true
                       :on-success [:fetch-initial-data/success]
                       :on-failure [:default-failure "App failed to load."]}}))))

(reg-event-fx
 :fetch-initial-data/success
 (fn [cofx [_ res]]
   (let [data (dt/read-transit-str res)]
     {:db (assoc (:db cofx) :polling/last-updated (:timestamp data))
      :db/init data})))

(reg-event-fx
 :more-init
 (fn [cofx]
   {:dispatch-n
    (concat (for [type [:type/shop
                        :type/register
                        :type/register-adjustment
                        :type/item
                        :type/category
                        :type/manufacturer
                        :type/order
                        :type/transfer
                        :type/count
                        :type/customer
                        :type/customer-type
                        :type/gift-card
                        :type/employee
                        :type/timesheet
                        :type/payment-type
                        :type/sale
                        ;; :type/refund ; No need for this at present, refunds only need  to be initialized when a refund is started.
                        ]]
              [:clear type])
            (list [:poll-for-updates]
                  [:complete-initialization]))}))

(reg-event-fx
 :complete-initialization
 (inject-cofx :server-flags)
 (fn [{:keys [db customer-display?]}]
   {:db (-> (assoc db :initialized? true)
            (assoc :user (q/find-one-by :employee/email (second (:user db))))) ; TODO temporary
    :dispatch-n (list [:navigation/dispatch-current] ; This has to be done after the db is loaded so that routing can check if any ids in the path are valid. Preferably after new-conn is initialized too.
                      (when-not customer-display?
                        [:complete-initialization-passkey]))}))

(defn passkey-init-db-and-realtime [db passkey]
  {:db (assoc db :passkey passkey)
   :realtime/init passkey})

(reg-event-fx
 :complete-initialization-passkey
 (inject-cofx :local-storage)
 (fn [{db :db passkey :local-storage}]
   (if passkey
     (passkey-init-db-and-realtime db passkey)
     {:dispatch [:complete-initialization-passkey-2]})))

(reg-event-fx
 :complete-initialization-passkey-2
 (inject-cofx :new-passkey)
 (fn [{db :db passkey :new-passkey}]
   (merge (passkey-init-db-and-realtime db passkey)
          {:local-storage-set passkey})))

(reg-event-fx
 :logout
 (inject-cofx :anything-pending?)
 (fn [{:keys [db] :as cofx}]
   (when (or (not (:anything-pending? cofx))
             (js/confirm "Are you sure you want to log out? Changes you made may not be saved.")) ; `js/confirm` is side-effecty so this should really be in fx.cljs. It's in other events too.
     {:db (dissoc db :user)
      :auth/sign-out nil})))

(reg-event-fx
 :customer-display/set-passkey
 (fn [cofx [_ passkey]]
   {:db (assoc (:db cofx) :customer-display/passkey passkey)
    :realtime/init passkey}))


(reg-event-fx
 :persist
 (fn [_ [_ tx]]
   {:service {:name :service/persist
              :payload tx
              :on-success [:persist/success]
              :on-failure [:persist/failure]}}))

(reg-event-fx
 :persist/success
 (fn []
   {:comms/request-finished :success}))

(reg-event-fx
 :persist/failure
 (fn [_ [_ error-type]]
   {:dispatch (on-failure "Your change was not saved." error-type)
    :comms/request-finished :failure}))

(reg-event-fx
 :change-pin
 (fn [{:keys [db]} [_ current-pin new-pin]]
   {:db (assoc db :change-pin/working? true)
    :service {:name :service/change-pin
              :payload {:current-pin current-pin
                        :new-pin new-pin}
              :on-success [:change-pin/success]
              :on-failure [:change-pin/failure]}}))

(defn change-pin-working-flag-off [cofx]
  (dissoc (:db cofx) :change-pin/working?))

(reg-event-fx
 :change-pin/success
 (fn [cofx [_ res]]
   (merge {:db (change-pin-working-flag-off cofx)}
          {:dispatch [:notify (case res
                                :current-pin-wrong "Wrong current PIN."
                                :pin-in-use "That PIN is in use by another user."
                                {:text "PIN changed." :type :success})]})))

(reg-event-fx
 :change-pin/failure
 (fn [cofx [_ error-type]]
   {:db (change-pin-working-flag-off cofx)
    :dispatch (on-failure "PIN change failed." error-type)}))


;;; Clocks

(defn clock-working-key [how employee-or-pin]
  [:clocks/working (if (= how :pin) :pin employee-or-pin)])

(reg-event-fx
 :clock-in-out
 (inject-cofx :conn)
 (fn [cofx [_ how employee-or-pin]]
   (when-not (get-in (:db cofx) (clock-working-key how employee-or-pin))
     {:db (assoc-in (:db cofx) (clock-working-key how employee-or-pin) true)
      :service {:name :service/clock-in-out
                :payload {:how how
                          :employee-or-pin employee-or-pin
                          :time (u/now)}
                :on-success [:clock-in-out/success how employee-or-pin]
                :on-failure [:clock-in-out/failure how employee-or-pin]}})))

(defn clock-working-flag-off [cofx how employee-or-pin]
  (dissoc-in (:db cofx) (clock-working-key how employee-or-pin)))

(reg-event-fx
 :clock-in-out/success
 (fn [cofx [_ how employee-or-pin {:keys [in-or-out employee tx] :as res}]]
   (merge {:db (clock-working-flag-off cofx how employee-or-pin)}
          (if (= res :no-employee-with-pin)
            {:dispatch [:notify "No employee with that PIN."]}
            {:db/transact-local tx
             :dispatch [:notify {:text (str "Clocked " (name in-or-out) " " (m/employee:name (fetch [:employee employee])) ".")
                                 :type :success}]}))))

(reg-event-fx
 :clock-in-out/failure
 (fn [cofx [_ how employee-or-pin error-type]]
   {:db (clock-working-flag-off cofx how employee-or-pin)
    :dispatch (on-failure (str "Clock change was not saved.") error-type)}))


;;; Active registers

(reg-event-fx
 :open-close-register
 (fn [{db :db} [_ after open-or-close register {:keys [amounts left-in-drawer notes]}]]
   (when-not (get-in db [:active-registers/working register])
     {:db (assoc-in db [:active-registers/working register] true)
      :service {:name :service/open-close-register
                :payload {:open-or-close open-or-close
                          :register register
                          :amounts amounts
                          :left-in-drawer left-in-drawer
                          :employee (:user db)
                          :notes notes
                          :time (u/now)}
                :on-success [:open-close-register/success after open-or-close register]
                :on-failure [:open-close-register/failure after open-or-close register]}})))

(defn open-close-register-finished [cofx open-or-close register]
  (let [db (dissoc-in (:db cofx) [:active-registers/working register])]
    (case open-or-close
      :open db
      :close (dissoc db :current-register)))) ; I'm not sure if it's necessary to unset :current-register after closing the register but the logic is complicated and I don't feel like thinking it through.

(reg-event-fx
 :open-close-register/success
 (fn [cofx [_ after open-or-close register res]]
   (merge {:db (open-close-register-finished cofx open-or-close register)}
          (if (= res :already)
            {:dispatch-n (list after
                               [:notify {:text (str "Register already " (case open-or-close :open "opened" :close "closed") ".")
                                         :type :notice}])}
            (merge {:db/transact-local (:tx res)
                    :dispatch-n (list after
                                      (case open-or-close
                                        :open [:choose-shop/continue register]
                                        :close [:navigate :page/register-count-complete (:register-count-id res)]))})))))

(reg-event-fx
 :open-close-register/failure
 (fn [cofx [_ _ open-or-close register error-type]]
   {:db (open-close-register-finished cofx open-or-close register)
    :dispatch (on-failure (str "Register " (name open-or-close) " was not saved.") error-type)}))


;;; Email

(reg-event-fx
 :send-email-receipt
 (fn [_ [_ sale {:keys [email name subject header footer]}]]
   {:service {:name :service/send-email
              :payload {:to (if name
                              (str name " <" email ">")
                              email)
                        :subject subject
                        :html (emails/sale-receipt (fetch [:sale sale]) header footer)}
              :on-success [:notify {:text (str "Receipt emailed to " email) :type :success}]
              :on-failure [:notify {:text "Emailing failed."}]}}))


;;; Sign Up

;; TODO
;; - Ideally make db entity creation and AWS user a transaction in that both succeed or fail together.
;; - When I go to do optimistic responses for offline it might be ideal to have the employee creation happen on the client side immediately (already doing this) and then get removed (or never transacted if I use `d/with`) if there's a server error doing the AWS user part.
;; - both of these for :lock-unlock too
(reg-event-fx
 :sign-up
 (fn [_ [_ email]]
   {:dispatch [:store/create :type/employee
               :additional {:employee/role :associate}]
    :service {:name :service/sign-up
              :payload email
              :on-success [:notify {:text "Employee created and email verification sent." :type :success}]
              :on-failure [:notify {:text "Employee creation failed."}]}}))


;; Employee Lock

;; TODO see transaction / optimistic reponse notes for :sign-up. They also apply to any event that uses :lock-unlock, namely :employee/archive-and-lock
(reg-event-fx
 :lock-unlock
 (fn [_ [_ lock-or-unlock employee]]
   {:dispatch [:store/save* employee :type/employee :employee/locked? (case lock-or-unlock :lock true :unlock false)]
    :service {:name :service/lock-unlock
              :payload {:lock-or-unlock lock-or-unlock
                        :email (:employee/email (q/entity employee))}
              :on-success [:notify {:text (str "Employee " (case lock-or-unlock :lock "locked" :unlock "unlocked") ".")
                                    :type :success}]
              :on-failure [:notify {:text "Employee lock state change failed."}]}}))


;;; Screen Lock / Switch User

(reg-event-db
 :screen/lock
 (fn [db [_ how]]
   (assoc db :screen/locked? true)))

(reg-event-fx
 :screen/unlock
 (fn [cofx [_ pin]]
   {:db (assoc (:db cofx) :unlock/working? true)
    :service {:name :service/switch-user
              :payload pin
              :on-success [:screen.unlock/success]
              :on-failure [:screen.unlock/failure]}}))

(reg-event-fx
 :screen.unlock/success
 (fn [cofx [_ user]]
   (let [db (dissoc (:db cofx) :unlock/working?)]
     (if user
       {:db (-> db
                (assoc :user user)
                (dissoc :screen/locked?))}
       {:db db
        :dispatch [:notify "No employee with that PIN."]}))))

(reg-event-fx
 :screen.unlock/failure
 (fn [cofx [_ error-type]]
   (merge {:db (dissoc (:db cofx) :unlock/working?)
           :dispatch (on-failure "Unlock failed." error-type)})))


;;; Polling

(def polling-interval 30000)

(reg-event-fx
 :poll-for-updates
 (fn [cofx]
   {:service {:name :service/poll-for-updates
              :payload (:polling/last-updated (:db cofx))
              :on-success [:poll-for-updates/success]
              :on-failure [:poll-for-updates/failure]}
    :dispatch-later [{:dispatch [:poll-for-updates]
                      :ms polling-interval}]}))

(reg-event-fx
 :poll-for-updates/success
 (fn [cofx [_ data]]
   {:db (assoc (:db cofx) :polling/last-updated (:timestamp data))
    :db/transact-polling-updates
    ;; Map statements (create operations) are not idempotent and running them again makes datascript throw an error (:db.unique/value constraint), so filter any out that would create entities that already exist in the local db (presumably because they originated with the user).
    (keep (fn [tx]
            (seq (remove (fn [statement]
                           (and (map? statement) (q/entity (:e/id statement))))
                         tx)))
          (:txs data))}))

(reg-event-fx
 :poll-for-updates/failure
 (fn []
   ;; Nothing here for now. Maybe at some point it'll stop pollling if there are too many errors or the user becomes unauthenticated or something.
   ))


;;; Chat

(reg-event-fx
 :chat-login
 (fn [{:keys [db]} [_ node]]
   (if-let [token (get-in db [:chat-tokens (:user db)])]
     {:chat/login-iframe [node token]}
     {:service {:name :service/chat-auth
                :on-success [:chat-auth/success node]
                :on-failure [:notify {:text "Unable to connect to chat."}]}})))

(reg-event-fx
 :chat-auth/success
 (fn [{:keys [db]} [_ node token]]
   {:db (assoc-in db [:chat-tokens (:user db)] token)
    :chat/login-iframe [node token]}))



;;;; Storage

(defn no-empties [x]
  (if (coll? x)
    (let [new-x (cond
                  (map? x)
                  (no-nils (fmap no-empties x))
                  (vector? x)
                  (vec (remove nil? (map no-empties x)))
                  :else (throw (ex-info "Unhandled collection type." {:value x})))]
      (if (empty? new-x) nil new-x))
    x))

(reg-event-fx
 :store/save
 [(inject-cofx :conn) (inject-cofx :new-conn)]
 (fn [{:keys [conn new-conn]} [_ id attribute value]]
   (let [tx (let [v (no-empties value)]
              (if (nil? v)
                [[:db.fn/retractAttribute [:e/id id] attribute]]
                [[:db/add [:e/id id] attribute (spec/parse attribute v)]]))]
     (if (u/new-id? id)
       (when (spec/valid-tx? new-conn tx :simple)
         {:db/transact-new tx})
       (when (spec/valid-tx? conn tx :whole)
         {:db/transact tx})))))

(reg-event-fx
 :store/save*
 (fn [_ [_ id type short-attribute value]]
   (let [id (if (= :new id)
              (fetch [:id/new type])
              id)
         attribute (keyword (name type) short-attribute)]
     {:dispatch [:store/save id attribute value]})))

;; TODO copypasta from subs
(defn grab-fact [id attr]
  (d/q [:find '?p '.
        :in '$ '?e
        :where ['?e attr '?p]]
       (if (u/new-id? id)
         @hyperdrive.db/new-conn @hyperdrive.db/conn)
       [:e/id id]))

(reg-event-fx
 :store/update*
 (fn [_ [_ id type attribute update-fn]]
   (let [fact (grab-fact (if (= :new id)
                           (fetch [:id/new type])
                           id)
                         attribute)]
     {:dispatch [:store/save* id type attribute (update-fn fact)]})))

(reg-event-fx
 :store/create
 [(inject-cofx :conn) (inject-cofx :new-conn)]
 (fn [{:keys [conn new-conn]} [_ type & {:keys [doc id additional]}]]
   (let [entity (-> (or doc (-> (d/pull new-conn '[*] [:e/id (d/q q/new-id-query new-conn type)])
                                (dissoc :db/id)))
                    (merge (new-entity type) additional)
                    (update :e/id (fn [eid]
                                    (or id eid))))
         tx [entity]]
     (when (spec/valid-tx? conn tx :whole)
       (merge {:db/transact tx}
              (when-not doc
                {:dispatch (when-not doc
                             [:clear type])}))))))

(reg-event-fx
 :store/delete
 (fn [_ [_ eid after]]
   (when (js/confirm "Confirm delete?") ; `js/confirm` is side-effecty so this should really be in fx.cljs.
     (merge {:db/transact [[:db/retractEntity [:e/id eid]]]}
            (when after
              {:dispatch after})))))



;;;; Navigation

(reg-event-fx
 :navigation/configure
 (fn []
   {:navigation/configure nil}))

(reg-event-fx
 :navigation/dispatch-current
 (fn []
   {:navigation/dispatch-current nil}))

(defn set-page [db name args]
  {:db (assoc db :page/name name :page/args args)})

;; An id is the only argument most page components take currently so I've harcoded this logic for the now. It would be straightforward to expand this to extract different route params for different routes (or just use real function handlers in the routes data with bidi's tagged handlers).
(defn route-handler [db path]
  (if-let [match (routes/match path)]
    (let [{page-name :handler params :route-params} match]
      (if (and (or (= page-name :page/sales-landing)
                   (= page-name :page/new-sale))
               (not (q/register-chosen? db)))
        {:dispatch-n (list [:choose-shop/set-continue path]
                           [:navigate :page/choose-shop])}
        (merge (set-page db page-name (when-let [id (:id params)] [id]))
               (when (= page-name :page/new-sale)
                 {:dispatch [:screen/lock]}))))
    (set-page db :page/not-found nil)))

(reg-event-fx
 :route
 (fn [{db :db} [_ path]]
   (merge
    (route-handler db path)
    {:scroll-to-top nil})))


(reg-event-fx
 :navigate
 (fn [_ [_ route params]]
   {:navigate (routes/path route params)}))

(reg-event-fx
 :navigate-path
 (fn [_ [_ path]]
   {:navigate path}))

(reg-event-fx
 :navigate-sku
 (fn [_ [_ sku]]
   {:dispatch-n (list [:navigate :page/item (:item (fetch [:sku sku]))]
                      [:modal/show :edit-sku sku])}))



;;;; Miscellaneous

(reg-event-fx
 :init-window
 (fn []
   {:init-window nil}))

(reg-event-fx
 :reload
 (fn []
   {:reload nil}))

;; Call with `dispatch-sync` to (hopefully) prevent browser popup blocking.
(reg-event-fx
 :open-tab
 (fn [_ [_ content]]
   {:open-tab content}))

(reg-event-fx
 :tab
 (fn []
   {:tab nil}))

(reg-event-fx
 :alert
 (fn [_ [_ message]]
   {:alert message}))

(reg-event-db
 :set-filepicker-working?
 (fn [db [_ working?]]
   (if working?
     (assoc db :filepicker-working? true)
     (dissoc db :filepicker-working?))))

(reg-event-fx
 :notify
 (fn [_ [_ opts]]
   {:notify (merge {:styling "bootstrap4"
                    :icons "fontawesome5"
                    :icon false
                    :type :error
                    :delay 6000
                    :modules {:Buttons {:sticker false}}}
                   (if (string? opts)
                     {:text opts} opts))}))









(reg-event-fx
 :show-email-receipt-modal
 (inject-cofx :conn)
 (fn [{:keys [conn]} [_ sale]]
   (let [customer (d/q '[:find (pull ?customer [:customer/email :customer/first-name :customer/last-name]) .
                         :in $ ?sid
                         :where
                         [?sale :e/id ?sid]
                         [?sale :sale/customer ?cid]
                         [?customer :e/id ?cid]]
                       conn sale)]
     {:dispatch [:modal/show :email-receipt sale {:email (:customer/email customer)
                                                  :name (m/customer:name (rename-keys customer {:customer/first-name :first-name
                                                                                                :customer/last-name :last-name}))}]})))









(defn render-currency [k]
  #(currency (get % k) :no-sign? true :coerce-nil? false))

(def export-config
  {:export/items {:file "items"
                  :columns [{:header "Name" :value :name}
                            {:header "Manufacturer" :value #(q/fact (:manufacturer %) :manufacturer/name)}
                            {:header "Price" :value (render-currency :price)}
                            {:header "MSRP" :value (render-currency :msrp-price)}
                            {:header "Wholesale" :value (render-currency :wholesale-price)}
                            {:header "Cost" :value (render-currency :default-cost)}
                            {:header "Stock" :value #(db/stock (:id %))}]} ; The stock cache should be a coeffect.
   :export/orders {:file "orders"
                   :columns [{:header "ID" :value :id}
                             {:header "Status" :value #(humanize (:status %))}
                             {:header "Shop" :value #(q/fact (:shop %) :shop/name)}
                             {:header "Vendor" :value :vendor}
                             {:header "Notes" :value :notes}]}})

;; Using table data directly is the most general approach: it will work even if the table entries don't correspond to database entities (though presumably in such cases the table is still sourcing its data in SOME way and perhaps I should be doing that querying, etc. directly).
(reg-event-fx
 :export
 (fn [_ [_ kind items]]
   (let [{:keys [file columns]} (get export-config kind)
         filename (str file ".csv")
         content (csv/write-csv
                  (cons
                   (for [col columns]
                     (:header col))
                   (for [item items]
                     (map #((:value %) item) columns))))]
     {:download [filename content]})))







(reg-event-fx
 :customer-views/send
 (fn [_ [_ type data]]
   (case type
     :add {:dispatch [:webusb/send :add data]}
     :total {:dispatch [:webusb/send :total]}
     :thanks {:dispatch-n (list [:webusb/send :thanks]
                                [:customer-display/present :thanks])})))



(reg-event-fx
 :customer-display/present
 (fn [_ [_ phase]]
   {:realtime/trigger
    [:customer-display/receive-present
     (let [sale (fetch [:sale/new])
           sale-id (:id sale)]
       (merge (case phase
                :sale
                {:phase :customer-display.phase/sale
                 :items (map (fn [line]
                               (assoc (case (:line-type line)
                                        :sku
                                        {:name (fetch [:sku/name (:sku line) :full? true])
                                         :image (fetch [:sku/primary-image (:sku line) :inherit? true])}
                                        :credit-account
                                        {:name "Credit Account Deposit"}
                                        :gift-card
                                        {:name (str "Gift Card Purchase: " (:code line))})
                                      :price (:price line)))
                             (:lines sale))
                 :total (fetch [:sale/total sale-id])}
                :thanks
                {:phase :customer-display.phase/thanks
                 :customer-first-name (q/fact (:customer sale) :customer/first-name)
                 :total (fetch [:sale/total sale-id])
                 :change (q/sale-cash-change sale-id)
                 :tier-savings (when-let [customer (:customer sale)]
                                 (let [tier-discount-percent (q/customer:discount customer)]
                                   (when (and (not (zero? tier-discount-percent))
                                              (not (zero? (:discount sale))))
                                     (fetch [:sale/discount-amount sale-id]))))})
              (when-let [customer (:customer sale)]
                (let [tier (q/customer:tier customer)]
                  {:customer {:name (fetch [:customer/name customer])
                              :tier-name (humanize (get u/tiers tier))
                              :tier-img (u/tier-image tier)}}))))]}))

(reg-event-fx
 :customer-display/signup
 (fn [_ [_ form]]
   {:realtime/trigger [:customer-display/receive-signup form]}))

(reg-event-db
 :customer-display/receive-present
 (fn [db [_ state]]
   (assoc db :customer-display/state state)))

(reg-event-fx
 :customer-display/receive-signup
 (fn [_ [_ form]]
   (if-let [customer (q/find-customer-by-email (:email form))]
     {:dispatch [:sale.new/set-customer :new customer]}
     {:dispatch-n (conj
                   (for [[attr v] (rename-keys form {:phone :mobile-phone})]
                     [:customer/change :new attr v])
                   [:modal/show :new-customer [:sale.new/set-customer :new]])})))




(reg-event-fx
 :customer-display/maybe-present-sale
 (inject-cofx :new-conn)
 (fn [{:keys [db new-conn]}]
   (let [last-sale-state (:customer-display/last-sale-state db)
         sale-state (d/pull new-conn '[*] [:e/id (d/q q/new-id-query new-conn :type/sale)])]
     (when-not (= sale-state last-sale-state)
       {:db (assoc db :customer-display/last-sale-state sale-state)
        :dispatch [:customer-display/present :sale]}))))







(defn pull-new-sale [new-conn]
  (let [new-sale-id (d/q q/new-id-query new-conn :type/sale)]
    (dissoc (d/pull new-conn '[*] [:e/id new-sale-id]) :db/id)))

(defn retract-new-sale-statement [new-sale-id]
  [:db/retractEntity [:e/id new-sale-id]])

(reg-event-fx
 :held-receipts/hold
 (inject-cofx :new-conn)
 (fn [{:keys [db new-conn]}]
   {:db (update db :held-receipts conj (pull-new-sale new-conn))
    :dispatch-n (list [:clear :type/sale]
                      ;; TODO clear any pending refund too
                      [:navigate :page/sales-landing])}))

(reg-event-fx
 :held-receipts/continue
 (inject-cofx :new-conn)
 (fn [{:keys [db new-conn]} [_ held-receipt-id]]
   (let [new-sale (pull-new-sale new-conn)
         held-receipt (first (filter #(= held-receipt-id (:e/id %)) (:held-receipts db)))]
     {:db (update db :held-receipts
                  (fn [held-receipts]
                    (as-> held-receipts $
                      (remove #(= held-receipt %) $)
                      (if (seq (dissoc new-sale :db/id :e/id :e/type :e/date)) ; Save as a held receipt only if the sale is modified. Brittle: if there are other initial attributes in a new sale someday this won't work and unmodified new sales will always be stored as held receipts when a held receipt is continued.
                        (conj $ new-sale) $))))
      :db/transact-new [(retract-new-sale-statement (:e/id new-sale))
                        held-receipt]
      :dispatch [:navigate :page/new-sale]})))

(reg-event-db
 :held-receipts/cancel
 (fn [db [_ held-receipt-id]]
   (update db :held-receipts (fn [held-receipts]
                               (remove #(= held-receipt-id (:e/id %)) held-receipts)))))








;; TODO For import/export I should be using the schema to pull off any :ref attributes so they aren't carried over if more (other than manufacturer and cateogory) are added leter.

;; TODO I use `new-entity`, `valid-tx?` and the db/transact effect directly here. Do I want to keep (some) of those centralized? Consider this after the datomic rewrite.
(reg-event-fx
 :import-items
 (inject-cofx :conn)
 (fn [{:keys [db conn]} [_ s]]
   (if-let [raw-items (let [res (try
                                  (read-string s)
                                  (catch :default _))]
                        (when (coll? res) res))] ; Coarse check in case a non-edn file is "successfully" parsed.
     (let [category-lookup (into {} (map (fn [id]
                                           [(fetch [:category/path id]) id])
                                         (q/find-by :e/type :type/category)))
           items (map (fn [item]
                        (as-> item $
                          (merge $ (new-entity :type/item)) ; new-entity after item in merge to overwrite :e/id, etc.
                          (assoc $ :item/employee (:user db))
                          (dissoc $ :manufacturer-name :category-name)
                          (if-let [manufacturer (some->> item
                                                         :manufacturer-name
                                                         (q/find-one-by :manufacturer/name))]
                            (assoc $ :item/manufacturer manufacturer) $)
                          (if-let [category (some->> item
                                                     :category-name
                                                     (get category-lookup))]
                            (assoc $ :item/category category) $)))
                      raw-items)]
       (if (spec/valid-tx? conn items :whole)
         (when (js/confirm (str "Import " (count items) " items?"))
           {:db/transact items
            :dispatch [:notify {:text "Items imported." :type :success}]})
         {:dispatch [:notify "Items failed validation."]}))
     {:dispatch [:notify "Malformed file."]})))

(reg-event-fx
 :export-items
 (inject-cofx :conn)
 (fn [{:keys [conn]} [_ items]]
   (let [data (->> (map #(do [:e/id (:id %)]) items)
                   (d/pull-many conn '[*])
                   (map (fn [item]
                          (as-> item $
                            (dissoc $ :db/id)
                            (if-let [manufacturer (:item/manufacturer item)]
                              (assoc $ :manufacturer-name (q/fact manufacturer :manufacturer/name)) $)
                            (if-let [category (:item/category item)]
                              (assoc $ :category-name (fetch [:category/path category])) $)))))]
     {:download ["export.edn" (pr-str data)]})))









(doseq [event-id [:auth/init
                  :auth/change-password
                  :scroll-to-top
                  :service]]
  (reg-event-fx
   event-id
   (fn [_ [_ & args]]
     {event-id (if (= (count args) 1)
                 (first args) args)})))







(reg-event-fx
 :toggle-sidebar-shown
 (fn [{:keys [db]}]
   (let [v (not (:sidebar-toggled? db))]
     {:db (assoc db :sidebar-toggled? v)
      :toggle-sidebar-shown-impure v})))












(reg-event-fx
 :employee/archive-and-lock
 (fn [_ [_ employee]]
   {:dispatch-n (list [:lock-unlock :lock employee]
                      [:store/save employee :archivable/archived? true]
                      [:navigate :page/employees])}))









(reg-event-db
 :save-install-prompt-event
 (fn [db [_ event]]
   (assoc db :install-prompt-event event)))

;; Call with `dispatch-sync` just in case.
(reg-event-fx
 :install-app
 (fn [{:keys [db]}]
   {:db (dissoc db :install-prompt-event)
    :show-install-prompt (:install-prompt-event db)}))













(reg-event-fx
 :start-refund-bogus
 (fn []
   {:dispatch [:sale/change :new :refund (:id (fetch [:refund/new]))]}))

(reg-event-fx
 :start-refund
 (fn [_ [_ sale-id]]
   {:dispatch-n (list [:clear :type/refund]
                      [:refund/change :new :sale sale-id]
                      [:start-refund-bogus] ; TODO get the ID of the new refund after clearing is done yuck
                      (when-let [customer (:customer (fetch [:sale sale-id]))]
                        [:sale.new/set-customer :new customer])
                      [:navigate :page/new-sale])}))


(reg-event-fx
 :clear-sale-and-refund
 (fn []
   {:dispatch-n (list [:clear :type/sale]
                      [:clear :type/refund])}))
