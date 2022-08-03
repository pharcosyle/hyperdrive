(ns hyperdrive.singularity.subs
  (:require-macros [reagent.ratom :refer [reaction] :rename {reaction rxn}])
  (:require [clojure.string :as str]
            [clojure.set :refer [difference]]
            [datascript.core :as d]
            [re-frame.core :refer [reg-sub reg-sub-raw subscribe dispatch] :rename {subscribe sub}]
            [reagent.ratom :refer [make-reaction]]
            [posh.reagent :as p]
            [hyperdrive.db :as db :refer [conn new-conn]]
            [hyperdrive.queries :as q]
            [hyperdrive.spec :as spec]
            [hyperdrive.util :as u :refer [listen fetch val-or-deref fixture]]
            [hyperdrive.singularity.comms :as comms]
            [hyperdrive.singularity.models :as m]
            [hyperdrive.singularity.webusb :as webusb]
            [hyperdrive.singularity.views :refer [lookup-page lookup-modal]]))





#_(defn pull-doc [query & params]
    (let [id (apply p/q query conn params)
          entity (rxn (p/pull conn '[*] @id))
          doc (u/strip @entity)]
      doc))

(defn pull-docs [query & params]
  (let [ids (apply p/q query conn params)
        entities (rxn (map (fn [id]
                             (p/pull conn '[*] id))
                           @ids))
        docs (rxn (map (fn [entity]
                         (u/strip @entity))
                       @entities))]
    docs))






(def size-comp (u/order-comp
                u/alpha-sizes
                (fn [a b]
                  (let [a-num (u/parse-number a)
                        b-num (u/parse-number b)]
                    (if (and a-num b-num)
                      (compare a-num b-num)
                      (compare a b))))))


;;;; Initialization

(reg-sub
 :logged-in?
 (fn [db]
   (:user db)))

(reg-sub
 :initialized?
 (fn [db]
   (:initialized? db)))



;;;; "Session"

(reg-sub-raw
 :user
 (fn [app-db]
   (let [id (rxn (:user @app-db))]
     (rxn (listen [:employee @id])))))

(reg-sub-raw
 :current-register
 (fn [app-db]
   (let [id (rxn (:current-register @app-db))]
     (rxn (listen [:register @id])))))

(reg-sub-raw
 :current-shop
 (fn []
   (let [id (rxn (:shop (listen [:current-register])))]
     (rxn (listen [:shop @id])))))

(reg-sub
 :register-chosen?
 (fn [db]
   (q/register-chosen? db)))

(reg-sub
 :auth?
 (fn [db [_ action]]
   (q/auth? (:user db) action)))

(reg-sub
 :screen/locked?
 (fn [db]
   (:screen/locked? db)))


;;; Page

(reg-sub
 :current-page
 (fn [db]
   (q/page:name db)))

(reg-sub
 :page/component
 (fn [db]
   ;; Page can take a little bit to get set initially particularly because of redirects (additional :navigate events), so guard against a nil page name.
   (when-let [page-name (q/page:name db)]
     (if (q/auth? (:user db) page-name)
       (vec (cons (lookup-page page-name)
                  (:page/args db)))
       [(lookup-page :page/unauthorized)]))))

(reg-sub
 :page/template?
 (fn [db]
   (not (contains? #{:page/customer-display} (q/page:name db)))))


;;; Modal

(reg-sub
 :modal/component
 (fn [db]
   (when-let [modal-name (:modal/name db)]
     (vec (cons (lookup-modal modal-name)
                (:modal/args db))))))



;;;; Models

(let [dict {:shop :shops
            :register :registers
            :register-count :register-counts
            :register-adjustment :register-adjustments
            :item :items
            :sku :skus
            :category :categories
            :manufacturer :manufacturers
            :order :orders
            :transfer :transfers
            :count :counts
            :customer :customers
            :customer-type :customer-types
            :credit-account :credit-accounts
            :gift-card :gift-cards
            :employee :employees
            :timesheet :timesheets
            :sale :sales
            :payment-type :payment-types}]
  (defn- models-for [model]
    (get dict model)))

(doseq [type [:shop :register :register-count :register-adjustment :item :sku :category :manufacturer :order :transfer :count :customer :customer-type :credit-account :gift-card :employee :timesheet :sale :refund :payment-type]
        :let [models (models-for type)]]
  (reg-sub-raw
   models
   (if (some #{type} [:item :customer])
     (fn [_ [_ ids]]
       (rxn
        (let [entity-cache (get @db/entities-cache (keyword :type type))]
          (vals (if ids
                  (select-keys entity-cache ids)
                  entity-cache)))))
     (fn [_ [_ ids]]
       (let [passed-in-ids? ids ; TODO temporary
             ids (or ids
                     (p/q '[:find [?e ...]
                            :in $ ?type
                            :where [?e :e/type ?type]]
                          conn (keyword :type type)))
             docs (rxn (map (fn [id]
                              (u/strip (datascript.core/pull @conn '[*] (if passed-in-ids?
                                                                          [:e/id id]
                                                                          id))))
                            (val-or-deref ids)))]
         docs)
       #_(rxn (map u/strip (datascript.core/q '[:find [(pull ?e [*]) ...]
                                                :in $ ?type
                                                :where [?e :e/type ?type]]
                                              @conn (keyword :type type)))))))

  (reg-sub-raw
   type
   (fn [_ [_ eid]]
     (rxn (u/strip @(p/pull (if (u/new-id? eid) new-conn conn) '[*] [:e/id eid])))))  ; HACK: I'm passing around ids in the view for the payment modal rewrite, curently the only place a new id would get passed into a regular entity sub is the credit account bit in the payment modal. Not sure if this qualifies as a hack but it's certainly more confusing now with the conflated approaches.

  ;; TODO remove
  ;; (reg-sub
  ;;  models
  ;;  (fn [db [_ ids]]
  ;;    (let [docs (get db model)]
  ;;      (if ids
  ;;        (filter #(u/seq-contains? ids (:id %)) docs)
  ;;        docs))))

  ;; TODO remove
  ;; (reg-sub
  ;;  model
  ;;  :<- [models]
  ;;  (fn [all [_ id]]
  ;;    (u/find-by-id id all)))
  )

(defn new-handler [type]
  (let [eid (sub [:id/new type])
        entity (rxn @(p/pull new-conn '[*] [:e/id @eid]))]
    (rxn (u/strip @entity))))

(doseq [type [:shop :register :register-adjustment :item :category :manufacturer :order :transfer :count :customer :customer-type :gift-card :employee :timesheet :payment-type :sale :refund]
        :let [new-key (keyword type :new)]]
  (reg-sub-raw
   new-key
   (fn []
     (new-handler (keyword :type type)))))

;; Use this instead of the type-specific :new event above from now on. Ultimately it'd be nice to just use :id/new.
(reg-sub-raw
 :new
 (fn [_ [_ type]]
   (new-handler type)))

(reg-sub-raw
 :id/new
 (fn [_ [_ type]]
   (p/q q/new-id-query new-conn type)))

(reg-sub-raw
 :new/invalid?
 (fn [_ [_ type]]
   (let [id (sub [:id/new type])
         entity (rxn @(p/pull new-conn '[*] [:e/id @id]))]
     (rxn (not (spec/valid-new-entity? @entity))))))


;;; Model helpers

(defn id-or-new-signal [model id]
  (if (or (= :new id) (u/new-id? id)) ; HACK: I'm passing around ids in the view for the payment modal rewrite so sometimes a new id and not just :new will get passed to sale-signal. Not sure if this qualifies as a hack but it's certainly more confusing now with the conflated approaches.
    (sub [(keyword model :new)])
    (sub [model id])))

(defn signal-fn [model]
  (fn [[_ id]]
    (id-or-new-signal model id)))

(defn price [inv price-type inherit?]
  (let [price (get inv (keyword (name price-type)))]
    (if (and inherit? (not price))
      (:price inv) price)))

(defn primary-image [inv]
  (first (:images inv)))


(reg-sub
 :shop/registers
 :<- [:registers]
 (fn [registers [_ shop]]
   (u/find {:shop shop} registers)))

(def item-signal (partial id-or-new-signal :item))
(def item-signal-fn (signal-fn :item))

(reg-sub-raw
 :item/full-name
 (fn [_ [_ id]]
   (let [item (item-signal id)
         manufacturer (rxn (listen [:manufacturer (:manufacturer @item)]))]
     (rxn
      (str (when @manufacturer
             (str (:name @manufacturer) " "))
           (:name @item))))))

;; (reg-sub
;;  :item/attributes
;;  item-signal-fn
;;  (fn [item [_ _ attr]]
;;    (sort (case attr
;;            :colors compare
;;            :sizes size-comp)
;;          (get item attr))))

(reg-sub-raw
 :item/full-attributes
 (fn [_ [_ item colors-or-sizes]]
   (let [item-attr-value (p/q '[:find ?attr-value .
                                :in $ ?item ?attr
                                :where
                                [?e :e/id ?item]
                                [?e ?attr ?attr-value]]
                              conn item (case colors-or-sizes
                                          :colors :item/colors
                                          :sizes :item/sizes))
         sku-attr-values (p/q '[:find [?attr-value ...]
                                :in $ ?item ?attr
                                :where
                                [?e :sku/item ?item]
                                [?e ?attr ?attr-value]]
                              conn item (case colors-or-sizes
                                          :colors :sku/color
                                          :sizes :sku/size))]
     (rxn
      (->> (concat @item-attr-value @sku-attr-values)
           set
           (sort (case colors-or-sizes
                   :colors compare
                   :sizes size-comp)))))))

(reg-sub-raw
 :item/skus-index
 (fn [_ [_ item]]
   (let [skus (p/q '[:find ?sku ?color ?size
                     :in $ ?item
                     :where
                     [?e :sku/item ?item]
                     [?e :e/id ?sku]
                     [?e :sku/color ?color]
                     [?e :sku/size ?size]]
                   conn item)]
     (rxn
      (->> @skus
           (map (fn [[sku color size]]
                  [[color size] sku]))
           (into {}))))))

;; (reg-sub-raw
;;  :item/sku
;;  (fn [_ [_ item color size]]
;;    (let [sku (p/q '[:find ?e .
;;                     :in $ ?item ?color ?size
;;                     :where
;;                     [?e :sku/item ?item]
;;                     [?e :sku/color ?color]
;;                     [?e :sku/size ?size]]
;;                   conn item color size)]
;;      (rxn (q/eid @sku)))))

(reg-sub-raw
 :item/skus
 (fn [_ [_ item]]
   (pull-docs q/item:skus-query item)))

(reg-sub-raw
 :item/stock
 (fn [_ [_ item shop]]
   (rxn (db/stock item shop))))

;; (reg-sub-raw
;;  :item/stock
;;  (fn [_ [_ item shop]]
;;    (let [skus (rxn (map :id (listen [:item/skus item])))
;;          stocks (rxn (for [sku @skus]
;;                        (sub [:sku/stock sku shop])))]
;;      (rxn (apply + (map deref @stocks))))))

(reg-sub-raw
 :item/stock-history
 (fn [_ [_ item opts]]
   (let [skus (if-let [sku (:sku opts)]
                #{sku}
                (q/item:skus item))]
     (rxn (q/item:stock-history (-> opts
                                    (dissoc :sku) ; Not needed after this.
                                    (assoc :skus skus)))))))

(reg-sub
 :item/price
 item-signal-fn
 (fn [item {:keys [price-type inherit?] :or {price-type :price}}]
   (price item price-type inherit?)))

(reg-sub
 :item/primary-image
 item-signal-fn
 (fn [item]
   (primary-image item)))

(def sku-signal (partial id-or-new-signal :sku))
(def sku-signal-fn (signal-fn :sku))

(reg-sub-raw
 :sku/name
 (fn [_ [_ id & {:keys [full?]}]]
   (let [sku (sku-signal id)
         sku-name (rxn (u/string-for @sku [:color :size]))]
     (if full?
       (rxn
        (str (:name (listen [:item (:item @sku)])) " " @sku-name))
       sku-name))))

(reg-sub-raw
 :sku/stock
 (fn [_ [_ sku shop]]
   (rxn (db/stock sku shop))))

(reg-sub-raw
 :sku/price
 (fn [_ [_ id & {:keys [price-type inherit?] :or {price-type :price}}]]
   (let [sku (sku-signal id)
         sku-price (rxn (price @sku price-type inherit?))]
     (if inherit?
       (rxn (or @sku-price
                (listen [:item/price (:item @sku) :price-type price-type :inherit? true])))
       sku-price))))

(reg-sub-raw
 :sku/primary-image
 (fn [_ [_ id & {:keys [inherit?]}]]
   (let [sku (sku-signal id)
         sku-primary (rxn (primary-image @sku))]
     (if inherit?
       (rxn (or @sku-primary
                (listen [:item/primary-image (:item @sku)])))
       sku-primary))))

(def category-signal (partial id-or-new-signal :category))

;; Helper for calculating category hierarchy quickly for lots of categories.
(reg-sub-raw
 :categories/index
 (fn []
   (let [categories (p/q '[:find ?eid ?parent-id ?name
                           :where
                           [?e :category/name ?name]
                           [(get-else $ ?e :category/parent :none) ?parent-id]
                           [?e :e/id ?eid]]
                         conn)]
     (rxn
      (->> @categories
           (map (fn [[category parent name]]
                  [category {:parent (when-not (= parent :none) parent)
                             :name name}]))
           (into {}))))))

(reg-sub-raw
 :category/path
 (fn [_ [_ category]]
   (let [index (sub [:categories/index])]
     (rxn
      (->> (iterate #(get @index (:parent %)) (get @index category))
           (take-while (complement nil?))
           reverse
           (map :name)
           (str/join " / " ))))))

;; ;; Not dynamic, p/q wouldn't seem to work with this query. Using `reg-sub-raw` just so it's not dependent on app-db.
;; (reg-sub-raw
;;  :category/hierarchy
;;  (fn [_ [_ root]]
;;    (let [rels
;;          (into {} (datascript.core/q
;;                    '[:find ?child ?parent
;;                      :in $ ?root %
;;                      :where
;;                      (parent-to-child ?root ?parent ?child)]
;;                    @conn [:e/id root]
;;                    '[[(parent-to-child ?c ?category ?ceid)
;;                       [?c :category/parent ?category]
;;                       [?c :e/id ?ceid]]
;;                      [(parent-to-child ?c ?category ?cid1)
;;                       [?c :category/parent ?cid1]
;;                       [?c1 :e/id ?cid1]
;;                       (parent-to-child ?c1 ?category)]]))]
;;      (rxn (->> (iterate #(get rels %) root)
;;                (take-while (complement nil?))
;;                reverse)))))
;;
;; (reg-sub-raw
;;  :category/path
;;  (fn [_ [_ id]]
;;    (time
;;     (let [names (map (fn [eid]
;;                        (datascript.core/pull @conn '[:category/name] [:e/id eid]))
;;                      (listen [:category/hierarchy id]))]
;;       (rxn (str/join " / " (map (fn [name]
;;                                   (:category/name name))
;;                                 names)))))))

(reg-sub
 :category/children
 :<- [:categories]
 (fn [categories [_ category]]
   (u/find {:parent category} categories)))

(reg-sub
 :categories/top-level
 :<- [:categories]
 (fn [categories]
   (sort-by :name (remove :parent categories))))

;; Passing in the stock managemnt entity subscription. Donno if this is a good pattern.
(reg-sub-raw
 :stock-management-items
 (fn [_ [_ entity]]
   (rxn (listen [:items (or (:items @entity) [])])))) ; :items sub will return everything if passed nil. Direct access of :items would be probably be wildly non-performant if it weren't cached.

(def order-signal (partial id-or-new-signal :order))
(def order-signal-fn (signal-fn :order))

(defn order-pieces [skus]
  (apply + (vals skus)))

(reg-sub
 :order/items
 order-signal-fn
 (fn [order]
   (:items order)))

(reg-sub
 :order/skus
 order-signal-fn
 (fn [order]
   (:skus order)))

(reg-sub
 :order/received-skus
 order-signal-fn
 (fn [order]
   (:received-skus order)))

(reg-sub
 :order/sku-qty
 order-signal-fn
 (fn [order [_ _ sku]]
   (get-in order [:skus sku])))

(reg-sub
 :order/unit-cost
 order-signal-fn
 (fn [order [_ _ item]]
   (get-in order [:costs item])))

(reg-sub
 :order/pieces
 order-signal-fn
 (fn [order]
   (order-pieces (:skus order))))

(reg-sub
 :order/received-pieces
 order-signal-fn
 (fn [order]
   (order-pieces (:received-skus order))))

(reg-sub
 :order/item-pieces
 (fn [[_ id item]]
   [(order-signal id)
    (sub [:item/skus item])])
 (fn [[order item-skus]]
   (-> (:skus order)
       (select-keys (map :id item-skus))
       order-pieces)))

(reg-sub
 :order/unit-cost-with-shipping
 (fn [[_ id item]]
   [(order-signal id)
    (sub [:order/unit-cost id item])
    (sub [:order/pieces id])])
 (fn [[order cost total-pieces]]
   (+ (or cost 0)
      (if (zero? total-pieces) ; Prevent divide-by-zero.
        0
        (/ (or (:shipping order) 0)
           total-pieces)))))

(reg-sub
 :order/item-subtotal
 (fn [[_ id item]]
   [(sub [:order/item-pieces id item])
    (sub [:order/unit-cost-with-shipping id item])])
 (fn [[pieces cost]]
   (* pieces cost)))

(reg-sub-raw
 :order/total
 (fn [_ [_ id]]
   (let [items (sub [:order/items id])]
     (rxn (apply + (map (fn [item]
                          (listen [:order/item-subtotal id item]))
                        @items))))))

;;;;;;;;;; TODO GROSS COPYPASTA ;;;;;;;

(reg-sub
 :order/received-item-pieces
 (fn [[_ id item]]
   [(order-signal id)
    (sub [:item/skus item])])
 (fn [[order item-skus]]
   (-> (:received-skus order)
       (select-keys (map :id item-skus))
       order-pieces)))

(reg-sub
 :order/received-unit-cost-with-shipping
 (fn [[_ id item]]
   [(order-signal id)
    (sub [:order/unit-cost id item])
    (sub [:order/received-item-pieces id])])
 (fn [[order cost total-pieces]]
   (+ (or cost 0)
      (if (zero? total-pieces) ; Prevent divide-by-zero.
        0
        (/ (or (:shipping order) 0)
           total-pieces)))))

(reg-sub
 :order/received-item-subtotal
 (fn [[_ id item]]
   [(sub [:order/received-item-pieces id item])
    (sub [:order/received-unit-cost-with-shipping id item])])
 (fn [[pieces cost]]
   (* pieces cost)))

(reg-sub-raw
 :order/received-total
 (fn [_ [_ id]]
   (let [items (sub [:order/items id])]
     (rxn (apply + (map (fn [item]
                          (listen [:order/received-item-subtotal id item]))
                        @items))))))

(reg-sub
 :order/pending-total
 (fn [[_ id]]
   [(sub [:order/total id])
    (sub [:order/received-total id])])
 (fn [[a b]]
   (- a b)))

;;; Not as bad but eh

(reg-sub
 :order/pending-pieces
 (fn [[_ id]]
   [(sub [:order/pieces id])
    (sub [:order/received-pieces id])])
 (fn [[a b]]
   (- a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reg-sub
 :order/has-sku
 (signal-fn :order)
 (fn [order [_ _ sku]]
   (get-in order [:skus sku])))

(reg-sub
 :order/received-qty
 (signal-fn :order)
 (fn [order [_ _ sku]]
   (get-in order [:received-skus sku])))

(reg-sub
 :order/all-received?
 (signal-fn :order)
 (fn [order]
   (= (:skus order) (:received-skus order))))

(reg-sub
 :order/finished?
 (signal-fn :order)
 (fn [order]
   (= (:status order) :finished)))

(defn transfer-pieces [skus]
  (apply + (vals skus)))

(reg-sub
 :transfer/sku-qty
 (signal-fn :transfer)
 (fn [transfer [_ _ sku]]
   (get-in transfer [:skus sku])))

(reg-sub
 :transfer/has-sku
 (signal-fn :transfer)
 (fn [transfer [_ _ sku]]
   (get-in transfer [:skus sku])))

(reg-sub
 :transfer/received-qty
 (signal-fn :transfer)
 (fn [transfer [_ _ sku]]
   (get-in transfer [:received-skus sku])))

(reg-sub
 :transfer/pieces
 (signal-fn :transfer)
 (fn [transfer]
   (transfer-pieces (:skus transfer))))

(reg-sub
 :transfer/received-pieces
 (signal-fn :transfer)
 (fn [transfer]
   (transfer-pieces (:received-skus transfer))))

(reg-sub
 :transfer/value
 (signal-fn :transfer)
 (fn [transfer]
   (apply + (map (fn [[sku qty]]
                   (* (q/sku:price sku) qty))
                 (:skus transfer)))))

(reg-sub
 :transfer/all-received?
 (signal-fn :transfer)
 (fn [transfer]
   (= (:skus transfer) (:received-skus transfer))))

(reg-sub
 :transfer/finished?
 (signal-fn :transfer)
 (fn [transfer]
   (= (:status transfer) :received)))

(def count-signal (partial id-or-new-signal :count))
(def count-signal-fn (signal-fn :count))

(reg-sub
 :count/sku-qty
 count-signal-fn
 (fn [count [_ _ sku]]
   (get-in count [:counted sku])))

(doseq [event-id [:count/expected-qty
                  :count/sku-in-count?]]
  (reg-sub
   event-id
   count-signal-fn
   (fn [count [_ _ sku]]
     (get-in count [:expected sku]))))

(doseq [[event-id attribute] [[:count/num-counted :counted]
                              [:count/num-expected :expected]]]
  (reg-sub
   event-id
   count-signal-fn
   (fn [count]
     (apply + (vals (get count attribute))))))

(reg-sub-raw
 :count/against-items
 (fn [_ [_ id]]
   (let [count (count-signal id)
         counted (rxn (keys (:counted @count)))
         category (rxn (:category @count))
         manufacturer (rxn (:manufacturer @count))
         query (rxn
                (cond
                  ;; Implicit `m/count:manual-adjustment?` logic.
                  (and @category @manufacturer)
                  ['[:find [?e ...]
                     :in $ ?category ?manufacturer
                     :where
                     [?e :item/category ?category]
                     [?e :item/manufacturer ?manufacturer]]
                   @category @manufacturer]
                  @category
                  ['[:find [?e ...]
                     :in $ ?category
                     :where [?e :item/category ?category]]
                   @category]
                  @manufacturer
                  ['[:find [?e ...]
                     :in $ ?manufacturer
                     :where [?e :item/manufacturer ?manufacturer]]
                   @manufacturer]
                  ;; This is a manual adjustment.
                  :else
                  ['[:find [?item ...]
                     :in $ [?sku-id ...]
                     :where
                     [?sku :e/id ?sku-id]
                     [?sku :sku/item ?item-id]
                     [?item :e/id ?item-id]]
                   (or @counted [])]))]
     (apply pull-docs @query))))

(defn delta [counted expected]
  (- (or counted 0) expected))

(reg-sub
 :count/difference
 (fn [[_ id sku]]
   [(sub [:count/sku-qty id sku])
    (sub [:count/expected-qty id sku])])
 (fn [[counted expected]]
   (delta counted expected)))

(reg-sub
 :count/shrinkage-num
 count-signal-fn
 (fn [count]
   (- (apply + (vals (:counted count)))
      (apply + (vals (:expected count))))))

(reg-sub
 :count/shrinkage-amount
 count-signal-fn
 (fn [count]
   (apply + (for [[sku qty] (:expected count)]
              (* (delta (get-in count [:counted sku]) qty)
                 (q/sku:cost sku))))))

;; TODO remove
#_(doseq [attr [:customer/first-name
                :customer/last-name]]
    (reg-sub-raw
     attr
     (fn [_ [_ id]]
       (p/q [:find '?p '.
             :in '$ '?e
             :where ['?e attr '?p]]
            (if (u/new-id? id)
              new-conn conn)
            [:e/id id]))))

(reg-sub
 :customer/name
 (signal-fn :customer)
 (fn [customer]
   (m/customer:name customer)))

(reg-sub-raw
 :customer/credit-account
 (fn [_ [_ customer]]
   (p/q '[:find ?credit-account .
          :in $ ?customer
          :where
          [?e :credit-account/customer ?customer]
          [?e :e/id ?credit-account]]
        conn customer)))

(reg-sub-raw
 :credit-account/activity
 (fn [_ [_ id]]
   (rxn (q/credit-account:activity id))))

(reg-sub-raw
 :credit-account/balance
 (fn [_ [_ id]]
   (rxn (q/credit-account:balance id))))

(reg-sub-raw
 :credit-account/available
 (fn [_ [_ id]]
   (let [balance (sub [:credit-account/balance id])
         limit (rxn (:credit-account/limit @(p/pull conn '[:credit-account/limit] [:e/id id])))]
     (rxn (max 0 (+ @balance @limit))))))

(reg-sub-raw
 :gift-card/activity
 (fn [_ [_ id]]
   (rxn (q/gift-card:activity id))))

(reg-sub-raw
 :gift-card/balance
 (fn [_ [_ id]]
   (rxn (q/gift-card:balance id))))















(reg-sub
 :payment/payments
 (fn [db [_ sale-id]]
   (get-in db [:new-payments sale-id])))

(reg-sub
 :payment/total
 (fn [[_ id]]
   (sub [:payment/payments id]))
 (fn [payments]
   (apply + (vals payments))))

(reg-sub
 :payment/balance
 (fn [[_ id]]
   (sub [:payment/total id]))
 (fn [payment-total [_ _ fill-total]]
   (- fill-total payment-total)))









;; This is "new.sale/" and not just "sale/" in particular because it takes :payment/total as an argument which includes entries being added into the payment modal
(reg-sub
 :new.sale/sufficient-payment
 (fn [[_ id]]
   [(sub [:sale/total id])
    (sub [:payment/total id])])
 (fn [[total payment-total]]
   (<= total payment-total)))









(def sale-signal (partial id-or-new-signal :sale))
(def sale-signal-fn (signal-fn :sale))

(def sales-tax 8)

(defn percent [n pcnt]
  (* n (/ pcnt 100)))

(reg-sub-raw
 :sale/subtotal
 (fn [_ [_ id]]
   (let [sale (sale-signal id)
         refund-amount (rxn (if-let [refund (:refund @sale)]
                              (listen [:refund/amount refund]) 0))]
     (rxn
      (- (apply + (map :price (:lines @sale)))
         @refund-amount)))))

(reg-sub
 :sale/discount-amount
 sale-signal-fn
 (fn [sale]
   (let [subtotal (apply + (map :price (filter #(= :sku (:line-type %)) (:lines sale))))] ; Not having gift cards feel discounts makes the math easier for put-balance-on-gift-card (and similarly refunds too, since those usually go to gift cards) and not having discounts for credit account / gift card refills seems fine in general or at least not likely to come up.
     (percent subtotal (or (:discount sale) 0)))))

(reg-sub
 :sale/tax
 (fn [[_ id]]
   [(sale-signal id)
    (sub [:sale/discount-amount id])])
 (fn [[sale discount-amount]]
   (let [subtotal (apply + (map :price (filter #(= :sku (:line-type %)) (:lines sale)))) ; Credit accounts refills and gift card purchases shouldn't be taxed because the purchases made with them later are.
         subtotal-with-discount (- subtotal discount-amount)]
     (if-not (:no-tax? sale)
       (percent subtotal-with-discount sales-tax)
       0))))

(reg-sub
 :sale/total
 (fn [[_ id]]
   [(sale-signal id)
    (sub [:sale/subtotal id])
    (sub [:sale/tax id])
    (sub [:sale/discount-amount id])])
 (fn [[sale subtotal tax discount-amount]]
   (-> subtotal
       (+ tax)
       (- discount-amount)
       (+ (or (:shipping-cost sale) 0)))))

;; TODO I don't think this logic needs to be a subscription anymore. What is it even for? Maybe just get rid of it
(reg-sub
 :sale/balance
 (fn [[_ id]]
   [(sale-signal id)
    (sub [:sale/total id])])
 (fn [[sale total]]
   (- total
      (apply + (map :transaction/amount (:transactions sale))))))

(reg-sub
 :sale/sku-line-discount
 sale-signal-fn
 (fn [sale [_ _ index]]
   (let [{:keys [sku price]} (get (:lines sale) index)
         sku-price (fetch [:sku/price sku :inherit? true])
         discount (as-> (/ price sku-price) $
                    (- 1 $)
                    (* $ 100)
                    (js/Math.round $)
                    (-> $ (max 0) (min 100)))]
     (when-not (zero? discount) discount))))

(reg-sub
 :sale.bulk/sku-qty
 sale-signal-fn
 (fn [sale [_ _ sku]]
   (let [qty (count (filter #(= (:sku %) sku) (:lines sale)))]
     (when-not (zero? qty) qty))))

(def refund-signal-fn (signal-fn :refund))

(reg-sub
 :refund/amount
 refund-signal-fn
 (fn [refund]
   (let [amount (apply + (map :price (db/refund-expanded-lines (:sale-lines refund))))]
     (+ amount (percent amount sales-tax))))) ; TODO assuming tax here, I should only refund tax on taxed sales/lines

(reg-sub-raw
 :payment-types/sorted
 (fn []
   (let [rels (p/q '[:find ?eid ?name
                     :where
                     [?e :e/type :type/payment-type]
                     [?e :e/id ?eid]
                     [?e :payment-type/name ?name]]
                   conn)]
     (rxn (->> @rels
               (sort (let [cash? #(= % (fixture :fixture/payment-type-cash))]
                       (fn [a b]
                         (let [[a-id a-name] a
                               [b-id b-name] b]
                           (cond
                             (cash? a-id) -1
                             (cash? b-id) 1
                             :else (compare a-name b-name))))))
               (map first))))))

(reg-sub-raw
 :ecom-sales/num-not-shipped
 (fn []
   (p/q '[:find (count ?e) .
          :where [?e :sale/ecom-status :not-shipped]]
        conn)))



;;;; Reports

(reg-sub
 :grouped-report/by
 (fn [db [_ report]]
   (get-in db [:reports report])))

(reg-sub-raw
 :reports/sale-lines
 (fn [_ [_ opts]]
   (rxn (q/sale-lines opts))))

(reg-sub
 :reports/grouped-sales
 (fn [[_ _ opts]]
   (sub [:reports/sale-lines opts]))
 (fn [lines [_ by opts]]
   (q/report:grouped-sales lines by opts)))

(reg-sub
 :reports/grouped-sales-summary
 (fn [[_ _ opts]]
   (sub [:reports/sale-lines opts]))
 (fn [lines [_ _ opts]]
   (q/report:grouped-sales-summary lines opts)))

(reg-sub-raw
 :reports/grouped-assets
 (fn [_ [_ by opts]]
   (rxn (q/report:grouped-assets by opts))))

(reg-sub
 :reports/grouped-assets-summary
 (fn [[_ by opts]]
   (sub [:reports/grouped-assets by opts]))
 (fn [assets]
   (q/report:grouped-assets-summary assets)))

(reg-sub-raw
 :reports/employee-performance
 (fn [_ [_ opts]]
   (rxn (q/report:employee-performance opts))))

(reg-sub-raw
 :reports/stock-history
 (fn [_ [_ opts]]
   (rxn (q/report:stock-history opts))))

(reg-sub-raw
 :reports/register-counts
 (fn []
   (rxn (q/report:register-counts))))

(reg-sub-raw
 :reports/register-count
 (fn [_ [_ reg-count]]
   (let [lines (q/report:register-count reg-count)]
     (rxn (let [amounts (:register-count/amounts @(p/pull conn '[:register-count/amounts] [:e/id reg-count]))]
            (map (fn [line]
                   (let [counted (get amounts (:payment-type line))]
                     (-> line
                         (assoc :counted counted)
                         (assoc :diff (- counted (:total line)))))) ; Li'l duplication of logic here, would be better if this were in a separate function in queries file. ; TODO todo?
                 lines))))))

;; TODO not rerpots but other subs that just wrap queries and aren't dynamic, they're just to cache results. Probably create a helper for this pattern.

(reg-sub-raw
 :sku/average-cost
 (fn [_ [_ sku]]
   (rxn (q/sku:cost sku))))

(reg-sub-raw
 :expected-in-drawer
 (fn [_ [_ register]]
   (rxn (q/expected-in-drawer register))))



;;;; Comms (unlock, clocks, active registers, etc.)

(reg-sub
 :unlock/working?
 (fn [db]
   (:unlock/working? db)))

(reg-sub
 :change-pin/working?
 (fn [db [_ employee]]
   (:change-pin/working? db)))

(reg-sub-raw
 :clock/in?
 (fn [_ [_ employee]]
   (p/q q/clock:in? conn employee)))

(reg-sub-raw
 :clock/in-time
 (fn [_ [_ employee]]
   (p/q '[:find ?in .
          :in $ ?employee
          :where
          [?e :clock/employee ?employee]
          [?e :clock/in ?in]]
        conn employee)))

(reg-sub-raw
 :clocks/employees-in
 (fn []
   (p/q '[:find [?employee ...]
          :where
          [_ :clock/employee ?employee]
          [?e :e/id ?employee]
          (not [?e :archivable/archived? true])]
        conn)))

(reg-sub-raw
 :clocks/employees-out
 (fn []
   (let [in (sub [:clocks/employees-in])
         all (p/q '[:find [?eid ...]
                    :where
                    [?e :e/type :type/employee]
                    (not [?e :archivable/archived? true])
                    [?e :e/id ?eid]]
                  conn)]
     (rxn (difference (set @all) (set @in))))))

(reg-sub
 :clock/working?
 (fn [db [_ employee-or-pin]]
   (get-in db [:clocks/working employee-or-pin])))

(reg-sub-raw
 :active-register/open?
 (fn [_ [_ register]]
   (p/q q/active-register:open? conn register)))

(reg-sub
 :active-register/working?
 (fn [db [_ register]]
   (get-in db [:active-registers/working register])))



;;;; Table

(defn table-signal [id]
  (sub [:table id]))

(def table-signal-fn (fn [[_ id]]
                       (table-signal id)))

(reg-sub
 :table
 (fn [db [_ id]]
   (get-in db [:tables id])))

(doseq [query-id [:params :search :filters :extra-shown?]
        :let [key query-id]]
  (reg-sub
   (keyword :table query-id)
   table-signal-fn
   (fn [table]
     (get table key))))

(reg-sub
 :table/page
 table-signal-fn
 (fn [table]
   (or (:page table) 1)))

(reg-sub
 :table/last-page
 (fn [[_ id source initial-sort search-keyfns]]
   [(sub [:table/total-results id source initial-sort search-keyfns])
    (sub [:table/limit id])])
 (fn [[total limit]]
   (-> total (/ limit) js/Math.ceil)))

(reg-sub
 :table/sort
 table-signal-fn
 (fn [table [_ _ initial-sort]]
   (or (:sort table) initial-sort [m/date :desc])))

(reg-sub
 :table/param-value
 (fn [[_ id]]
   (sub [:table/params id]))
 (fn [params [_ _ param]]
   (get params param)))

(reg-sub
 :table/filter-value
 (fn [[_ id]]
   (sub [:table/filters id]))
 (fn [filters [_ _ filter-fn]]
   (get-in filters [filter-fn :value])))

(reg-sub
 :table/limit
 table-signal-fn
 (fn [table [_ table-id]]
   (or (:limit table) (if (and (vector? table-id) (= (first table-id) :item/skus))
                        99999999 10))))

(def filtered? (some-fn :params :search :filters))

(reg-sub
 :table/filtered?
 table-signal-fn
 (fn [table]
   (filtered? table)))

(reg-sub
 :table/clearable?
 table-signal-fn
 (fn [table]
   (or (filtered? table) (:sort table))))

(defn search [search-keyfns search-value docs]
  (if search-value
    (let [terms (->> search-value
                     str/lower-case
                     (#(str/split % #"\s+"))
                     (remove str/blank?))]
      ;; Doc matches search if every term is in at least one candidate.
      (filter (fn [doc]
                (let [candidates
                      (->> doc
                           ((apply juxt search-keyfns))
                           (remove nil?)
                           (map (fn [v]
                                  (str/lower-case (if (keyword? v)
                                                    (name v) (str v))))))
                      term-has-match?
                      (fn [term]
                        (some #(str/includes? % term) candidates))]
                  (every? term-has-match? terms)))
              docs))
    docs))

(defn perform-sort [[keyfn order] xs]
  (sort-by
   (fn [x]
     (let [v (keyfn x)]
       (if (string? v)
         (-> v str/trim str/lower-case) v)))
   (case order
     :asc compare
     :desc #(compare %2 %1))
   xs))

(reg-sub-raw
 :table/all-results
 (fn [_ [_ id source initial-sort search-keyfns]]
   (let [docs (rxn (cond
                     (fn? source) (listen (source (listen [:table/params id])))
                     ;; (fn? source) (let [fn-res (source (listen [:table/params id]))]
                     ;;                (if (keyword? (first fn-res))
                     ;;                  (listen fn-res)
                     ;;                  fn-res))
                     (keyword? (first source)) (listen source) ; It's a query-v.
                     :else source)) ; It's a collection of documents.
         sorting (sub [:table/sort id initial-sort])
         search-value (sub [:table/search id])
         filters (sub [:table/filters id])]
     (rxn
      (as-> @docs docs
        (if-let [filter-fns (seq (->> @filters vals (map :fn)))]
          (filter (apply every-pred filter-fns) docs)
          docs)
        (search search-keyfns @search-value docs)
        ;; HACK: Messy-ass search-by-sku. This behaves inconsistently in that it doesn't matter what other filters are set, if the search matches a sku code the item will be in the results. Also that it doesn't split the search-value string and consider each peice separately. And finally that it only happens for tables with subs specified here.
        (if (and (or (= source [:items])
                     #_(and (not (fn? source))
                            (keyword? (first source))
                            (= (first source) :whatever)))
                 @search-value)
          (->> (let [sv (str/trim @search-value)]
                 (concat (q/find-items-by-sku-code sv)
                         (q/find-items-by-sku-style-number sv)))
               (remove (fn [more-item]
                         (some #(= more-item (:id %)) docs)))
               (map #(u/strip (datascript.core/pull @conn '[*] [:e/id %])))
               (into docs))
          docs)
        ;; End search-by-sku ------------
        (perform-sort @sorting docs))))))

;; (reg-sub-raw
;;  :table/all-results
;;  (fn [_ [_ id]]
;;    (let [clauses (sub [:table/source id])
;;          sorting (sub [:table/sort id])
;;          search-keyfns (sub [:table/search-keyfns id])
;;          search-value (sub [:table/search id])
;;          filters (sub [:table/filters id])]
;;      ;; TODO how to do sorting (by arbitrary function, by property is straightforward) and filters
;;      ;; Map query structure works! Though only with datascript and not posh.
;;      (let [query (rxn (vec (list* :find '[?e ...]
;;                                   :where (map (fn [clause]
;;                                                 (vec (cons '?e clause)))
;;                                               @clauses))))
;;            docs (p/q @query conn)]
;;        (rxn @docs)))))

(reg-sub
 :table/results
 (fn [[_ id source initial-sort search-keyfns]]
   [(sub [:table/all-results id source initial-sort search-keyfns])
    (sub [:table/page id])
    (sub [:table/limit id])])
 (fn [[all page limit]]
   (->> all
        (drop (* (dec page) limit))
        (take limit))))

(reg-sub
 :table/total-results
 (fn [[_ id source initial-sort search-keyfns]]
   (sub [:table/all-results id source initial-sort search-keyfns]))
 (fn [results]
   (count results)))



;;;; Selects

(doseq [{:keys [name source] :as config}
        [{:name :select/shops
          :source [:shops]
          :all-text "All Shops"}
         {:name :select/registers
          :source (fn [{shop :shop}]
                    [:shop/registers shop])
          :all-text "All Registers"}
         {:name :select/skus
          :source (fn [{item :item}]
                    [:item/skus item])
          :text-fn #(fetch [:sku/name (:id %)])
          :sorting {:comp (u/multi-comp #(compare (:color %1) (:color %2))
                                        #(size-comp (:size %1) (:size %2)))}
          :all-text "All Skus"}
         {:name :select/manufacturers
          :source [:manufacturers]
          :all-text "All Manufacturers"}
         {:name :select/categories
          :source [:categories]
          :text-fn #(fetch [:category/path (:id %)])
          :all-text "All Categories"}
         {:name :select/customer-types
          :source [:customer-types]
          :all-text "All Customer Types"}
         {:name :select/employees
          :source [:employees]
          :text-fn m/employee:name
          :all-text "All Employees"}
         {:name :select/payment-types
          :source [:payment-types]
          :all-text "All Payment Types"}]]
  (reg-sub
   name
   (fn [[_ & args]]
     (sub (if (fn? source)
            (source args)
            source)))
   (fn [docs [_ & {:keys [none? all?]}]]
     (u/select-opts (merge {:items docs
                            :none? none?
                            :all? all?
                            :text-fn :name
                            :value-fn :id
                            :sort? true}
                           config)))))



;;;; Web USB

(reg-sub-raw
 :webusb/device-names
 (fn [app-db]
   ;; Just refresh the devices list every second instead of listening for connect/disconnect events or whatever. I couldn't get the events to fire when I tried.
   (let [query-devices (fn []
                         (webusb/get-devices #(dispatch [:webusb/set-devices %])))
         interval-id (js/setInterval query-devices 1000)]
     (query-devices) ; Up-front call before setInterval kicks in.
     (make-reaction
      (fn [] (:devices @app-db))
      :on-dispose (fn []
                    (js/clearInterval interval-id)
                    (dispatch [:webusb/clear-devices]))))))

;; (reg-sub
;;  :webusb/device-names
;;  :<- [:webusb/devices]
;;  (fn [devices]
;;    (webusb/device-names devices)))



;;;; Miscellaneous

(reg-sub-raw
 :comms/synced?
 (fn [app-db]
   (rxn (and (not (comms/anything-pending?))
             (not= (:modal/name @app-db) :showstopper))))) ; HACK: so users don't think their stuff is saved when the :showstopper modal gets shown on error. An db flag would be better.

(reg-sub
 :filepicker-working?
 (fn [db]
   (:filepicker-working? db)))










(reg-sub-raw
 :commission/leaderboard
 (fn []
   (rxn (q/commission:leaderboard))))

(reg-sub-raw
 :commission/lines
 (fn [_ [_ employee]]
   (rxn (q/commission:lines employee))))

(reg-sub-raw
 :customer/tier
 (fn [_ [_ customer]]
   (rxn (q/customer:tier customer
                         (:customer/assigned-tier @(p/pull conn '[:customer/assigned-tier] [:e/id customer]))))))










(reg-sub
 :passkey
 (fn [db]
   (:passkey db)))

(reg-sub
 :customer-display/passkey-set?
 (fn [db]
   (boolean (:customer-display/passkey db))))

(reg-sub
 :customer-display/state
 (fn [db]
   (:customer-display/state db)))








(reg-sub
 :held-receipts/sorted
 (fn [db]
   (seq (sort-by :e/date #(compare %2 %1) (:held-receipts db)))))








;;;; Some gross `pull-docs` stuff for easy compaability with table `:source`.

(reg-sub-raw
 :customer/sales
 (fn [_ [_ customer]]
   (pull-docs '[:find [?e ...]
                :in $ ?customer
                :where [?e :sale/customer ?customer]]
              customer)))

(reg-sub-raw
 :employee/sales
 (fn [_ [_ employee]]
   (pull-docs '[:find [?e ...]
                :in $ ?employee
                :where [?e :sale/employee ?employee]]
              employee)))

(reg-sub-raw
 :employee/timesheets
 (fn [_ [_ employee]]
   (pull-docs '[:find [?e ...]
                :in $ ?employee
                :where [?e :timesheet/employee ?employee]]
              employee)))

(reg-sub-raw
 :register-count/sales
 (fn [_ [_ reg-count]]
   (pull-docs q/in-register-count-query-sales u/between? reg-count)))

(reg-sub-raw
 :register-count/adjustments
 (fn [_ [_ reg-count]]
   (pull-docs q/in-register-count-query-register-adjustments u/between? reg-count)))

(reg-sub-raw
 :employees/humans
 (fn []
   (pull-docs q/human-employees-query (fixture :fixture/employee-ecom))))








(doseq [query-id [:sidebar-toggled?]]
  (reg-sub
   query-id
   #(get % query-id)))

(doseq [[query-id func] [[:charts/earnings-this-week q/charts:earnings-this-week]
                         [:charts/items-sold-today-by-designer q/charts:items-sold-today-by-designer]]]
  (reg-sub-raw
   query-id
   (fn [_ [_ & args]]
     (rxn (apply func args)))))






(reg-sub
 :installable?
 (fn [db]
   (:install-prompt-event db)))









(reg-sub-raw
 :refunded-sale-lines
 (fn []
   ;; TODO p/q instead
   (rxn (q/all-refunded-sale-lines))))

(reg-sub-raw
 :sale-is-refund-with-refundable-money
 (fn [_ [_ sale-id]]
   (rxn
    (and (q/is-sale-a-refund? (listen [:sale sale-id]))
         (neg? (listen [:sale/total sale-id]))))))










(reg-sub-raw
 :shopify-store
 (fn []
   (p/q q/get-setting-query conn :settings/shopify-store)))

(reg-sub-raw
 :chat-available?
 (fn []
   (p/q q/get-setting-query conn :settings/chat-available?)))
