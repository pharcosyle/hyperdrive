(ns hyperdrive.queries
  (:require [clojure.string :as str]
            [datascript.core :as d]
            [hyperdrive.db :as db :refer [conn]]
            #?@(:cljs [[cljs-time.core :refer [today-at-midnight]]
                       [hyperdrive.util :as u :refer [fetch sum-key kmap fmap between? fixture]]
                       [hyperdrive.singularity.models :as m]])))


(let [fact** (fn [db id a]
               ;; I'd like to be doing this with a more idiomatic `pull` but datom access is faster and I want that speed just in case.
               ;; (get (d/pull db (vector a) [:e/id id]) a)
               (-> (d/datoms db :eavt [:e/id id] a) first :v))]
  (defn fact* [db id & as]
    (when id
      (let [v (fact** db id (first as))]
        (if (next as)
          (recur db v (rest as))
          v)))))

(defn fact [id & as]
  (apply fact* @conn id as))

(defn- entity* [db id]
  (d/entity db [:e/id id]))

(defn entity [id]
  (entity* @conn id))

(defn find-by* [db & attrs-and-val]
  (d/q '[:find [?id ...]
         :in $ [?attr ...] ?v
         :where
         [?e ?attr ?v]
         [?e :e/id ?id]]
       db (butlast attrs-and-val) (last attrs-and-val)))

(def find-one-by* (comp first find-by*))

;; Copypasta
(defn item-sku-unique [db item color size]
  (d/q '[:find ?e .
         :in $ ?item ?color ?size
         :where
         [?e :sku/item ?item]
         [?e :sku/color ?color]
         [?e :sku/size ?size]]
       db item color size))




(defn find-skus-by-code* [db code]
  (seq (concat (find-by* db :sku/code (if (= (count code) 13) ; Lightspeed-printed labels have an extra digit for some reason.
                                        (str/join (butlast code))
                                        code))
               (find-by* db :sku/upc :sku/ean :sku/custom-sku-number code))))

(defn find-sales-by-code* [db code]
  (seq (find-by* db :sale/code code)))

(defn find-gift-cards-by-code* [db code]
  (seq (find-by* db :gift-card/code code)))








(def get-setting-query
  '[:find ?v .
    :in $ ?k
    :where
    [?e :settings/key ?k]
    [?e :settings/value ?v]])

(defn get-setting [k]
  (d/q get-setting-query @conn k))







#?
(:clj
 (do
   (defn initial-db []
     @conn)

   (defn find-clock [employee]
     (d/q '[:find (pull ?e [*]) .
            :in $ ?employee
            :where [?e :clock/employee ?employee]]
          @conn employee))

   (defn find-active-register [register]
     (d/q '[:find (pull ?e [*]) .
            :in $ ?register
            :where [?e :active-register/register ?register]]
          @conn register))

   (defn item-skus [item]
     (d/q '[:find [?id ...]
            :in $ ?item
            :where
            [?e :sku/item ?item]
            [?e :e/id ?id]]
          @conn item))

   (defn find-employee-by-pin [pin]
     (find-one-by* @conn :employee/pin pin))

   (def pin-in-use? find-employee-by-pin) ; TODO I feel like there should be a more concise way to see if a datom with a value just exists.

   ;; TODO this probably won't be necessary after I'm done with datomic stuff since :employee/email can be used in a lookup ref.
   (defn find-employee-by-email [email]
     (find-one-by* @conn :employee/email email)))












 :cljs
 (do
   (defn find-by [& attrs-and-val]
     (apply find-by* @conn attrs-and-val))

   (defn find-one-by [& attrs-and-val]
     (apply find-one-by* @conn attrs-and-val))




   (defn eid [id]
     (when id
       (:e/id (d/pull @conn '[:e/id] id))))

   ;; I guess this could just use the `eid` function instad of doing the same logic over again with `pull-many` (assuming there are no nils returned by `eid`).
   (defn eids [ids]
     (map :e/id (d/pull-many @conn '[:e/id] ids)))

   ;; (defn grab-pull [query & params]
   ;;   (let [ids (apply d/q query @conn params)]
   ;;     (map pretty-doc ids)))


   (def item:skus-query '[:find [?e ...]
                          :in $ ?item
                          :where [?e :sku/item ?item]])

   (defn item:skus [item]
     (eids (d/q item:skus-query @conn item)))






   

   (defn all-refunded-sale-lines []
     (->> (d/q '[:find [?lines ...]
                 :where
                 [_ :refund/sale-lines ?lines]]
               @conn)
          (apply concat)))

   (defn sale-line-refunded? [sale-line all-refund-lines]
     (some #{(:id sale-line)} all-refund-lines))




   



   (def new-id-query
     '[:find ?eid .
       :in $ ?type
       :where
       [?e :e/type ?type]
       [?e :e/id ?eid]])








   ;; TODO maybe in db.cljs or wherever the models-for dict goes? Or some sort of logic.cljs?
   (def ^:private permissions
     {:admin
      #{:page/employees
        :page/employee
        :page/employee-timesheets
        :page/employee-sales
        :page/shops
        :page/shop
        :page/payment-types
        :any-settings
        :set-default-cost
        :create-or-change-credit-account
        :change-completed-sale
        :delete-stock-management-entities
        :set-max-commission}
      :manager
      #{:page/new-order
        :page/new-count
        :page/sales
        :page/grouped-sales
        :page/grouped-assets
        :page/employee-performance
        :page/stock-history
        :page/closing-counts
        :page/closing-count
        :page/closing-count-sales
        :page/closing-count-adjustments
        :page/register-adjustments
        :modal/drawer-adjustment
        :any-reports
        :bulk-sale-view}})

   (let [roles-to-permissions
         (->> permissions
              (mapcat (fn [[role actions]]
                        (map (fn [action]
                               [action role])
                             actions)))
              (into {}))]
     (defn auth? [employee action]
       (let [minimum-auth (get roles-to-permissions action :associate)]
         (if (= minimum-auth :associate)
           true
           (let [user-auth (:role (fetch [:employee employee]))
                 level (fn [role]
                         (.indexOf u/roles role))]
             (<= (level minimum-auth) (level user-auth)))))))




   (defn register-chosen? [db]
     (:current-register db))



   (defn page:name [db]
     (:page/name db))



   (defn item:in-stock? [item]
     (pos? (db/stock (:id item))))

   (defn item:has-size? [item size]
     (u/seq-contains? (:sizes item) size))

   (defn item:has-tags? [item tags]
     (u/seq-contains-all? (:tags item) tags))

   (defn within? [field entity [start end]]
     (between? start end (get entity field)))

   ;; Anonymous filter-fns don't work well in tables so I'm just making some named versions here.
   (def date-within? (partial within? :date))
   (def received-on-within? (partial within? :received-on))
   (def expected-on-within? (partial within? :expected-on))
   (def in-within? (partial within? :in))
   #_(defn in-out-within? [{:keys [in out]} [start end]]
       (and (u/after? in start) (u/before? out end)))



   (defn sku:price [sku]
     (or (fact sku :sku/price)
         (fact sku :sku/item :item/price)))

   (defn sku:wholesale-price [sku]
     (or (fact sku :sku/wholesale-price)
         (fact sku :sku/price)
         (fact sku :sku/item :item/wholesale-price)
         (fact sku :sku/item :item/price)))

   (defn sku:default-cost [sku]
     (or (fact sku :sku/default-cost)
         (fact sku :sku/item :item/default-cost)))



   (defn in-shop? [doc shop]
     (= shop (:shop (fetch [:register (:register doc)]))))







   (defn find-skus-by-code [code]
     (find-skus-by-code* @conn code))

   (defn find-sales-by-code [code]
     (find-sales-by-code* @conn code))

   (defn find-gift-cards-by-code [code]
     (find-gift-cards-by-code* @conn code))








   (def ^:private when-input-rules
     '[[(when-input ?e ?attr ?v)
        [(not ?v)]]
       [(when-input ?e ?attr ?v)
        [?e ?attr ?v]]])







   
   (defn- entity->history [e & attrs]
     (as-> e $
       (select-keys $ (concat [:e/id :e/type :e/date] attrs))
       (update $ :e/type #(keyword (name %)))
       (kmap #(keyword (name %)) $)))

   (defn stock-history [opts]
     (let [processors
           (let [{:keys [date-range shop employee]} opts]
             {:sale
              (fn []
                (mapcat
                 (fn [[sale sale-shop]]
                   (let [history (entity->history sale :sale/employee)]
                     (for [[sku qty] (m/sale:cart (:sale/lines sale))]
                       (merge history {:sku sku
                                       :adj (- qty)
                                       :shop sale-shop}))))
                 (d/q '[:find (pull ?e [:e/id :e/type :sale/lines :sale/employee :e/date]) ?sale-shop
                        :in $ ?between-fn ?date-range ?shop ?employee %
                        :where
                        [?e :e/type :type/sale]
                        (when-input ?e :sale/employee ?employee)
                        [?e :e/date ?date]
                        [(?between-fn ?date-range ?date)]
                        [?e :sale/register ?register-id]
                        [?register :e/id ?register-id]
                        (when-input ?register :register/shop ?shop)
                        [?register :register/shop ?sale-shop]
                        (not [?e :sale/ecom-status :canceled])]
                      @conn between? date-range shop employee when-input-rules)))
              :refund
              (fn []
                (mapcat
                 (fn [[refund refund-shop]]
                   (let [history (entity->history refund :refund/employee)]
                     (for [[sku qty] (let [lines (db/refund-expanded-lines (:refund/sale-lines refund))]
                                       (m/sale:cart lines))]
                       (merge history {:sku sku
                                       :adj qty
                                       :shop refund-shop}))))
                 (d/q '[:find (pull ?e [:e/id :e/type :refund/lines :refund/sale-lines :refund/employee :e/date]) ?refund-shop
                        :in $ ?between-fn ?date-range ?shop ?employee %
                        :where
                        [?e :e/type :type/refund]
                        (when-input ?e :refund/employee ?employee)
                        [?e :e/date ?date]
                        [(?between-fn ?date-range ?date)]
                        [?e :refund/register ?register-id]
                        [?register :e/id ?register-id]
                        (when-input ?register :register/shop ?shop)
                        [?register :register/shop ?refund-shop]]
                      @conn between? date-range shop employee when-input-rules)))
              :order
              (fn []
                (mapcat
                 (fn [order]
                   (let [history (entity->history order :order/shop :order/employee)]
                     (for [[sku qty] (:order/received-skus order)]
                       (merge history {:sku sku
                                       :adj qty}))))
                 (d/q '[:find [(pull ?e [:e/id :e/type :order/shop :order/received-skus :order/employee :e/date]) ...]
                        :in $ ?between-fn ?date-range ?shop ?employee %
                        :where
                        [?e :e/type :type/order]
                        (when-input ?e :order/shop ?shop)
                        (when-input ?e :order/employee ?employee)
                        [?e :e/date ?date]
                        [(?between-fn ?date-range ?date)]]
                      @conn between? date-range shop employee when-input-rules)))
              :transfer
              (fn []
                (mapcat
                 (fn [transfer]
                   (let [history (entity->history transfer :transfer/employee)]
                     (remove nil? (for [[direction dir-shop] (select-keys transfer [:transfer/from :transfer/to])
                                        [sku qty] (:transfer/received-skus transfer)] ; In-transit skus will continue to appear in the originating shop. Sent skus that never arrive are unchanged.
                                    (when (or (not shop) (= shop dir-shop))
                                      (merge history {:sku sku
                                                      :adj (let [sign (case direction
                                                                        :transfer/from -
                                                                        :transfer/to +)]
                                                             (sign qty))
                                                      :shop dir-shop}))))))
                 (d/q '[:find [(pull ?e [:e/id :e/type :transfer/from :transfer/to :transfer/received-skus :transfer/employee :e/date]) ...]
                        :in $ ?between-fn ?date-range ?shop ?employee %
                        :where
                        [?e :e/type :type/transfer]
                        (or-from-to ?e ?shop)
                        (when-input ?e :transfer/employee ?employee)
                        [?e :e/date ?date]
                        [(?between-fn ?date-range ?date)]]
                      @conn between? date-range shop employee (concat when-input-rules '[[(or-from-to ?e ?shop)
                                                                                          (when-input ?e :transfer/from ?shop)]
                                                                                         [(or-from-to ?e ?shop)
                                                                                          (when-input ?e :transfer/to ?shop)]]))))
              :count
              (fn []
                (mapcat
                 (fn [count]
                   (let [history (entity->history count :count/shop :count/employee)]
                     (remove nil? (for [[sku qty] (merge-with + (:count/counted count) (fmap - (:count/expected count)))]
                                    (when-not (zero? qty)
                                      (merge history {:sku sku
                                                      :adj qty}))))))
                 (d/q '[:find [(pull ?e [:e/id :e/type :count/shop :count/counted :count/expected :count/employee :e/date]) ...]
                        :in $ ?between-fn ?date-range ?shop ?employee %
                        :where
                        [?e :e/type :type/count]
                        (when-input ?e :count/shop ?shop)
                        (when-input ?e :count/employee ?employee)
                        [?e :e/date ?date]
                        [(?between-fn ?date-range ?date)]]
                      @conn between? date-range shop employee when-input-rules)))})]

       (let [ps (if-let [type (:type opts)]
                  [(get processors type)]
                  (vals processors))]
         (mapcat #(%) ps))))








   (defn sku:average-cost [sku]
     (or (d/q '[:find (avg ?unit-cost) .
                :in $ ?sku ?sku-qty ?item-cost ?order-pieces ?calc
                :where
                [?order :order/received-skus ?skus]
                [(?sku-qty ?skus) ?qty]
                [(get-else $ ?order :order/costs {}) ?costs]
                [?sku :sku/item ?item]
                [(?item-cost ?costs ?item) ?cost]
                [(?order-pieces ?skus) ?pieces]
                [(get-else $ ?order :order/shipping 0) ?shipping]
                [(?calc ?cost ?pieces ?shipping) ?unit-cost]]
              @conn [:e/id sku]
              #(get % sku)
              #(or (get %1 %2) 0)
              #(apply + (vals %))
              (fn [cost pieces shipping]
                (+ cost (if (zero? pieces)
                          0 (/ shipping pieces)))))))

   (defn sku:cost [sku]
     (or (sku:average-cost sku)
         (sku:default-cost sku)))







   (defn- commission-date [{:keys [sku date]}]
     (or (d/q '[:find (max ?d) .
                :in $ ?contains-sku ?before
                :where
                [?e :order/received-on ?d]
                [(< ?d ?before)] ; Ensure that old commissions don't change when new purchase orders come in.
                [?e :order/received-skus ?skus]
                [(?contains-sku ?skus)]]
              @conn #(contains? % sku) date)
         (fact sku :e/date)
         (u/now)))

   (let [months #(* u/one-day 30 %)
         amounts (reverse
                  (array-map
                   0 0.5
                   (months 1) 1
                   (months 3) 1.5
                   (months 6) 2
                   (months 12) 3
                   (months 18) 5
                   (months 24) 7
                   (months 30) 8
                   (months 36) 10))]
     (defn- commission-percentage [sale-line-data]
       (let [commission (let [date (commission-date sale-line-data)]
                          (->> amounts
                               (filter (fn [[threshold _]]
                                         (< date (- (u/now) threshold))))
                               first
                               second))]
         (if-let [max-commission (fact (:sku sale-line-data) :sku/item :item/max-commission)]
           (min commission max-commission)
           commission))))

   (defn commission [{:keys [price] :as sale-line-data}]
     (* price (/ (commission-percentage sale-line-data) 100)))







;;;; Reports

   (defn sale-lines [{:keys [date-range shop category manufacturer employee]}]
     (as-> @conn $
       (d/q {:find [[(list 'pull '?e [:e/id :sale/lines :sale/customer :sale/register :e/date])
                     '...]]
             :where (remove nil? [['?e :e/type :type/sale]
                                  (when shop
                                    ['?e :sale/register '?register-eid])
                                  (when shop
                                    ['?register :e/id '?register-eid])
                                  (when shop
                                    ['?register :register/shop shop])
                                  ;; Ugly but just leave it like this for now. Update: I could do this like I do the :employee-performance query (using `between?` as a query fn).
                                  (when date-range
                                    ['?e :e/date '?date])
                                  (when date-range
                                    (let [[start _] date-range]
                                      (when start
                                        [(list '>= '?date start)])))
                                  (when date-range
                                    (let [[_ end] date-range]
                                      (when end
                                        [(list '< '?date end)])))])}
            $)
       (let [all-refunded-lines (all-refunded-sale-lines)]
         (map (fn [sale]
                (update sale :sale/lines #(remove (fn [line]
                                                    (sale-line-refunded? line all-refunded-lines))
                                                  %)))
              $))
       (mapcat (fn [sale]
                 (for [line (:sale/lines sale)]
                   (assoc line
                          :sale (:e/id sale)
                          :customer (:sale/customer sale)
                          :shop (:shop (fetch [:register (:sale/register sale)]))
                          :date (:e/date sale))))
               $)
       (filter #(= (:line-type %) :sku) $)
       ;; TODO messy and copypasta-ish from the table results sub.
       (if-let [filter-fns (seq (remove nil? [(when category
                                                #(= category (:category (fetch [:item (:item (fetch [:sku (:sku %)]))]))))
                                              (when manufacturer
                                                #(= manufacturer (:manufacturer (fetch [:item (:item (fetch [:sku (:sku %)]))]))))
                                              (when employee
                                                #(= employee (:employee %)))]))]
         (filter (apply every-pred filter-fns) $)
         $)))

   (defn- qty-total-cost-profit [lines]
     (let [total (sum-key :price lines)
           cost (sum-key #(sku:cost (:sku %)) lines)]
       {:qty (count lines)
        :total total
        :cost cost
        :profit (- total cost)}))

   (defn summarize-sale-lines [lines {:keys [rollup rollup-shops]}]
     (let [[lines shop-quantities]
           (if rollup
             (as-> lines $
               (group-by #(:shop (fetch [:register (:register (fetch [:sale (:sale %)]))])) $)
               (if (= rollup :pick-shops)
                 (select-keys $ rollup-shops)
                 $)
               [(apply concat (vals $)) (fmap count $)])
             [lines nil])]
       (let [{:keys [cost profit] :as qtcp} (qty-total-cost-profit lines)]
         (assoc qtcp
                :shop-qty shop-quantities
                :margin (if (zero? cost)
                          0
                          (/ (/ profit cost) 2))))))

   (defn report:grouped-sales [sale-lines by opts]
     (->> sale-lines
          (group-by (case by
                      :sku :sku
                      :category #(:category (fetch [:item (:item (fetch [:sku (:sku %)]))]))
                      :manufacturer #(:manufacturer (fetch [:item (:item (fetch [:sku (:sku %)]))]))
                      :customer :customer
                      :shop :shop
                      :employee :employee))
          (map (fn [[grouping lines]]
                 (assoc (summarize-sale-lines lines opts) :grouping grouping)))))

   (defn report:grouped-sales-summary [sale-lines opts]
     (summarize-sale-lines sale-lines opts))

   (defn report:grouped-assets [by opts] ; opts not used yet
     (as-> @conn $
       (d/q {:find (remove nil? [(list 'pull '?e [:e/id])
                                 (when-not (= by :shop)
                                   '?gid)])
             :where (remove nil? [['?e :e/type :type/sku]
                                  ['?e :sku/item '?iid]
                                  ['?item :e/id '?iid]
                                  (when-not (= by :shop)
                                    (let [attr (case by
                                                 :category :item/category
                                                 :manufacturer :item/manufacturer)]
                                      [(list 'get-else '$ '?item attr :nil) '?gid]))])}
            $)
       (let [shops (map :id (fetch [:shops]))]
         (mapcat (fn [[{:keys [:e/id]} group]]
                   (let [group (when-not (= group :nil) group)]
                     (for [shop (if (= by :shop)
                                  shops
                                  [nil])
                           :let [stock (db/stock id shop)]]
                       {:group (or group shop)
                        :stock stock
                        :sku-cost (* stock (sku:cost id))
                        :sku-sale-value (* stock (sku:price id))})))
                 $))
       (group-by :group $)
       (map (fn [[group members]]
              {:grouping group
               :remaining (sum-key :stock members)
               :total-cost (sum-key :sku-cost members)
               :sale-value (sum-key :sku-sale-value members)})
            $)))

   (defn report:grouped-assets-summary [assets]
     {:remaining (sum-key :remaining assets)
      :total-cost (sum-key :total-cost assets)
      :sale-value (sum-key :sale-value assets)})

   (defn report:employee-performance [{:keys [date-range] :as opts}]
     (let [grouped-lines (group-by :employee (sale-lines opts))
           times
           (->> (concat
                 (d/q '[:find ?employee ?time
                        :in $ ?between-fn ?date-range
                        :where
                        [?e :e/type :type/timesheet]
                        [?e :timesheet/in ?in]
                        [(?between-fn ?date-range ?in)]
                        [?e :timesheet/out ?out]
                        [(- ?out ?in) ?time]
                        [?e :timesheet/employee ?employee]]
                      @conn between? date-range)
                 (d/q '[:find ?employee ?time
                        :in $ ?between-fn ?now ?date-range
                        :where
                        [?e :e/type :type/clock]
                        [?e :clock/in ?in]
                        [(?between-fn ?date-range ?in)]
                        [(- ?now ?in) ?time]
                        [?e :clock/employee ?employee]]
                      @conn between? (u/now) date-range))
                (group-by first)
                (fmap (fn [rs]
                        (apply + (map second rs)))))]
       (for [employee (d/q '[:find [?eid ...]
                             :where
                             [?e :e/type :type/employee]
                             [?e :e/id ?eid]]
                           @conn)]
         (let [lines (get grouped-lines employee)
               {:keys [qty total profit] :as qtcp} (qty-total-cost-profit lines)
               time (get times employee)
               hours (when time (/ time u/one-hour))]
           (assoc (select-keys qtcp [:qty :total :profit])
                  :employee employee
                  :worked (or time 0)
                  :items-per-hour (when time (/ qty hours))
                  :sales-per-hour (when time (/ total hours))
                  :profit-per-hour (when time (/ profit hours))
                  :commission (sum-key commission lines))))))

   (def report:stock-history stock-history)

   (defn item:stock-history [{:keys [skus date-range type shop employee] :as opts}]
     (as-> (when-not (= type :bot)
             (stock-history opts)) $
       (filter #(some #{(:sku %)} skus) $)
       (if (and (not (first date-range))
                (or (not type) (= type :bot))
                (not employee))
         (concat (keep (fn [sku]
                         (let [adj (db/bot sku shop)] ; When no shop filter is set the bot entries show up grouped together. A little inconsistent but not a big deal.
                           (when-not (zero? adj)
                             {:type :bot
                              :adj adj
                              :shop shop
                              :sku sku})))
                       skus) $)
         $)
       (sort-by :date $) ; Docs have to be date-sorted in order to calculate the running-total, even if they're later sorted into another order in the table.
       (reduce (fn [history event]
                 (conj history (assoc event :running-total (+ (or (:running-total (last history)) 0) ; No history entries yet for first reduction.
                                                              (:adj event)))))
               [] $)
       (map-indexed (fn [idx event]
                      (assoc event :order idx)) $)))







   ;; TODO merge this with the reg-count-line logic. There some more in the subs that use this too. The only differences between these two is the :sale/register and :register-adjustment/register line. Make the logic for the register clause is below the time range clause (which will be much more narrowing in the long run) and do it for reg-count-line too! Update: ideally I'd like to make a reusable rule or something too that does the between-open-close logic and cleans up some repeated code. Wait for the datomic rewrite before considering this. Wait maybe I should just run that bit once and pass the result into every query that needs it?
   (def in-register-count-query-sales '[:find [?e ...]
                                        :in $ ?between-fn ?reg-count
                                        :where
                                        [?r :e/id ?reg-count]
                                        [?r :register-count/open-date ?open]
                                        [?r :e/date ?close]
                                        [?r :register-count/register ?register]
                                        [?e :sale/register ?register]
                                        [?e :e/date ?date]
                                        [(?between-fn ?open ?close ?date)]])
   (def in-register-count-query-register-adjustments '[:find [?e ...]
                                                       :in $ ?between-fn ?reg-count
                                                       :where
                                                       [?r :e/id ?reg-count]
                                                       [?r :register-count/open-date ?open]
                                                       [?r :e/date ?close]
                                                       [?r :register-count/register ?register]
                                                       [?e :register-adjustment/register ?register]
                                                       [?e :e/date ?date]
                                                       [(?between-fn ?open ?close ?date)]])

   (defn- reg-count-amounts [reg-count payment-type attr]
     (let [amounts (fact reg-count attr)]
       (if payment-type
         (get amounts payment-type)
         (apply + (vals amounts)))))

   (declare sale-cash-change)

   (defn- reg-count-line [reg-count payment-type]
     (let [open (reg-count-amounts reg-count payment-type :register-count/open-amounts)
           counted (reg-count-amounts reg-count payment-type :register-count/amounts)
           payments (let [paid (d/q '[:find (sum ?payment) .
                                      :with ?sale
                                      :in $ ?payment-in-reg-count ?reg-count
                                      :where
                                      [?e :e/id ?reg-count]
                                      [?e :register-count/open-date ?open]
                                      [?e :e/date ?close]
                                      [?e :register-count/register ?register]
                                      [?sale :sale/register ?register] ; TODO payments might come from other registers than the sale one (i.e. layaway payments) so when rewriting this after datomic rewrite use :transaction/register instead.
                                      [?sale :sale/transactions ?sale-payments]
                                      [(?payment-in-reg-count ?sale-payments ?open ?close) ?payment]]
                                    @conn
                                    (fn [payments open close]
                                      (as-> payments $
                                        (filter #(between? open close (:e/date %)) $)
                                        (if payment-type
                                          (filter #(= (:transaction/source %) payment-type) $)
                                          (remove #(= (:transaction/line-type %) :type/exchange) $))
                                        (map :transaction/amount $)
                                        (apply + $)
                                        (when-not (zero? $) $)))
                                    reg-count)
                          ;; TODO `refunded` query is 100% copypasta of `paid` with just :sale/register and :sale/transactions changed to :refund/register and :refund/transctions. Also it assumes that the transactions will have :e/date on them but I'm considering removing it since they'll always be the date of the refund for refund transactions.
                          refunded (d/q '[:find (sum ?payment) .
                                          :with ?sale
                                          :in $ ?payment-in-reg-count ?reg-count
                                          :where
                                          [?e :e/id ?reg-count]
                                          [?e :register-count/open-date ?open]
                                          [?e :e/date ?close]
                                          [?e :register-count/register ?register]
                                          [?sale :refund/register ?register]
                                          [?sale :refund/transactions ?sale-payments]
                                          [(?payment-in-reg-count ?sale-payments ?open ?close) ?payment]]
                                        @conn
                                        (fn [payments open close]
                                          (as-> payments $
                                            (filter #(between? open close (:e/date %)) $)
                                            (if payment-type
                                              (filter #(= (:transaction/source %) payment-type) $)
                                              (remove #(= (:transaction/line-type %) :type/exchange) $))
                                            (map :transaction/amount $)
                                            (apply + $)
                                            (when-not (zero? $) $)))
                                        reg-count)
                          ;; TODO end copypasta -----------------
                          cash-change (if (or (not payment-type)
                                              (= payment-type (fixture :fixture/payment-type-cash)))
                                        (d/q '[:find (sum ?cash-change) .
                                               :with ?sale
                                               :in $ ?between-fn ?sale-cash-change ?reg-count
                                               :where
                                               ;; TODO duplicated from in-register-count-query-sales above ------
                                               [?r :e/id ?reg-count]
                                               [?r :register-count/open-date ?open]
                                               [?r :e/date ?close]
                                               [?r :register-count/register ?register]
                                               [?sale :sale/register ?register]
                                               [?sale :e/date ?date]
                                               [(?between-fn ?open ?close ?date)]
                                               ;; TODO end duplication --------
                                               [?sale :e/id ?sale-id]
                                               [(?sale-cash-change ?sale-id) ?cash-change]]
                                             @conn between? sale-cash-change reg-count))]
                      (- paid refunded (or cash-change 0)))
           adjustments (d/q '[:find (sum ?amount) .
                              :with ?reg-adj
                              :in $ ?between-fn ?amount-fn ?reg-count ?payment-type %
                              :where
                              [?e :e/id ?reg-count]
                              [?e :register-count/open-date ?open]
                              [?e :e/date ?close]
                              [?e :register-count/register ?register]
                              [?reg-adj :register-adjustment/register ?register]
                              [?reg-adj :e/date ?date]
                              [(?between-fn ?open ?close ?date)]
                              (when-input ?reg-adj :register-adjustment/payment-type ?payment-type)
                              [?reg-adj :register-adjustment/type ?reg-adj-type]
                              [?reg-adj :register-adjustment/amount ?abs-amount]
                              [(?amount-fn ?reg-adj-type ?abs-amount) ?amount]]
                            @conn
                            between?
                            (fn [reg-adj-type amount]
                              ((case reg-adj-type :add + :payout -) amount))
                            reg-count payment-type when-input-rules)
           total (+ open payments adjustments)
           diff (- counted total)]
       {:open open
        :payments payments
        :adjustments adjustments
        :total total
        :counted counted
        :diff diff}))

   (defn report:register-counts []
     (map (fn [reg-count]
            (merge (kmap #(keyword (name %)) reg-count)
                   (reg-count-line (:e/id reg-count) nil)))
          (d/q '[:find [(pull ?e [:e/id
                                  :register-count/register
                                  :register-count/employee
                                  :register-count/open-date
                                  :e/date])
                        ...]
                 :where [?e :e/type :type/register-count]]
               @conn)))

   (defn report:register-count [reg-count]
     (map (fn [payment-type]
            (assoc (reg-count-line reg-count payment-type) :payment-type payment-type))
          (eids (d/q '[:find [?e ...]
                       :where [?e :e/type :type/payment-type]]
                     @conn))))







   (def clock:in?
     '[:find ?e .
       :in $ ?employee
       :where [?e :clock/employee ?employee]])

   (def active-register:open?
     '[:find ?e .
       :in $ ?register
       :where [?e :active-register/register ?register]])







   (defn items-for-skus [ids]
     (distinct (map #(fact % :sku/item) ids)))

;;;; Messy-ass search-by-sku

   (defn find-items-by-sku-code [code]
     (items-for-skus (find-skus-by-code code)))

   (defn find-items-by-sku-style-number [style-number]
     (items-for-skus (find-by :sku/style-number style-number)))






   ;; Inconsistent results if multiple purchase orders have the exact same date. Would be easy to fix this but it's unlikely to come up.
   (defn order:num-sold [id]
     (let [order (entity id)
           skus (keys (:order/received-skus order))]
       (as-> (d/q '[:find [?s ...]
                    :in $ ?order-date
                    :where
                    [?e :e/type :type/order]
                    [?e :e/date ?date]
                    [(<= ?order-date ?date)]
                    [?e :order/received-skus ?s]]
                  @conn (:e/date order)) $
         (map #(select-keys % skus) $)
         (let [stock (zipmap skus (map #(- (db/stock %) (db/bot %)) skus))]
           (conj $ (fmap - stock)))
         (apply merge-with (fnil + 0 0) $)
         (into {} (map (fn [[sku n]]
                         [sku (if (neg? n)
                                0 (let [max (get (:order/received-skus order) sku)]
                                    (if (< max n)
                                      max n)))])
                       $))
         (apply + (vals $)))))







   (def human-employees-query
     ;; I couldn't figure out a way to do this with an arbitrary number of arguments.
     '[:find [?e ...]
       :in $ ?fixture-employee          ; ?fixture-employee-2
       :where
       [?e :e/type :type/employee]
       (not [?e :archivable/archived? true]) ; TODO Put his here since anywhere I want human employees I also want non-archived. Do this better.
       (not [?e :e/id ?fixture-employee])
       ;; (not [?e :e/id ?fixture-employee-2])
       ])









   (defn- commission-line [date sale-id {:keys [sku price]}]
     (let [p (commission-percentage {:sku sku :date date})]
       {:sku sku :sale sale-id :date date :percentage p :commission (* (/ p 100) price)}))

   ;; TODO super temporary
   (defn- refund-commission-lines [employee]
     (->> (d/q '[:find ?lines ?date
                 :where
                 [?e :refund/sale-lines ?lines]
                 [?e :e/date ?date]]
               @conn)
          (map (fn [[lines date]]
                 [(db/refund-expanded-lines lines) date]))
          (mapcat (fn [[lines date]]
                    (map #(assoc % :date date) lines)))
          (filter #(= (:employee %) employee))
          (map (fn [x]
                 (update (commission-line (:date x) nil x) :commission -))))) ; TODO get sale-id instead of passing nil

   (defn commission-from-refunds [employee]
     (sum-key :commission (refund-commission-lines employee)))



   (defn- beginning-of-month []
     (let [now (js/Date.)]
       (.getTime (js/Date. (.getFullYear now) (.getMonth now)))))

   (defn commission:leaderboard []
     (let [commissions (->> (d/q '[:find ?lines ?date
                                   :in $ ?since
                                   :where
                                   [?e :e/type :type/sale]
                                   [?e :e/date ?date]
                                   [(> ?date ?since)]
                                   [?e :sale/lines ?lines]]
                                 @conn (beginning-of-month))
                            (mapcat (fn [[lines date]]
                                      (->> lines
                                           (filter #(= (:line-type %) :sku))
                                           (map #(assoc % :date date)))))
                            (reduce (fn [r line]
                                      (update r (:employee line) (fnil + 0) (commission line)))
                                    {}))]
       (->> (for [employee (eids (d/q human-employees-query @conn (fixture :fixture/employee-ecom)))]
              {:employee employee
               :commission (or (+ (get commissions employee) (commission-from-refunds employee)) 0)}) ; TODO The refunds since the beginning of the month not all of them
            (sort-by :commission #(compare %2 %1))
            (map-indexed (fn [idx entry]
                           (assoc entry :rank (inc idx)))))))

   (defn commission:lines [employee]
     (concat
      (mapcat
       (fn [{:keys [:e/id :sale/lines :e/date]}]
         (->> lines
              (filter #(= (:line-type %) :sku))
              (filter #(= (:employee %) employee))
              (map (partial commission-line date id))))
       (d/q '[:find [(pull ?e [:e/id :sale/lines :e/date]) ...]
              :where [?e :e/type :type/sale]]
            @conn employee))
      (refund-commission-lines employee)))









   ;; Rough calculation of points for top tier.
   ;; (as-> (for [n (range 1 11)]
   ;;         (* 800 (/ n 10))) $
   ;;   ;; Given: one $800 purchase every month should qualify a customer for the top tier. The percentage drops about 10% a month.
   ;;   (apply + $)
   ;;   ;; There's a 20 point bonus per sale to factor in frequency of purchases, subtract that.
   ;;   (- $ (* 20 10)))
   ;; ;; 4200
   (def ^:private top-tier-points 4200)
   (def ^:private num-tiers (count u/tiers))
   (def ^:private sale-point-bonus 20)

   (defn- tier-sale-percentage [d]
     (let [ago (- (u/now) d)
           zero-level (* u/one-day 30 18) ; 18 months.
           pcnt (inc (- (/ ago zero-level)))]
       (max pcnt 0)))

   (defn- tier-sale-points [lines d]
     (+ (* (sum-key :price lines) (tier-sale-percentage d)) sale-point-bonus))

   (defn- tier-customer-points [customer]
     (d/q '[:find (sum ?points) .
            :with ?e
            :in $ ?calc ?customer
            :where
            [?e :sale/customer ?customer]
            ;; TODO After datomic rewrite write some logic preventing refunds from contributing to cutstomer tier points
            ;; [(get-else $ ?e :sale/refund-for :nil) ?refund]
            ;; [(= ?refund :nil)]
            [?e :e/date ?d]
            [?e :sale/lines ?lines]
            [(?calc ?lines ?d) ?points]]
          @conn tier-sale-points customer))

   (defn- tier-find [points]
     (loop [tier num-tiers]
       (if (>= points (* top-tier-points (/ tier num-tiers))) ; `>=` so that 0 = 0 for the termination case.
         (if (< tier 1) 1 tier) ; "Less than one" just in case there's somehow a negative point value. Also a fencepost situation, just make 0 and 1 the bottom tier.
         (recur (dec tier)))))

   (defn customer:tier
     ([customer] (customer:tier customer (fact customer :customer/assigned-tier)))
     ;; Two-arg version for injected `assigned-tier` in subscriptions.
     ([customer assigned-tier]
      (max (or assigned-tier 0) (tier-find (tier-customer-points customer)))))

   (defn customer:tier-discount [customer]
     (-> (customer:tier customer (fact customer :customer/assigned-tier))
         (- 1)
         (* 3)))







   (defn customer:discount [customer]
     (max (or (fact customer :customer/type :customer-type/discount) 0)
          (customer:tier-discount customer)))







   (defn find-customer-by-email [email]
     (find-one-by :customer/email :customer/email2 email))








   (defn- account-activity [type id]
     (let [account (entity id)]
       (as-> (d/q '[:find (pull ?e [:e/id :sale/code :e/date]) ?activity
                    :in $ ?filter-activity
                    :where
                    [?e :sale/lines ?lines] ; For some reason this doesn't work if I have a get-else here which I wanted to do for safety even though currently :sale/lines is required. Doing get-else but having `[?e :e/type :type/sale]` as the first clause works too.
                    [(get-else $ ?e :sale/transactions []) ?payments]
                    [(?filter-activity ?lines ?payments) ?activity]]
                  @conn
                  (fn [lines payments]
                    (let [charges (seq (keep (fn [line]
                                               (when (and (= (:line-type line) (keyword (name type)))
                                                          (case type
                                                            :type/credit-account (= (:credit-account line) id)
                                                            :type/gift-card (= (:code line) (:gift-card/code account))))
                                                 (:price line)))
                                             lines))
                          usages (seq (keep (fn [payment]
                                              (when (and (= (:transaction/line-type payment) type)
                                                         (= (:transaction/source payment) id))
                                                (:transaction/amount payment)))
                                            payments))]
                      (when (or charges usages)
                        {:charge charges
                         :usage usages})))) $
         (mapcat (fn [[sale activity]]
                   (mapcat (fn [[action amounts]]
                             (map (fn [amount]
                                    {:action action
                                     :amount ((case action :charge + :usage -) amount)
                                     :sale-id (:e/id sale)
                                     :code (:sale/code sale)
                                     :date (:e/date sale)})
                                  amounts))
                           activity))
                 $)
         (if-let [bot-balance (case type
                                :type/credit-account (:credit-account/bot-balance account)
                                :type/gift-card (:gift-card/bot-balance account))]
           (conj $ {:action :bot
                    :amount bot-balance})
           $))))

   (def credit-account:activity (partial account-activity :type/credit-account))
   (def gift-card:activity (partial account-activity :type/gift-card))

   (defn- account-balance [type id]
     (apply + (map :amount (account-activity type id)))) ; This could obviously be made more efficient by not relying on `account-activity`.

   (def credit-account:balance (partial account-balance :type/credit-account))
   (def gift-card:balance (partial account-balance :type/gift-card))








   (defn expected-in-drawer [register]
     (let [db @conn
           date-of-newest (d/q '[:find (max ?d) .
                                 :in $ ?register
                                 :where
                                 [?e :register-count/register ?register]
                                 [?e :e/date ?d]]
                               db register)]
       (d/q '[:find ?left .
              :in $ ?date-of-newest
              :where
              [?e :register-count/register ?register]
              [?e :e/date ?date-of-newest]
              [?e :register-count/left-in-drawer ?left]]
            db date-of-newest)))








   (defn charts:earnings-this-week []
     (let [today (today-at-midnight)]
       (as-> (map #(- (today-at-midnight)
                      (* % u/one-day))
                  (range 7)) $
         (reverse $)
         (map (fn [day]
                [(u/date day :month-and-day)
                 (->> (d/q '[:find [?lines ...]
                             :in $ ?between-fn ?date-range
                             :where
                             [?e :e/type :type/sale]
                             [?e :e/date ?date]
                             [(?between-fn ?date-range ?date)]
                             [?e :sale/lines ?lines]]
                           @conn between? [day (+ day u/one-day)])
                      (map (fn [lines]
                             (->> lines
                                  (filter #(= (:line-type %) :sku))
                                  (sum-key :price))))
                      (apply +))]) $)
         {:labels (map first $)
          :data (map second $)})))
   
   (defn charts:items-sold-today-by-designer [customer]
     (as-> (d/q '[:find [?lines ...]
                  :in $ ?since
                  :where
                  [?e :e/type :type/sale]
                  [?e :e/date ?date]
                  [(> ?date ?since)]
                  [?e :sale/lines ?lines]]
                @conn (today-at-midnight)) $
       (mapcat (fn [lines]
                 (->> lines
                      (filter #(= (:line-type %) :sku))
                      (map #(fact (:sku %) :sku/item :item/manufacturer)))) $)
       (frequencies $)
       {:labels (map #(fact % :manufacturer/name) (keys $))
        :data (vals $)}))









   ;; TODO temporarly, for sanity
   (defn is-sale-a-refund? [sale]
     (:refund sale))
   (defn sale-cash-change [sale-id]
     (- (->> (fetch [:sale sale-id])
             :transactions
             (remove #(= (:transaction/line-type %) :type/exchange))
             (map :transaction/amount)
             (apply +))
        (max 0 (fetch [:sale/total sale-id]))))))
