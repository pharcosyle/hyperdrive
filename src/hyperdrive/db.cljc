(ns hyperdrive.db
  (:require [clojure.core.async :refer [chan sliding-buffer put!]]
            [datascript.core :as d]
            [hyperdrive.spec :as spec]
            [hyperdrive.util :as u :refer [fmap]]
            #?@(:clj [[clojure.java.io :as io]
                      [hyperdrive.ion.store :as store]])
            #?@(:cljs [[reagent.core :as r]
                       [posh.reagent :as p]])))

(def schema
  {:e/id {:db/unique :db.unique/value}
   :e/type {:db/index true}
   :category/parent {:db/index true}
   ;; :item/colors {:db/cardinality :db.cardinality/many}
   ;; :item/sizes {:db/cardinality :db.cardinality/many}
   ;; :item/tags {:db/cardinality :db.cardinality/many}
   :sku/item {:db/index true}
   :sku/size {:db/index true}
   :credit-account/customer {:db/unique :db.unique/value}
   :employee/email {:db/unique :db.unique/value}
   :employee/pin {:db/unique :db.unique/value}
   :sale/shopify-order-id {:db/unique :db.unique/identity}
   :clock/employee {:db/unique :db.unique/value}
   :active-register/register {:db/unique :db.unique/value}})




(defn- get-from-stock-map
  ([stock-map key] (get-from-stock-map stock-map key nil))
  ([stock-map key shop]
   (let [shop-map (get @stock-map key)]
     (if shop
       (or (get shop-map shop) 0)
       (apply + (vals shop-map))))))





(def ^:private merge-stock-maps (partial merge-with (partial merge-with (fn [a b]
                                                                          (+ (or a 0) (or b 0))))))

(declare refund-expanded-lines)
(defn- stock-map-for-entity [db e]
  (case (:e/type e)
    :type/sale
    (when-not (= (:sale/ecom-status e) :canceled)
      (let [shop (:register/shop (d/entity db [:e/id (:sale/register e)]))]
        (apply merge-stock-maps (for [sale-line (filter #(= (:line-type %) :sku) (:sale/lines e))]
                                  {(:sku sale-line) {shop -1}}))))
    :type/refund
    (let [shop (:register/shop (d/entity db [:e/id (:refund/register e)]))]
      (apply merge-stock-maps (for [sale-line (refund-expanded-lines (:refund/sale-lines e))]
                                {(:sku sale-line) {shop 1}})))
    :type/order
    (fmap (fn [qty]
            {(:order/shop e) qty})
          (:order/received-skus e))
    :type/transfer
    (fmap (fn [qty]
            {(:transfer/from e) (- qty)
             (:transfer/to e) qty})
          (:transfer/received-skus e))
    :type/count
    (fmap (fn [qty]
            {(:count/shop e) qty})
          (merge-with + (:count/counted e) (fmap - (:count/expected e))))
    nil))

(defn- negate-stock-map [stock-map]
  (fmap #(fmap - %) stock-map))

(defn- pare [pred m]
  (when-let [keyvals (seq (remove (comp pred val) m))]
    (into {} keyvals)))

(defn- stock-map-deltas [diffs]
  (->> diffs
       (fmap #(pare zero? %))
       (pare nil?)))

;; Assumes :sku/item doesn't change but could be easily updated to allow that, just use db-before/db-after for calculating the item deltas like I do for skus in `tx->stock-cache!`
(defn- item-deltas [sku-deltas db]
  (->> sku-deltas
       (map (fn [[sku-id diff]]
              {(:sku/item (d/entity db [:e/id sku-id]))
               diff}))
       (apply merge-stock-maps)
       stock-map-deltas))

(defn deltas->stock-cache! [sku-deltas db stock-cache]
  (when-let [deltas (merge sku-deltas (item-deltas sku-deltas db))]
    (swap! stock-cache merge-stock-maps deltas)))

(defn- safe-pull [db id]
  (try
    (d/pull db '[*] [:e/id id])
    (catch #?(:clj Exception :cljs :default) _)))

(defn- entities-before-and-after [tx {:keys [db-before db-after]}]
  (map (fn [statement]
         (let [id (if (map? statement)
                    (:e/id statement)
                    (let [[_ [_ id]] statement]
                      id))]
           [(safe-pull db-before id)
            (safe-pull db-after id)]))
       tx))

(defn- update-stock-cache! [db before-and-after stock-cache]
  (let [sku-deltas
        (->> before-and-after
             (mapcat (fn [[before after]]
                       [(when before
                          (negate-stock-map (stock-map-for-entity db before)))
                        (when after
                          (stock-map-for-entity db after))]))
             (apply merge-stock-maps)
             stock-map-deltas)]
    (deltas->stock-cache! sku-deltas db stock-cache)))








#?
(:clj
 (do
   (def ^:dynamic conn nil)
   (def ^:dynamic stock-cache nil)
   (def ^:dynamic bots nil)

   (def ^:dynamic records nil)

   ;; TODO If I'm keeping this it should be dynamically rebound like conn, stock-cache, etc.
   (defonce shopify-sync-queue (chan (sliding-buffer 100))) ; Put this here instead of shopify_integration.clj to avoid a circular dependency.



   
   ;; TODO TEMPORARY

   (defonce temp-state
     (memoize (fn [_]
                {:conn (d/create-conn schema)
                 :stock-cache (atom nil)
                 :bots (atom nil)
                 :records (atom [])})))

   (defmacro bind-state [env & body]
     `(binding [conn (:conn (temp-state ~env))
                stock-cache (:stock-cache (temp-state ~env))
                bots (:bots (temp-state ~env))
                records (:records (temp-state ~env))]
        ~@body))
   



   
   (defn- transact! [tx]
     (when-not (spec/valid-tx? @conn tx :whole)
       (throw (ex-info "Invalid tx." {:tx tx})))
     (let [tx-report (d/transact! conn tx)]
       (update-stock-cache! @conn (entities-before-and-after tx tx-report) stock-cache)
       ;; TODO this and also the commented out processing-loop in shopify_integration: (put! shopify-sync-queue [(config/get-env) tx-report])
       ))

   (defn init! []
     (let [entities (store/all-entities (store/get-db-name))]
       (if-let [invalid-entities (seq (remove spec/valid-entity? entities))]
         (throw (ex-info "Entities failed validation during server-side datascript load." {:invalid-entities invalid-entities}))
         ;; TODO make this `log/error` again once I know how to find process output in the logs
         ;; Add back to requires: [hyperdrive.db :as db]
         ;; (log/error "Entities failed validation during server-side datascript load." invalid-entities)
         (do
           (transact! entities)
           (reset! bots (-> (if (< 10000 (count entities)) "bots.edn" "bots_white.edn") ; TODO crazy hack
                            io/resource ; Get rid of clojure.java.io require when this is gone
                            u/read-edn-file))
           (deltas->stock-cache! @bots @conn stock-cache)))))

   (defn save! [tx]
     (transact! tx)
     (store/handle-tx! (store/get-db-name) tx))

   (defn add-record! [tx]
     (swap! records conj {:tx tx
                          :timestamp (u/now)}))

   (defn persist! [tx]
     (save! tx)
     (add-record! tx)))








 :cljs
 (do
   (declare conn)
   (declare new-conn)
   (defonce ^:private stock-cache (r/atom nil))
   (defonce ^:private bots (atom nil))
   (defonce entities-cache (r/atom nil))

   (def bot (partial get-from-stock-map bots))

   (defn- create-entities-cache []
     (into {} (for [type [:type/item :type/customer]]
                [type (->> (d/q '[:find [(pull ?e [*]) ...]
                                  :in $ ?type
                                  :where [?e :e/type ?type]]
                                @conn type)
                           (map (fn [entity]
                                  [(:e/id entity) (u/strip entity)]))
                           (into {}))])))

   (defn- update-entities-cache! [bas]
     (doseq [ba bas
             :let [type (some :e/type ba)
                   eid (some :e/id ba)
                   after (second ba)]]
       (when (some #{type} [:type/item :type/customer])
         (if after
           (swap! entities-cache assoc-in [type eid] (u/strip after))
           (swap! entities-cache u/dissoc-in [type eid])))))

   (defn init! [data]
     (defonce conn (d/conn-from-db (:db data)))
     (defonce new-conn (d/create-conn schema))
     (reset! stock-cache (:stock-cache data))
     (reset! entities-cache (create-entities-cache))
     (reset! bots (:bots data))
     (p/posh! conn)
     (p/posh! new-conn))

   (defn transact! [tx]
     (let [tx-report (p/transact! conn tx)
           bas (entities-before-and-after tx tx-report)]
       (update-stock-cache! @conn bas stock-cache)
       (update-entities-cache! bas)))

   (defn transact-new! [tx]
     (p/transact! new-conn tx))))






(def stock (partial get-from-stock-map stock-cache))














;; TODO temporary until datomic rewrite
(defn refund-expanded-lines [lines]
  (->> (d/q '[:find [?lines ...]
              :where
              [_ :sale/lines ?lines]]
            @conn)
       (apply concat)
       (filter #(some #{(:id %)} lines))))
