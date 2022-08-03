(ns hyperdrive.ion.store
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [datomic.client.api :as d]
            [hyperdrive.ion.config :as config]))


(def doc-schema
  [{:db/ident :bogus/id
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :bogus/id-and-attribute
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db/unique :db.unique/identity}
   {:db/ident :bogus/value
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}])





(defn get-db-name []
  (config/get-param :db-name))

(def get-client
  (memoize #(d/client {:server-type :ion
                       :region "us-east-2"
                       :system "exogenesis"
                       ;; :creds-profile "<your_aws_profile_if_not_using_the_default>"
                       :endpoint "http://entry.exogenesis.us-east-2.datomic.net:8182/"
                       :proxy-port 8182})))

(defn get-conn [db-name]
  (d/connect (get-client) {:db-name db-name}))

(defn get-db [db-name]
  (d/db (get-conn db-name)))

(defn get-conn-and-db [db-name]
  (let [conn (get-conn db-name)]
    [conn (d/db conn)]))



(def ^:private delimiter " DELIMITER ")



;; Convenience

(defn xact [db-name tx]
  (d/transact (get-conn db-name) {:tx-data tx}))

(defn- one [db-name eid]
  (d/pull (get-db db-name) '[*] eid))

(defn- boguses->entities [boguses]
  (->> boguses
       (group-by :bogus/id)
       (map (fn [[_ entities]]
              (into {} (for [{:keys [:bogus/id-and-attribute :bogus/value]} entities]
                         [(-> (str/split id-and-attribute (re-pattern delimiter))
                              second
                              edn/read-string)
                          (edn/read-string value)]))))))

(defn all [db-name attr]
  (apply concat
         (d/q '[:find (pull ?e [*])
                :in $ ?attr
                :where [?e ?attr _]]
              (get-db db-name) attr)))

(defn all-of-type [db-name type]
  (boguses->entities
   (apply concat
          (d/q '[:find (pull ?x [*])
                 :in $ ?type
                 :where
                 [?e :bogus/value ?type]
                 [?e :bogus/id ?id]
                 [?x :bogus/id ?id]]
               (get-db db-name) (pr-str type)))))

#_(xact db-name [{:bogus/id "111"
                  :bogus/id-and-attribute (str "111" delimiter ":item/name")
                  :bogus/value "THE hat"}])



(defn handle-tx! [db-name tx]
  (as-> (for [statement tx]
          (if (map? statement)
            (let [entity statement]
              (for [[attribute value] entity]
                {:bogus/id (:e/id entity)
                 :bogus/id-and-attribute (str (:e/id entity) delimiter attribute)
                 :bogus/value (pr-str value)}))
            (let [[operation [_ id] attribute value] statement
                  bogus-id-and-attribute (str id delimiter attribute)]
              (case operation
                :db/add
                [{:bogus/id id
                  :bogus/id-and-attribute bogus-id-and-attribute
                  :bogus/value (pr-str value)}]
                :db.fn/retractAttribute
                (when-let [eid (ffirst (d/q '[:find ?e
                                              :in $ ?id-and-attribute
                                              :where [?e :bogus/id-and-attribute ?id-and-attribute]]
                                            (get-db db-name) bogus-id-and-attribute))]
                  [[:db/retractEntity eid]])
                :db/retractEntity
                (let [eids (apply concat (d/q '[:find ?e
                                                :in $ ?id
                                                :where [?e :bogus/id ?id]]
                                              (get-db db-name) id))]
                  (for [eid eids]
                    [:db/retractEntity eid])))))) $
    (apply concat $)
    ;; Datomic will transact an empty tx and error on a nil tx. Guard against the second scenario and prevent the first while I'm at it.
    (when (seq $)
      (xact db-name $))))

(defn all-entities [db-name]
  (boguses->entities (all db-name :bogus/id)))
