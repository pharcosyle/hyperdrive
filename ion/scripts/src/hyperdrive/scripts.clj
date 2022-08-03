(ns hyperdrive.scripts
  (:require [clojure-csv.core :as csv]
            [cognitect.aws.client.api :as aws]
            [datomic.client.api :as d]
            [hyperdrive.scripts.migrate :as migrate]
            [hyperdrive.util :as u :refer [gen-id new-entity read-edn-file fixture]]
            [hyperdrive.ion.shopify-integration :as shopify]
            [hyperdrive.ion.store :as store])
  (:import [clojure.lang ExceptionInfo]))


;; TODO opypasta from services.clj

(def ^:private get-aws-client
  (memoize (fn [config]
             (doto (aws/client config)
               (aws/validate-requests true)))))

(defn- throw-on-anomaly! [res msg]
  (when (:cognitect.anomalies/category res)
    (throw (ex-info msg res))))




(def ^:private migration-dir ".migration-output")

(defn- read-migration-file [output-dir f]
  (read-edn-file (str  migration-dir "/" output-dir "/migrate/" f)))

(defn- date-format-today []
  (.format (new java.text.SimpleDateFormat "yyyy-MM-dd") (java.util.Date.))) ; `clj-time` or something would be more idomatic but this is easy.




;;;; Migrate

(defn- default-output-to [label]
  (str migration-dir "/" label "_" (date-format-today)))

(defn migrate! [{:keys [label output-to dump-extra?] :or {output-to (default-output-to label)} :as opts}]
  (migrate/dump (assoc (select-keys opts [:access-token-params :subset])
                       :output-to output-to))
  (when dump-extra?
    (migrate/dump-extra (assoc (select-keys opts [:access-token-params])
                               :output-to output-to)))
  (migrate/migrate-1 output-to)
  (migrate/images-to-cloud output-to)
  (migrate/migrate-2 output-to))

(def ^:private access-token-params
  {:refresh-token "XXXXX"
   :client-id "XXXXX"
   :client-secret "XXXXX"})

(defn migrate-krush! [opts]
  (migrate! (merge {:label "krush"
                    :access-token-params access-token-params}
                   opts)))

(def ^:private privilege-subset
  {:customer-type-id "13"
   :manufacturer-id "267"})

(defn migrate-privilege! [opts]
  (migrate! (merge {:label "privilege"
                    :access-token-params access-token-params
                    :subset privilege-subset}
                   opts)))




;;;; Shopify

(defn shopify-product-csv-initial [migrate-output-dir & {:as opts}]
  (csv/write-csv
   (shopify/product-csv-initial (read-migration-file migrate-output-dir "addendum.edn") opts)))

(defn shopify-product-csv-update [& {:as opts}]
  (csv/write-csv
   (shopify/product-csv-update opts)))

(def shopify-ensure-webhooks! shopify/ensure-webhooks!)

;; For local dev:
;; - `(shopify-ensure-webhooks! "STORE" "ACCESS_KEY" "ASSIGNED.localtunnel.me")`
;; - Run at terminal: npx localtunnel --port 5000




;;;; Env setup

;;; Data

(defn- me []
  (merge (new-entity :type/employee)
         {:employee/email "pharcosyle@gmail.com"
          :employee/role :admin
          :employee/name "The Director"}))

(defn- initial-users [email name]
  [(me) (merge (new-entity :type/employee)
               {:employee/email email
                :employee/role :admin
                :employee/name name})])

(defn fixture-data []
  (let [ecom-shop-id (gen-id)]
    [(merge (new-entity :type/shop)
            {:e/id ecom-shop-id
             :shop/name "eCom Shop"})
     (merge (new-entity :type/register)
            {:e/id (fixture :fixture/register-ecom)
             :register/name "eCom Register"
             :register/shop ecom-shop-id})
     (merge (new-entity :type/employee)
            {:e/id (fixture :fixture/employee-ecom)
             :employee/name "eCommerce"})
     (merge (new-entity :type/payment-type)
            {:e/id (fixture :fixture/payment-type-cash)
             :payment-type/name "Cash"})
     (merge (new-entity :type/payment-type)
            {:e/id (fixture :fixture/payment-type-paypal)
             :payment-type/name "Paypal"})]))

(defn- settings-data [settings]
  (for [[k v] settings]
    {:e/id (gen-id)
     :e/type :type/settings
     :settings/key k
     :settings/value v}))

(def ^:private krush-settings
  (let [contact-email "XXXXXX"]
    {:settings/contact-email contact-email
     :settings/webstore-url "XXXXXXX"
     :settings/logo-image "z8QfTPYiSaaGOd84JRqz"
     :settings/sale-receipt-footer [:span "\nNO REFUNDS! EXCHANGE ONLY" [:br] "\nWITHIN 7-DAYS. " [:br] "\nALL ITEMS MUST HAVE ORIGINAL TAGS ON IT NO EXCEPTIONS!" [:br] "\n(SAME DAY) EXHANGE ONLY ON ALL HATS DUE TO HEALTH & SANITARY REASONS." [:br] "\nNO REFUNDS ON ELECTRONIC PRODUCTS" [:br] "\nEXCHANGE ONLY WITHIN 3-DAYS IF DEFECTIVE::" [:br] "\nLAYAWAY POLICY IS 35% OF ITEM AND EXCLUDES SALE ITEMS" [:br] "\nLAYAWAYS ARE ONLY 14 DAYS AND IS NON-REFUNDABLE" [:br] "\nALL SALES FINAL ON {SALE ITEMS} NO REFUNDS OR EXCHANGES" [:br] "\nALL SALES ARE FINAL ON WHEAT TIMBERLANDS/ NO EXCHANGES" [:br] "\nIF YOU HAVE ANY QUESTIONS OR CONCERNS " [:br] "\nYOU CAN EMAIL US AT " contact-email [:br] "\nALL BLACK FRIDAY SALES ARE FINAL! NO EXCEPTIONS" [:br]]
     :settings/chat-available? true
     ;; TODO
     ;; :settings/shopify-enabled? true
     ;; :settings/shopify-store "krush-exclusive"
     ;; :settings/shopify-access-token "062c0f113ac8d4bfe5ba4e9b69ee63b8"
     ;; :settings/shopify-shared-secret "05fca20dca2fe337f48e5a52801c1305"
     }))

(def ^:private privilege-settings
  (let [contact-email "XXXXX"]
    {:settings/contact-email contact-email
     :settings/webstore-url "XXXXXXXX"
     :settings/logo-image "gQteyhXRtqQy4zf4MrRB"
     :settings/sale-receipt-footer [:span "\nYou can email us at " contact-email [:br]]
     :settings/chat-available? true
     ;; TODO
     ;; :settings/shopify-enabled? true
     ;; :settings/shopify-store "privilege-society"
     ;; :settings/shopify-access-token "" ; TODO
     ;; :settings/shopify-shared-secret "" ; TODO
     }))

(defn- migration-data [migrate-output-dir]
  (let [data (read-migration-file migrate-output-dir "db.edn")]
    (->> data
         (mapcat (fn [[type docs]]
                   (map #(assoc % :e/type type) docs)))
         ;; Add a register per shop for convenience.
         (concat (for [shop (:type/shop data)]
                   (merge (new-entity :type/register)
                          {:register/name "Register 1"
                           :register/shop (:e/id shop)}))))))


;;; Database

(defn- retrying [f failed]
  (loop [attempt 0]
    (let [res (try
                (f)
                (catch ExceptionInfo e
                  e))]
      (when (= (:cognitect.anomalies/category (ex-data res)) :cognitect.anomalies/unavailable)
        (if (< attempt 3)
          (do
            (Thread/sleep (* (inc attempt) 1000))
            (recur (inc attempt)))
          (failed))))))

(defn- setup-db [db-name]
  (if (some #{db-name} (d/list-databases (store/get-client) {}))
    (throw (ex-info "Database already exists." {:db-name db-name}))
    (do
      (d/create-database (store/get-client) {:db-name db-name})
      (retrying
       #(store/xact db-name store/doc-schema)
       #(throw (ex-info "Failed to set up database." {:db-name db-name}))))))

(defn- rm-db [db-name]
  (d/delete-database (store/get-client) {:db-name db-name}))


;;; Parameters

(def ^:private region "us-east-2")
(def ^:private app-name "neutrino")

(defn- get-ssm []
  (get-aws-client {:api :ssm
                   :region region}))

(defn- put-parameter [env key value]
  (throw-on-anomaly!
   (aws/invoke (get-ssm)
               {:op :PutParameter
                :request {:Name (str "/datomic-shared/" (name env) "/" app-name "/" (name key))
                          :Value value
                          :Type "String"
                          :Overwrite true}})
   "Unable to put parameter."))


;;; SES

(defn- get-ses []
  (get-aws-client {:api :sesv2
                   :region "us-east-1"})) ; TODO opypasta from services.clj

(defn create-email-identity [email]
  (when-not (as-> (let [res (aws/invoke (get-ses) {:op :ListEmailIdentities})]
                    (throw-on-anomaly! res "Unable to list email identities.")
                    res) $
              (:EmailIdentities $)
              (first (filter #(= (:IdentityName %) email) $)))
    (throw-on-anomaly!
     (aws/invoke (get-ses)
                 {:op :CreateEmailIdentity
                  :request {:EmailIdentity email}})
     "Unable to create email identity.")))


;;; Import

;; Do importing as described here if there are large volumes in the future: https://docs.datomic.com/cloud/best.html#pipeline-transactions
(defn- import! [db-name entities]
  ;; TODO hopefully temporary, I was getting this error when I tried to do them all at once: "Transaction too large: 12750698 total bytes."
  (doseq [chunk (partition-all 5000 entities)]
    (store/handle-tx! db-name chunk)
    (println (str "Chunk of " (count chunk) " done...")))
  (println (str "Imported " (count entities) " entities.")))


;;; Execute

(defn- setup! [env db-name data contact-email {:keys [store access-token callback-url-base]} old-db-name]
  (setup-db db-name)
  (import! db-name data)
  (put-parameter env :db-name db-name)
  (when contact-email
    (create-email-identity contact-email))
  (when (and store access-token callback-url-base)
    (shopify-ensure-webhooks! store access-token callback-url-base))
  (when old-db-name
    (rm-db old-db-name))
  (println "Setup complete!"))

(defn setup-krush-env! [db-name migration-date old-db-name]
  (setup! :krush
          db-name
          (concat (read-edn-file ".migration-external/krush_employees.edn")
                  (fixture-data)
                  (settings-data krush-settings)
                  (migration-data (str "krush_" (or migration-date (date-format-today)))))
          (:settings/contact-email krush-settings)
          {:store (:settings/shopify-store krush-settings)
           :access-token (:settings/shopify-access-token krush-settings)
           :callback-url-base "https://ei8k4eqo5g.execute-api.us-east-2.amazonaws.com/krush"}
          old-db-name))

(defn setup-privilege-env! [db-name migration-date old-db-name]
  (setup! :privilege
          db-name
          (concat (initial-users "contact@privilegeusa.com" "Mo")
                  (fixture-data)
                  (settings-data privilege-settings)
                  (migration-data (str "privilege_" (or migration-date (date-format-today)))))
          (:settings/contact-email privilege-settings)
          {:store (:settings/shopify-store privilege-settings)
           :access-token (:settings/shopify-access-token privilege-settings)
           :callback-url-base "https://4xga9ae88j.execute-api.us-east-2.amazonaws.com/privilege"}
          old-db-name))

(defn setup-dev-env! [db-name old-db-name]
  (setup! :dev
          db-name
          (concat [(assoc (me) :employee/pin "0000")]
                  (fixture-data)
                  (settings-data (dissoc krush-settings :settings/shopify-enabled?))
                  (->> (read-edn-file "../../team/demo-data.edn") vals (apply concat)))
          nil
          nil
          old-db-name))
