(ns hyperdrive.ion.services
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            [cognitect.aws.client.api :as aws]
            [datascript.core :as d]
            [datascript.transit :as dt]
            [org.httpkit.client :as http]
            [hyperdrive.db :as db]
            [hyperdrive.queries :as q]
            [hyperdrive.util :as u :refer [now new-entity chat-url http-error-code?]]
            [hyperdrive.ion.config :as config]
            [hyperdrive.ion.logging :as log])
  (:import [java.io ByteArrayOutputStream]
           [java.util Base64]
           [java.util.zip GZIPOutputStream]))


(defn- gzip [s]
  (with-open [out (ByteArrayOutputStream.)
              gzip (GZIPOutputStream. out)]
    (.write gzip (.getBytes s))
    (.finish gzip)
    (.toByteArray out)))

(defonce ^:private init-dummy-db
  (let [init-dummy-db* (memoize (fn [_]
                                  (do (db/init!) "cool")))]
    (fn []
      (init-dummy-db* (config/get-env))))) ; Remove hyperdrive.ion.config from requires when this is gone

(defn initial-data-handler []
  (init-dummy-db)
  (->> {:timestamp (now) ; TODO when rewriting this it'll probably still be important to know the timestamp of the db value when loading up an empty client, depending on how appsync works (make sure I don't miss any updates between the time loading starts and the time I start receiving appsync tx updates)
        :db (q/initial-db)
        :stock-cache @db/stock-cache
        :bots @db/bots}
       dt/write-transit-str
       gzip
       (.encodeToString (Base64/getEncoder))))

(defn switch-user-handler [pin]
  (pr-str (q/find-employee-by-pin pin)))

(defn persist-handler [tx]
  (db/persist! tx)
  nil)

(defn change-pin-handler [user-email {:keys [current-pin new-pin]}]
  (let [employee (q/find-employee-by-email user-email)
        pin (q/fact employee :employee/pin)]
    (cond
      (and pin (not= pin current-pin)) (pr-str :current-pin-wrong)
      (q/pin-in-use? new-pin) (pr-str :pin-in-use)
      :else (do
              (db/save! [[:db/add [:e/id employee] :employee/pin new-pin]])
              nil))))

(defn clock-in-out-handler [{:keys [how employee-or-pin time]}]
  (let [employee (case how
                   :employee employee-or-pin
                   :pin (q/find-employee-by-pin employee-or-pin))]
    (pr-str
     (if (and (not employee) (= how :pin))
       :no-employee-with-pin
       (let [clock (q/find-clock employee)
             [in-or-out tx]
             (if clock
               [:out [(dissoc (merge (new-entity :type/timesheet)
                                     {:timesheet/employee employee
                                      :timesheet/in (:clock/in clock)
                                      :timesheet/out time})
                              :e/date)
                      [:db/retractEntity [:e/id (:e/id clock)]]]]
               [:in [(dissoc (merge (new-entity :type/clock)
                                    {:clock/employee employee
                                     :clock/in time})
                             :e/date)]])]
         (db/persist! tx)
         {:in-or-out in-or-out
          :employee employee
          :tx tx})))))

(defn open-close-register-handler [{:keys [open-or-close register amounts left-in-drawer employee notes time]}]
  (pr-str
   (case open-or-close
     :open (if (q/find-active-register register)
             :already
             (let [tx [(dissoc (merge (new-entity :type/active-register)
                                      {:active-register/register register
                                       :active-register/employee employee
                                       :active-register/activated time}
                                      (when amounts
                                        {:active-register/amounts amounts}))
                               :e/date)]]
               (db/persist! tx)
               {:tx tx}))
     :close (if-let [active-register (q/find-active-register register)]
              (let [register-count (merge (new-entity :type/register-count)
                                          {:register-count/register register
                                           :register-count/open-employee (:active-register/employee active-register)
                                           :register-count/open-date (:active-register/activated active-register)
                                           :register-count/employee employee}
                                          (when-let [open-amounts (:active-register/amounts active-register)]
                                            {:register-count/open-amounts open-amounts})
                                          (when amounts
                                            {:register-count/amounts amounts})
                                          (when left-in-drawer
                                            {:register-count/left-in-drawer left-in-drawer})
                                          (when notes
                                            {:register-count/notes notes}))
                    tx [register-count
                        [:db/retractEntity [:e/id (:e/id active-register)]]]]
                (db/persist! tx)
                {:register-count-id (:e/id register-count)
                 :tx tx})
              :already))))

(def ^:private get-aws-client
  (memoize (fn [config]
             (doto (aws/client config)
               (aws/validate-requests true)))))

(defn- throw-on-anomaly! [res msg]
  (when (:cognitect.anomalies/category res)
    (throw (ex-info msg res))))

(defn- get-ses []
  (get-aws-client {:api :sesv2
                   :region "us-east-1"}))

(defn send-email-handler [{:keys [to subject html] :or {subject ""}}]
  (throw-on-anomaly!
   (aws/invoke (get-ses)
               {:op :SendEmail
                :request {:FromEmailAddress (q/get-setting :settings/contact-email)
                          :Destination {:ToAddresses [to]}
                          :Content {:Simple {:Subject {:Data subject}
                                             :Body {:Html {:Data html}}}}}})
   "Unable to send email."))

(defn- get-cognito-idp [region]
  (get-aws-client {:api :cognito-idp
                   :region region}))

(defn- pool-and-username [pool-id email]
  {:UserPoolId pool-id
   :Username email})

(defn sign-up-handler [{:keys [pool-id region]} email]
  (throw-on-anomaly!
   (aws/invoke (get-cognito-idp region)
               {:op :AdminCreateUser
                :request (merge (pool-and-username pool-id email)
                                {:DesiredDeliveryMediums ["EMAIL"]
                                 :UserAttributes [{:Name "email"
                                                   :Value email}
                                                  {:Name "email_verified"
                                                   :Value "true"}]})})
   "Unable to create user."))

(defn lock-unlock-handler [{:keys [pool-id region]} {:keys [lock-or-unlock email]}]
  (case lock-or-unlock
    :lock (do
            (throw-on-anomaly!
             (aws/invoke (get-cognito-idp region)
                         {:op :AdminDisableUser
                          :request (pool-and-username pool-id email)})
             "Unable to disable user.")
            (throw-on-anomaly!
             (aws/invoke (get-cognito-idp region)
                         {:op :AdminUserGlobalSignOut
                          :request (pool-and-username pool-id email)})
             "Unable to perform global sign out."))
    :unlock (throw-on-anomaly!
             (aws/invoke (get-cognito-idp region)
                         {:op :AdminEnableUser
                          :request (pool-and-username pool-id email)})
             "Unable to enable user.")))

(defn log-handler [{:keys [level message data]}]
  (log/log level message data :frontend? true)
  nil)




;;;; Chat

(defn chat-auth-handler [user-email]
  (if (q/get-setting :settings/chat-available?)
    (let [chat-api (str chat-url "/api/v1")
          chat-password "omnipass"
          employee (q/entity (q/find-employee-by-email user-email))
          username (-> (:employee/email employee)
                       (str/split #"@")
                       first)
          api-login (fn [continue]
                      (let [res @(http/request
                                  {:url (str chat-api "/login")
                                   :method :post
                                   :body (json/write-str {:username username
                                                          :password chat-password})})
                            body (json/read-str (:body res) :key-fn keyword)]
                        (if-let [e (or (:error res)
                                       (when (not= (:status body) "success")
                                         body)
                                       (when (http-error-code? (:status res))
                                         (str "HTTP Status " (:status res))))]
                          (if continue
                            (continue)
                            (throw (ex-info "Chat API login failed." {:error e})))
                          (pr-str (get-in body [:data :authToken])))))
          api-register (fn []
                         (let [res @(http/request
                                     {:url (str chat-api "/users.register")
                                      :method :post
                                      :body (json/write-str {:username username
                                                             :email (:employee/email employee)
                                                             :pass chat-password
                                                             :name (:employee/name employee)})})
                               body (json/read-str (:body res) :key-fn keyword)]
                           (if-let [e (or (:error res)
                                          (when-not (:success body)
                                            (or (:error body) "No error given."))
                                          (when (http-error-code? (:status res))
                                            (str "HTTP Status " (:status res))))]
                             (throw (ex-info "Chat API register failed." {:error e}))
                             (api-login nil))))]
      (api-login api-register))
    (throw (ex-info "Chat is disabled." {}))))







(defn poll-for-updates [since]
  (pr-str {:timestamp (now)
           :txs (->> @db/records
                     (filter #(< since (:timestamp %)))
                     (map :tx))}))
