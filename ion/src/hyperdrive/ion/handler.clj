(ns hyperdrive.ion.handler
  (:require [clojure.edn :as edn]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [compojure.core :refer [GET POST defroutes context]]
            [datomic.ion.lambda.api-gateway :as apigw]
            [ring.middleware.defaults :refer [wrap-defaults secure-api-defaults]]
            [hyperdrive.db :as db] ; TODO remove once I'm not using `bind-state` any more
            [hyperdrive.util :refer [slurp-reader]]
            [hyperdrive.ion.config :as config]
            [hyperdrive.ion.http-api :as http-api]
            [hyperdrive.ion.services :as svs]))


(defmacro ^:private bind-env-temp [env & body]
  `(config/bind-env
    ~env
    (db/bind-state
     ~env
     ~@body)))



(defn- response [body]
  (json/write-str
   (merge {:status :success}
          (when body
            {:body body}))))

(defn- get-user-email [input]
  (get-in input [:identity :claims :email]))

(defn get-pool-id-and-region [input]
  (let [[_ region pool-id] (re-find #"https\:\/\/cognito\-idp\.(.*)\.amazonaws\.com\/(.*)" (get-in input [:identity :issuer]))]
    {:pool-id pool-id
     :region region}))

(defn warpgate [{raw-input :input}]
  (let [input (json/read-str raw-input :key-fn keyword)]
    (bind-env-temp
     (:env input)
     (let [{:keys [service-name payload]} (-> input
                                              (get-in [:arguments :freight])
                                              edn/read-string)]
       (response
        (case service-name
          :service/initial-data (svs/initial-data-handler)
          :service/switch-user (svs/switch-user-handler payload)
          :service/persist (svs/persist-handler payload)
          :service/change-pin (svs/change-pin-handler (get-user-email input) payload)
          :service/clock-in-out (svs/clock-in-out-handler payload)
          :service/open-close-register (svs/open-close-register-handler payload)
          :service/send-email (svs/send-email-handler payload)
          :service/sign-up (svs/sign-up-handler (get-pool-id-and-region input) payload)
          :service/lock-unlock (svs/lock-unlock-handler (get-pool-id-and-region input) payload)
          :service/log (svs/log-handler payload)
          :service/chat-auth (svs/chat-auth-handler (get-user-email input))
          :service/poll-for-updates (svs/poll-for-updates payload)))))))



(defroutes routes
  (POST "/shopify-webhooks" req (http-api/shopify-webhooks req)))

(defn- safe-slurp-reader [x]
  (when x (slurp-reader x)))

(def jumpgate-routes
  (fn [& [req & handler-args]]
    (bind-env-temp
     (get-in req [:datomic.ion.edn.api-gateway/data :env])
     (apply routes
            (update req :body safe-slurp-reader)
            handler-args))))

(defn wrap-middleware [handler]
  (wrap-defaults handler (assoc-in secure-api-defaults [:security :ssl-redirect] false)))

(def jumpgate (apigw/ionize (wrap-middleware jumpgate-routes)))
