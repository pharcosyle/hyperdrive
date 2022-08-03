(ns hyperdrive.ion.repl
  (:require [clojure.pprint :refer [pprint]]
            [clojure.core.async :refer [put!]]
            [clojure.data.json :as json]
            [datomic.ion.dev :as ion-dev]
            [org.httpkit.server :refer [run-server]]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.reload :refer [wrap-reload]]
            [prone.middleware :refer [wrap-exceptions]]
            [hyperdrive.util :refer [slurp-reader]]
            [hyperdrive.ion.handler :refer [warpgate jumpgate-routes]]
            [hyperdrive.ion.shopify-integration :as shopify]))


;;;; Local servers

(defonce ^:private servers (atom nil))

(defn- wrap-middleware [handler]
  (-> handler
      (wrap-defaults api-defaults)
      wrap-exceptions
      wrap-reload))

(defn- warpgate-handler [req]
  {:body (warpgate
          {:input (-> (slurp-reader (:body req))
                      ;; (json/read-str :key-fn keyword)
                      ;; (assoc :env "dev")
                      ;; json/write-str
                      )})})

(defn- jumpgate-handler [req]
  (jumpgate-routes req #_(assoc-in req [:datomic.ion.edn.api-gateway/data :env] "dev")))

(defn start-servers! []
  (when-not @servers
    (reset! servers [(run-server (wrap-middleware #'warpgate-handler) {:port 4000})
                     (run-server (wrap-middleware #'jumpgate-handler) {:port 5000})])
    (println "Servers started.")))

(defn stop-servers! []
  (when @servers
    (doseq [server @servers]
      (server))
    (reset! servers nil)
    (println "Servers stopped.")))

(defn restart-servers! []
  (stop-servers!)
  (start-servers!))




;;;; Deploy ion

(defn deploy!
  ([group] (deploy! group nil))
  ([group args]
   (as-> (ion-dev/push (or args {})) $
     (do
       (pprint $)
       (ion-dev/deploy (merge (select-keys args [:creds-profile :region :uname])
                              (select-keys $ [:rev])
                              {:group group})))
     (do
       (pprint $)
       (loop []
         (let [status-data (ion-dev/deploy-status (merge (select-keys args [:creds-profile :region])
                                                         (select-keys $ [:execution-arn])))]
           (pprint status-data)
           (when (= (:code-deploy-status status-data) "RUNNING")
             (Thread/sleep 5000)
             (recur))))))))




;;;; Shopify

(defn shopify-start-sync! []
  (when-not @shopify/processing-loop
    (reset! shopify/processing-loop (shopify/create-processing-loop!))
    (println "Loop started.")))

(defn shopify-stop-sync! []
  (when @shopify/processing-loop
    (put! @shopify/processing-loop true)
    (reset! shopify/processing-loop nil)
    (println "Loop stopped.")))

(defn shopify-restart-sync! []
  (shopify-stop-sync!)
  (shopify-start-sync!))
