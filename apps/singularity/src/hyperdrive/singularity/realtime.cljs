(ns hyperdrive.singularity.realtime
  (:require [cljs.reader :refer [read-string]]
            [re-frame.core :refer [dispatch]]
            ;; ["pusher-js/dist/web/pusher" :as Pusher]
            ))

(declare channel)

(def ^:private event-name "client-customer-display")

(defn init! [passkey]
  ;; (let [pusher (Pusher. (env :pusher-key) (clj->js {:cluster (env :pusher-cluster)}))]
  ;;   (defonce channel (.subscribe pusher (str "private-" passkey))))
  ;; (.bind channel event-name
  ;;        (fn [data]
  ;;          (dispatch (-> data
  ;;                        (js->clj :keywordize-keys true)
  ;;                        :event
  ;;                        read-string))))
  )

(defn trigger [data]
  #_(.trigger channel event-name (->> data
                                      pr-str
                                      (hash-map :event)
                                      clj->js)))




;; TODO with amplify pubsub (is it the same as amazon SNS?), maybe do customer display as a separate app BUT ONLY IF THE WORK HAS A LOT IN COMMON WITH BULK BUY GENERICIZING OF SINGLUARLITY. Review bulk buy app notes first.
;; - search :customer-display? and isCustomerDisplay in project


;; Reference from handler.clj

#_(GET "/customer-display" req (index-handler :customer-display? true))

#_[:body
   [:div#app]
   (when customer-display?
     [:script (str "isCustomerDisplay = true;")])
   (include-js (str "/js/main.js"))]


;; (defn pusher-auth-handler [{:keys [socket_id channel_name]}]
;;   (realtime/authenticate socket_id channel_name))

;; (ns hyperdrive.ion.realtime
;;   (:require [hyperdrive.config :refer [env]])
;;   (:import [com.pusher.rest Pusher]))

;; (declare pusher)

;; (defn init! []
;;   (defonce pusher (doto (Pusher. (env :pusher-app-id) (env :pusher-key) (env :pusher-secret))
;;                     (.setCluster (env :pusher-cluster)))))

;; (defn authenticate [socket-id channel-name]
;;   (.authenticate pusher socket-id channel-name))
