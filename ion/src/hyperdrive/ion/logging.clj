(ns hyperdrive.ion.logging
  (:require [datomic.ion.cast :as cast]
            [hyperdrive.util :refer [now]]
            [hyperdrive.ion.config :as config]))

(cast/initialize-redirect :stderr) ; From the docs: "There is no redirection when running in Datomic Cloud, so it is ok to leave calls to initialize-redirect in your production code."

(defn- emit
  ([level message] (emit level message nil))
  ([level message data]
   (cast/event (merge {:msg "AppCast"
                       ::level level
                       ::message message
                       ::timestamp (now)
                       ::env (config/get-env)}
                      data))
   ;; "Custom metrics are not supported in the Solo Topology. Calls to cast/metric will have no effect."
   (when-let [name (case level
                     :error :AppCastError
                     :warning :AppCastWarning
                     nil)]
     (cast/metric {:name name :value 1 :units :count}))))

(defn log [level message data & {:keys [frontend?]}]
  (try
    (emit level message (merge {::data data}
                               (when frontend?
                                 {::frontend? true})))
    (catch Exception _
      (try
        (emit :error "Logging error")
        (catch Exception _)))))

;; (def info (partial log :info))
(def warning (partial log :warning))
(def error (partial log :error))
