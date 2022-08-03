(ns hyperdrive.singularity.logging
  (:require [re-frame.db :refer [app-db]]
            [hyperdrive.singularity.services :refer [service]]))

(defn- send
  ([level message] (send level message nil))
  ([level message data]
   (service {:name :service/log
             :payload {:level level
                       :message message
                       :data data}})))

(defn- error-details [e]
  (merge {:message (.-message e)
          :name (.-name e)
          :cause (.-cause ^js e)
          :filename (.-fileName e)
          :line-number (.-lineNumber e)
          :column-number (.-columnNumber e)
          :stack (.-stack e)}
         (when-let [data (ex-data e)]
           {:ex-data (str data)}))) ; Convert to string in case there's stuff in there without a literal reader/writer.

(defn- log [level message details]
  (try
    (let [print-fn (case level
                     :info js/console.log
                     :warning js/console.warn
                     :error js/console.error)
          data (merge {:details (if (instance? js/Error details)
                                  (error-details details)
                                  details)
                       :url js/window.location.href}
                      (select-keys @app-db [:user :page/name :page/args :modal/name]))]
      (print-fn (str "[" (name level) "] " message) data)
      (send level message data))
    (catch :default e
      (try
        (println (str "Logging error: " e))
        (send :error "Logging error")
        (catch :default _)))))

;; (def info (partial log :info))
;; (def warning (partial log :warning))
(def error (partial log :error))
