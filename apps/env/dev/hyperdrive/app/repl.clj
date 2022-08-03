(ns hyperdrive.app.repl
  (:require [hyperdrive.util :refer [require-and-resolve]]
            [hyperdrive.app.build :refer [bundle]]))


(def ^:private watch-key :watch-bundle-config)

(defn watch [bundle-config-sym rules-ref]
  (bundle @(require-and-resolve bundle-config-sym))
  (add-watch rules-ref watch-key (fn [& _]
                                   (bundle @(require-and-resolve bundle-config-sym :reload? true)))))

(defn unwatch [rules-ref]
  (remove-watch rules-ref watch-key))
