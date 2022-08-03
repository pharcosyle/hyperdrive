(ns hyperdrive.singularity.repl
  (:require [hyperdrive.app.repl :as repl]
            [hyperdrive.singularity.style :refer [rules]]
            [shadow.http.push-state :as shadow-http]))


(defn watch []
  (repl/watch 'hyperdrive.singularity.frontend/bundle-config #'rules))

(defn unwatch []
  (repl/unwatch #'rules))
