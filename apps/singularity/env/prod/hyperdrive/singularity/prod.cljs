(ns hyperdrive.singularity.prod
  (:require [hyperdrive.singularity.core :as core]))

;; Ignore println statements in prod.
(set! *print-fn* (fn [& _]))

(core/init!)
