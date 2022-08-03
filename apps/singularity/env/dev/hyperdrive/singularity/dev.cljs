(ns ^:dev/once hyperdrive.singularity.dev
  (:require [re-frame.core :as re-frame :refer [dispatch-sync]]
            [re-frisk.core :refer [enable-re-frisk!]]
            [hyperdrive.singularity.core :as core]))

(enable-console-print!)

(enable-re-frisk!)

(defn ^:dev/before-load before-reload []
  (dispatch-sync [:modal/close])) ; After a reload a bootstrap modal will become unstable and the backdrop remains, undismissable.

(defn ^:dev/after-load after-reload []
  (re-frame/clear-subscription-cache!)
  (core/mount-root))

(core/init!)
