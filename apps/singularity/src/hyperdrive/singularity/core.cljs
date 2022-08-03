(ns hyperdrive.singularity.core
  (:require [reagent.dom :as dom]
            [re-frame.core :refer [dispatch dispatch-sync]]
            [hyperdrive.singularity.views :as views]
            hyperdrive.singularity.events
            hyperdrive.singularity.subs))

(defn mount-root []
  (dom/render [views/main] (.getElementById js/document "app")))

(defn init! []
  (dispatch-sync [:auth/init])
  (dispatch [:navigation/configure])
  (mount-root)
  (dispatch [:init-window]))
