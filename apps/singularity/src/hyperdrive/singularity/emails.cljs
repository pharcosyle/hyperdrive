(ns hyperdrive.singularity.emails
  (:require-macros [hiccups.core :refer [html]])
  (:require hiccups.runtime
            [hyperdrive.singularity.templates :as t]))

(defn sale-receipt [sale header footer]
  (html
   (t/sale-receipt
    (merge {:url-type :absolute
            :title "Receipt" ; Probably doesn't matter in an email.
            :sale sale}
           (when header
             {:body-top (list header [:br])})
           (when footer
             {:body-bottom (list [:br] footer)})))))
