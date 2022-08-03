(ns hyperdrive.singularity.frontend
  (:require [hiccup.page :refer [include-js include-css html5]]
            [hyperdrive.singularity.style :refer [styles]]))


(def ^:private node-modules "node_modules/")

(def bundle-config
  {:index-page
   (html5
    [:head
     [:title "Hyperdrive"]
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no"}]
     (include-css "https://fonts.googleapis.com/css?family=Nunito:200,200i,300,300i,400,400i,600,600i,700,700i,800,800i,900,900i")
     (include-css "/css/main.css")
     ;; [:link {:rel "manifest" :href "/manifest.json"}]
     ;; ;; iOS meta tags and icons.
     ;; [:meta {:name "apple-mobile-web-app-capable" :content "yes"}]
     ;; [:meta {:name "apple-mobile-web-app-status-bar-style" :content "black"}]
     ;; [:meta {:name "apple-mobile-web-app-title" :content "Hyperdrive"}]
     ;; [:link {:rel "apple-touch-icon" :href "/favicon.ico"}]
     ]
    [:body
     [:div#app]
     (include-js "/js/shared.js")
     (include-js "/js/main.js")])
   
   :app-styles
   styles

   :stylesheets
   (map
    #(str node-modules %)
    ["@fortawesome/fontawesome-free/css/all.css"
     "startbootstrap-sb-admin-2/css/sb-admin-2.css"
     "bootstrap4-tagsinput-douglasanpa/tagsinput.css"
     "flatpickr/dist/flatpickr.css"
     "@aws-amplify/ui/dist/style.css"])

   :assets
   [[(str node-modules "@fortawesome/fontawesome-free/webfonts") "webfonts"]]})
