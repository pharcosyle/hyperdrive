(ns hyperdrive.app.build
  (:require [clojure.string :as str]
            [clojure.java.io :refer [make-parents]]
            [clojure.java.shell :refer [sh]]
            [me.raynes.fs :as fs]
            [shadow.cljs.devtools.api :as shadow]
            [hyperdrive.util :refer [require-and-resolve]]))


(def ^:private public-out-dir "out/public")

(defn bundle [{:keys [index-page app-styles stylesheets assets]} & {:keys [minify-css?]}]
  (let [dir public-out-dir]
    (fs/mkdirs dir)
    (spit (str dir "/index.html") index-page)
    (let [f (str dir "/css/main.css")
          css (as-> (map slurp stylesheets) $
                (concat $ [app-styles])
                (str/join "\n" $)
                (if minify-css? $ $)
                ;; TODO Minification. I was doing `(minify-css $)` above but getting a weird error when I tried to boot the project with the asset-minifier dep.
                ;; asset-minifier {:mvn/version "0.2.7"}
                ;; [asset-minifier.core :refer [minify-css]]
                )]
      (make-parents f)
      (spit f css))
    (doseq [[asset path] (conj assets ["resources/public" nil])]
      ((if (fs/directory? asset)
         fs/copy-dir-into
         fs/copy)
       asset (str dir "/" path)))))

(defn- build-sw []
  (let [{:keys [err] :as res} (sh "node" "resources/build-sw.js" public-out-dir)]
    (when-not (str/blank? err)
      (throw (ex-info "Error building service worker." res)))
    (println res)))

(defn release [bundle-config]
  (shadow/release :app)
  (bundle bundle-config :minify-css? true)
  (build-sw))



(defn -main [bundle-config-sym]
  (let [config @(require-and-resolve (symbol bundle-config-sym))]
    (release config)))
