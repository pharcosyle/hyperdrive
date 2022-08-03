(ns hyperdrive.ion.config
  (:require [datomic.ion :as ion]))


(def ^:dynamic *env* nil)

(def ^:private get-ion-env
  (memoize #(get (ion/get-env) :env)))

(defn get-env []
  (or (get-ion-env) *env*))

(defmacro bind-env [env & body]
  `(binding [*env* (keyword ~env)]
     ~@body))



(defn- fail [k]
  (throw (ex-info "Unable to get a value." {:key k})))

(def ^:private get-params
  (let [get-params* (memoize (fn [env]
                               (let [app (or (get (ion/get-app-info) :app-name) (fail :app-name))
                                     env (or env (fail :env))]
                                 (ion/get-params {:path (str "/datomic-shared/" (name env) "/" app "/")}))))]
    (fn []
      (get-params* (get-env)))))

(defn get-param [k]
  (or (get (get-params) (name k)) (fail k)))
