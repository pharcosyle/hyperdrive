(ns hyperdrive.singularity.services
  (:require [cljs.reader :refer [read-string]]
            [re-frame.core :refer [dispatch]]
            ["@aws-amplify/auth" :default Auth]
            ["aws-appsync" :default AWSAppSyncClient]
            ["graphql-tag" :default gql]
            ["pako" :as pako] ; TODO add to packages.json after datomic rewrite if I'm keeping this
            ["/aws-exports" :default awsconfig]
            ["/graphql/mutations" :refer [megalith]]))


(defn notify-aws-error [err]
  (dispatch [:notify (or (.-message err) err "Operation failed.")])) ; This is kind of lazy, I should be writing custom messages (e.g. "Password not changed.") and dispatching :default-failure (with :not-authenticated as the error type for the auth stuff).

(def ^:private get-appsync-client
  (fn []
    (-> Auth
        .currentSession
        (.then (fn [session]
                 (AWSAppSyncClient.
                  (clj->js {:url (.-aws_appsync_graphqlEndpoint awsconfig)
                            :region (.-aws_appsync_region awsconfig)
                            :auth {:type (.-aws_appsync_authenticationType awsconfig)
                                   :jwtToken (-> session .getIdToken .getJwtToken)}
                            :disableOffline true})))))))

#_(def ^:private get-appsync-client
  (memoize
   (fn []
     (AWSAppSyncClient.
      (clj->js {:url (.-aws_appsync_graphqlEndpoint awsconfig)
                :region (.-aws_appsync_region awsconfig)
                :auth {:type (.-aws_appsync_authenticationType awsconfig)
                       :jwtToken (fn []
                                   (-> Auth
                                       .currentSession
                                       (.then (fn [session]
                                                (-> session .getIdToken .getJwtToken)))))}
                :disableOffline true})))))

(defn service [opts]
  (-> (get-appsync-client)
      (.then (fn [client]
               (-> client
                   (.mutate (clj->js
                             {:mutation (gql megalith)
                              :variables {:freight
                                          (pr-str (merge {:service-name (:name opts)}
                                                         (when-let [payload (:payload opts)]
                                                           {:payload payload})))}}))
                   (.then (fn [res]
                            (js/console.log res) ; TODO remove
                            (let [{:keys [status body] :as payload} (-> (-> res .-data .-megalith)
                                                                        js/JSON.parse
                                                                        (js->clj :keywordize-keys true))]
                              (if (= status "success")
                                (when-let [on-success (:on-success opts)]
                                  (dispatch (conj on-success
                                                  ((if (:compressed? opts)
                                                     #(.inflate pako (js/atob %) (clj->js {:to "string"}))
                                                     read-string)
                                                   body))))
                                (throw payload))))))))
      (.catch (fn [err]
                (js/console.log "errored!") ; TODO remove
                (js/console.dir err)        ; TODO remove
                (if-let [on-failure (:on-failure opts)]
                  (dispatch (conj on-failure
                                  (cond
                                    (or (some #{err} ["No current user" "not authenticated"])
                                        (= (some-> err .-networkError .-statusCode) 401))
                                    :not-authenticated
                                    (and err (.-networkError err))
                                    :network-error
                                    :else :internal-server-error)))
                  (notify-aws-error err))))))
