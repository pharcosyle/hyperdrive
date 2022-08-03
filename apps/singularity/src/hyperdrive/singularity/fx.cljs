(ns hyperdrive.singularity.fx
  (:require [clojure.string :as str]
            [re-frame.core :refer [reg-fx dispatch]]
            [accountant.core :as accountant]
            [hyperdrive.db :as db]
            [hyperdrive.util :as u :refer [chat-url filestack-api-key]]
            [hyperdrive.singularity.comms :as comms]
            [hyperdrive.singularity.logging :as log]
            [hyperdrive.singularity.realtime :as realtime]
            [hyperdrive.singularity.routes :as routes]
            [hyperdrive.singularity.services :refer [notify-aws-error service]]
            [hyperdrive.singularity.webusb :as webusb]
            ["@aws-amplify/auth" :default Auth]
            ["filestack-js" :as filestack]
            ["pnotify/dist/es/PNotify" :default PNotify]
            ["pnotify/dist/es/PNotifyButtons"]
            ["jquery" :as jq]
            ["/aws-exports" :default awsconfig]))


(reg-fx
 :init-window
 (fn []
   (.addEventListener js/window "error"
                      (fn [e]
                        ;; Wrap in try/catch so on the off chance there's an error in the logging code it doesn't throw another top-level error and start and infinite loop.
                        (try
                          (log/error "Top-level javascript error."
                                     {:message (.-message e)
                                      :filename (.-filename e)
                                      :line-number (.-lineno e)
                                      :column-number (.-colno e)
                                      :error {:message (when-let [error (.-error e)]
                                                         (.-message error))
                                              :stack (when-let [error (.-error e)]
                                                       (.-stack error))}})
                          (catch :default _))))
   (doto (jq js/window)
     (.keypress (fn [e]
                  (dispatch [:scanner/keystroke (.-which e)])))
     ;; If there's a modal open, close it when the user hits the back/forward button.
     (.on "popstate" (fn []
                       (dispatch [:modal/close]))))
   ;; Something in my local environment overwrites this but it works in production.
   (set! js/window.onbeforeunload (fn []
                                    (when (comms/anything-pending?)
                                      "Are you sure you want to leave? Changes you made may not be saved."))) ; Provided message is typically ignored by the browser.
   (when-let [sw js/navigator.serviceWorker]
     (if goog.DEBUG
       (-> sw
           .-ready
           (.then (fn [reg]
                    (.unregister reg))))
       (.addEventListener
        js/window "load"
        (fn []
          (-> sw
              (.register "/service-worker.js")
              (.then
               (fn [reg]
                 (.addEventListener
                  reg "updatefound"
                  (fn []
                    (when-let [new-worker (.-installing reg)]
                      (.addEventListener
                       new-worker "statechange"
                       (fn []
                         (when (and (= (.-state new-worker) "installed")
                                    (.-controller sw))
                           (dispatch [:notify {:title "A new version of Hyperdrive is available!"
                                               :text "It will be automatically installed when all open Hyperdrive tabs are closed."
                                               :type :info
                                               :hide false}]))))))))))))))
   (.addEventListener js/window "beforeinstallprompt" #(dispatch [:save-install-prompt-event %]))))

(reg-fx
 :reload
 (fn [path]
   (set! js/window.location (routes/path :page/home))))

(reg-fx
 :open-tab
 (fn [content]
   (when-let [new-tab (js/open)] ; Can return null if e.g. popups are blocked.
     (-> new-tab .-document (doto (.write content) .close))))) ; `.close` stops the tab spinny.

(reg-fx
 :scroll-to-top
 (fn []
   (js/window.scroll 0 0)))

(reg-fx
 :modal
 (fn [method]
   (.modal (jq ".modal") method))) ; HACK: Ideally don't use a jquery selector to get the modal window, but the whole bootstrap modal is kind of a hack anyway.

;; HACK: Made to work specifically with the upcs modal.
(reg-fx
 :tab
 (fn []
   (let [current (jq js/document.activeElement)
         next (let [inputs (jq ".modal input")
                    index (.index inputs current)]
                (.get inputs (if (= index -1)
                               0 (inc index))))]
     (if next
       (doto next .focus .select)
       (.select current))))) ; Highlight the input text if this we're on the last input so repeated scans replace the value and don't just keep appending to it.

(reg-fx
 :alert
 (fn [message]
   (js/alert message)))

(reg-fx
 :notify
 (fn [opts]
   (.alert PNotify (clj->js opts))))

(reg-fx
 :local-storage-set
 (fn [value]
   (.setItem js/localStorage u/local-storage-passkey-key (pr-str value))))



;;;; DB

(reg-fx
 :db/init
 (fn [data]
   (db/init! data)
   (dispatch [:more-init])))

(defn catch-errors [db-fn tx]
  (try
    (db-fn tx)
    true
    (catch :default e
      (log/error (str "Datascript exception. Tx: " tx) e)
      (dispatch [:modal/show :showstopper {:heading "Local database exception"
                                           :text (str "Your change was not saved. This is a bug, please notify your system administrator." " Transaction data: " tx  " Exception: " e)
                                           :bad? true}])
      false)))

(reg-fx
 :db/transact
 (fn [tx]
   (when (catch-errors db/transact! tx)
     (comms/push tx))))

(reg-fx
 :db/transact-new
 (fn [tx]
   (catch-errors db/transact-new! tx)))

(reg-fx
 :db/transact-local
 (fn [tx]
   (catch-errors db/transact! tx)))

(reg-fx
 :db/transact-polling-updates
 (fn [txs]
   (try
     (doseq [tx txs]
       (db/transact! tx))
     (catch :default e
       (log/error (str "Datascript exception during update. Txs: " txs) e)
       (dispatch [:modal/show :showstopper {:heading "Update failed"
                                            :text (str "An error occurred while updating the app with the newest data from the server.")
                                            :bad? true}])))))



;;;; Comms

(reg-fx
 :comms/request-finished
 (fn [status]
   (comms/on-request-finished status)))



;;;; Navigation

(reg-fx
 :navigation/configure
 (fn []
   (accountant/configure-navigation!
    {:nav-handler #(dispatch [:route %])
     :path-exists? routes/match})))

(reg-fx
 :navigation/dispatch-current
 (fn []
   (accountant/dispatch-current!)))

(reg-fx
 :navigate
 (fn [path]
   (accountant/navigate! path)))



;;;; Filestack

(reg-fx
 :filestack/pick
 ;; TODO. Maybe do a memoize function at the top level instead
 (let [client (.init filestack filestack-api-key)]
   (fn [on-complete]
     (-> client
         (.picker (clj->js
                   {:accept "image/*"
                    :fromSources ["local_file_system" "url" "webcam" "imagesearch" "facebook" "instagram" "dropbox" "gmail"]
                    :maxFiles 20
                    :maxSize (* 1024 1024) ; 1MB limit, don't let them fill up filestack storage too fast.
                    :onClose #(dispatch [:set-filepicker-working? false])
                    :onUploadDone
                    (fn [res]
                      (let [res (js->clj res :keywordize-keys true)]
                        (when-let [failed (seq (:filesFailed res))]
                          (dispatch [:notify {:title (str (count failed) " image(s) failed to upload")
                                              :text (->> failed (map :filename) (str/join ", "))
                                              :hide false}]))
                        (let [uploaded (->> res :filesUploaded (map :handle))]
                          (dispatch (conj on-complete uploaded)))))}))
         .open))))



;;;; Web USB

(defn notify-webusb-error [e]
  (dispatch [:notify {:title "Web USB Error" :text e}]))

(reg-fx
 :webusb/connect
 (fn []
   (webusb/connect
    :on-error (fn [e]
                (when-not (= (.-name e) "NotFoundError") ; User closed the connect dialog without selecting anything.
                  (notify-webusb-error e)))
    :on-webusb-missing (fn []
                         (js/alert "This feature is only available on Chrome.")))))

(reg-fx
 :webusb/send-to-devices
 (fn [lines]
   ;; Lots of transient stuff could go wrong, like another app browser tab already claiming the pole display interface, so just tell the user something happened and forget about any errors.
   (webusb/send lines :on-error notify-webusb-error)))



;;;; Realtime

(reg-fx
 :realtime/init
 (fn [passkey]
   (realtime/init! passkey)))

(reg-fx
 :realtime/trigger
 (fn [event]
   (realtime/trigger event)))









(reg-fx
 :download
 (fn [[filename content]]
   (let [el (js/document.createElement "a")]
     (.setAttribute el "href" (str "data:text/plain;charset=utf-8," (js/encodeURIComponent content)))
     (.setAttribute el "download" filename)
     (set! (-> el .-style .-display) "none")
     (js/document.body.appendChild el)
     (.click el)
     (js/document.body.removeChild el))))









(reg-fx
 :auth/init
 (fn []
   (.configure Auth awsconfig)))

(reg-fx
 :auth/login-current-user
 (fn []
   (-> Auth
       .currentAuthenticatedUser
       (.then (fn [cognito-user]
                (let [user [:employee/email (-> cognito-user .-attributes .-email)]]
                  (dispatch [:initialize user]))))
       (.catch notify-aws-error))))

(reg-fx
 :auth/sign-out
 (fn []
   (-> Auth
       .signOut
       (.then (fn [] (dispatch [:reload])))
       (.catch notify-aws-error))))

(reg-fx
 :auth/change-password
 (fn [[current new]]
   (-> Auth
       .currentAuthenticatedUser
       (.then (fn [cognito-user]
                (.changePassword Auth cognito-user current new)))
       (.then (fn [data]
                (if (= data "SUCCESS")
                  (dispatch [:notify {:text "Password changed." :type :success}])
                  (notify-aws-error data))))
       (.catch notify-aws-error))))


(reg-fx
 :service
 service)








(reg-fx
 :toggle-sidebar-shown-impure
 (fn [toggled?]
   (.toggleClass (jq "body") "sidebar-toggled" (boolean toggled?))))








(reg-fx
 :show-install-prompt
 (fn [event]
   (.prompt event)))









(reg-fx
 :chat/login-iframe
 (fn [[node token]]
   ;; I'm not sure how to detect that rocketchat is connected but there doesn't seem to be any harm logging in extra times so just try repeatedly for a while and wrap .postMessage in a check to make sure the node hasn't disappeared because we left the page or whatever.
   (dotimes [n 20]
     (js/setTimeout
      (fn []
        (when-let [cw (.-contentWindow node)]
          (.postMessage cw
                        (clj->js {:event "login-with-token"
                                  :loginToken token})
                        chat-url)))
      (* n 300)))))
