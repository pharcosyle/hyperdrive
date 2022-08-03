(ns hyperdrive.singularity.webusb
  (:require [clojure.string :as str]
            [hyperdrive.util :refer [truncate]]))

(def ^:private usb js/navigator.usb)

(defn get-devices [cb]
  (if usb
    (-> usb
        .getDevices
        (.then cb))
    ;; (cb nil)
    ))

(defn device-names [devices]
  (str/join " and " (map #(.-productName ^js %) devices)))

(def ^{:private true
       :doc "Logic Controls Line Display LD9000"}
  pole
  {:vendor-id 0x0fa8
   :configuration 1
   :interface 0
   :endpoint 2
   :line-length 20})

(defn connect [& {:keys [on-error on-webusb-missing]}]
  (if usb
    (-> usb
        (.requestDevice (clj->js {:filters []})); {:vendorId (:vendor-id pole)} ; Filter out stuff that isn't a pole display.
        (.catch on-error))
    (on-webusb-missing)))

;; Uses all paired devices (just calls `.getDevices()`) even if there was an error opening the device or claiming a configuration/interface. Transfers may therefore fail and fill up the console with errors. Really there should be a layer after Chrome's paired device management (requestDevice, the permission dropdown in the omnibar), something like a list that shows you which are paired, a checkbox to toggle them on/off, and some indicator to show if they're in a bad state (interface/configuration claiming failed or whatever). Right now I conflate the two by just using the getDevices and consequently there's some unmanaged possible error states and stuff. Update: The conflation bit is still true but I updated the transfer code to attempt opening/claiming on every transfer.
(defn send [{:keys [line1 line2]} & {:keys [on-error]}]
  (get-devices
   (fn [devices]
     (doseq [^js device devices]
       ;; (println (.-opened device))
       ;; Open the device / configure / etc on every call, sometimes the device disconnects e.g. if I leave my macbook for a while.
       (-> device
           .open
           (.then (fn [] (.selectConfiguration device (:configuration pole))))
           (.then (fn [] (.claimInterface device (:interface pole))))
           (.then
            (fn []
              (.transferOut
               device
               (:endpoint pole)
               (let [str->array-buffer
                     (fn [s]
                       (let [buf (js/ArrayBuffer. (* (count s) 2)) ; Two bytes for each char.
                             buf-view (js/Uint16Array. buf)]
                         (dotimes [n (count s)]
                           (aset buf-view n (.charCodeAt s n)))
                         buf))
                     data (let [newline "\r\n"
                                length (dec (:line-length pole))] ; The cursor takes up a character so I guess just don't use the last character of each line.
                            (str newline newline
                                 (truncate line1 length) newline
                                 (truncate line2 length)))]
                 (str->array-buffer data)))))
           (.catch on-error))))))
