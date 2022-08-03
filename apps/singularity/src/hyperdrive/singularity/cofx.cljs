(ns hyperdrive.singularity.cofx
  (:require [cljs.reader :refer [read-string]]
            [re-frame.core :refer [reg-cofx]]
            [hyperdrive.db :as db :refer [conn new-conn]]
            [hyperdrive.queries :as q]
            [hyperdrive.util :as u :refer [gen-id gen-code]]
            [hyperdrive.singularity.comms :as comms]))

(reg-cofx
 :conn
 (fn [cofx]
   (assoc cofx :conn @conn)))

;; ;; Gonna use this one instead of :conn from now on.
;; (reg-cofx
;;  :ds
;;  (fn [cofx]
;;    (assoc cofx :ds @conn)))

(reg-cofx
 :new-conn
 (fn [cofx]
   (assoc cofx :new-conn @new-conn)))

(reg-cofx 
 :id
 (fn [cofx]
   (assoc cofx :id (gen-id))))

(reg-cofx
 :code
 (fn [cofx]
   (assoc cofx :code (gen-code @conn))))

;; (reg-cofx
;;  :now
;;  (fn [cofx]
;;    (assoc cofx :now (u/now))))

(reg-cofx
 :server-flags
 (fn [cofx]
   (assoc cofx :customer-display? js/window.isCustomerDisplay)))

(reg-cofx
 :anything-pending?
 (fn [cofx]
   (assoc cofx :anything-pending? (comms/anything-pending?))))

(reg-cofx
 :local-storage
 (fn [cofx]
   (assoc cofx :local-storage (read-string (.getItem js/localStorage u/local-storage-passkey-key)))))

(reg-cofx
 :new-passkey
 (fn [cofx]
   (assoc cofx :new-passkey (str (+ (rand-int 900000) 100000)))))
