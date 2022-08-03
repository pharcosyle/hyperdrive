(ns hyperdrive.singularity.comms
  (:require [re-frame.core :refer [dispatch]]
            [reagent.core :as r])
  (:import [goog.async Debouncer]))

;; spec:
;; - debounce 2 seconds when change is for an attribute and override/re-fire when another change for the same attribute comes in
;; - only fire next xhr after the previous one is done (maintain order)
;; - stop saving all suff if there's an error
;;
;; state:
;; - persist-hold (atom)
;; - debouncer
;; - persist-queue (queue)
;; - :persist/request-in-progress? flag
;;
;; - a tx comes in. Look at the persist-hold:
;;   - if it's a change
;;     - if nothing is there or it's an overriding change: call A with the tx
;;     - if it's a different change: do C and call A with the new tx
;;   - else
;;     - do C if there's something in the hold then call B with the new tx  
;;
;; - when debouncer completes it calls B with the tx in the hold and clears the hold.
;;
;; A: reset the tx into the hold and start/fire the debouncer.
;; B: - put tx in the queue
;;    - if flag :persist/request-in-progress? is false, do D
;; C: stop the debouncer, call B with the tx in the hold, and clear the hold
;; D: peek the queue and if there's something set :persist/request-in-progress? to true and fire xhr :persist event with the first (pop) tx in the queue
;;    - on complete
;;      - set :persist/request-in-progress? to false
;;      - if success, do D
;;      - if failure: throw up the undismissable error modal. Wipe the queue, .stop the debouncer, and set the hold to nil

(declare enqueue!)
(declare enqueue-hold!)


(defonce ^:private persist-hold (r/atom nil))
(defonce ^:private debouncer (Debouncer. (fn []
                                           (enqueue-hold!)) 2000)) ; Gotta wrap `enqueue-hold!` in a function because it's only declared, not defined yet.

(defn- single-fact-tx? [tx]
  (and (= (count tx) 1)
       (let [statement (first tx)]
         (and (not (map? statement))
              (some #{(first statement)} [:db/add :db.fn/retractAttribute])))))

(defn- overrides-hold? [tx]
  (let [id-and-attr (fn [tx]
                      [(-> tx first (get 1))
                       (-> tx first (get 2))])]
    (= (id-and-attr tx) (id-and-attr @persist-hold))))

(defn- something-held? []
  @persist-hold)

(defn- enqueue-hold! []
  (let [tx @persist-hold]
    (reset! persist-hold nil)
    (enqueue! tx)))

(defn- stop-debouncer-and-enqueue-hold! []
  (.stop debouncer)
  (enqueue-hold!))

(defn push [tx]
  (if (single-fact-tx? tx)
    (do
      (when (and (something-held?)
                 (not (overrides-hold? tx)))
        (stop-debouncer-and-enqueue-hold!))
      (.fire debouncer)
      (reset! persist-hold tx))
    (do
      (when (something-held?)
        (stop-debouncer-and-enqueue-hold!))
      (enqueue! tx))))



(defonce ^:private persist-queue (r/atom #queue []))
(defonce ^:private request-in-progress? (r/atom false))

(defn- try-firing-next-request! []
  (when-let [tx (peek @persist-queue)]
    (swap! persist-queue pop)
    (reset! request-in-progress? true)
    (dispatch [:persist tx])))

(defn- enqueue! [tx]
  (swap! persist-queue conj tx)
  (when-not @request-in-progress?
    (try-firing-next-request!)))



(defn anything-pending? []
  (or (seq @persist-queue)
      (something-held?)
      @request-in-progress?))

(defn- clear []
  (reset! persist-queue #queue [])
  (.stop debouncer)
  (reset! persist-hold nil))

(defn on-request-finished [status]
  (reset! request-in-progress? false)
  (case status
    :success (try-firing-next-request!)
    :failure (clear)))
