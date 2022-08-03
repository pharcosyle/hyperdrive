(ns hyperdrive.singularity.models
  (:refer-clojure :exclude [name]) ; Get rid of that "being replaced by" warning.
  (:require [clojure.string :as str]
            [hyperdrive.util :as u :refer [fixture]]))

(defn- name [named]
  (u/string-for named [:first-name :last-name]))

(defn register-adjustment:change [reg-adj]
  ((case (:type reg-adj)
     :add +
     :payout -)
   (:amount reg-adj)))

(def customer:name name)
(def employee:name :name)

(defn sku:barcode [sku]
  (or (:upc sku) (:ean sku) (:code sku)))

(defn timesheet:time [ts]
  (- (:out ts) (:in ts)))

(def sale:barcode :code)

(defn sale:cart [lines]
  (->> lines
       (filter #(= (:line-type %) :sku))
       (map :sku)
       frequencies))


;; TODO Used on multiple models, maybe make this polymorphic later if I end up doing types.
(def date :date)


(defn count:manual-adjustment? [count]
  (and (not (:category count))
       (not (:manufacturer count))))







(defn register-count:cash-deposit [reg-count]
  (- (get-in reg-count [:amounts (fixture :fixture/payment-type-cash)])
     (:left-in-drawer reg-count)))
