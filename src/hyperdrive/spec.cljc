(ns hyperdrive.spec
  (:require [clojure.spec.alpha :as s]
            [datascript.core :as d]
            [hyperdrive.util :as u :refer [fmap]]))


;;;; General

(s/def ::generic some?)
(s/def ::id any?) ; TODO!!!!!! Using a mix of test data with 10-count string ids at present. (s/and string? #(= (count %) 8)) ; TODO 2020-01-30 Update: made this `any?` instead of `string?` so I can assign a new refund id to a new sale during refunds temporarily
(s/def ::date pos-int?) ; Whether a negative value can be used to express dates before the epoch is an application decision. There's no call at present so keep the spec restrictive.
(s/def ::number number?)
(s/def ::pos-int pos-int?)
(s/def ::money (s/and number? pos?))
(s/def ::any-money (s/and number? (complement neg?)))
(s/def ::qty-map (s/map-of ::id pos-int? :min-count 1))
(s/def ::any-qty-map (s/map-of ::id int? :min-count 1))
(s/def ::money-map (s/map-of ::id ::money :min-count 1))
;; (s/def ::any-money-map (s/map-of ::id ::any-money :min-count 1))
(s/def ::list (s/coll-of string? :kind vector? :min-count 1))
(s/def ::unique-list (s/coll-of string? :kind vector? :min-count 1 :distinct true))
(s/def ::id-list (s/coll-of ::id :kind vector? :min-count 1 :distinct true))
(s/def ::percent (s/and pos-int? #(<= % 100)))
(s/def ::decimal-percent (s/and number? #(<= 0 % 100)))
(s/def ::code string?)




;;;; Entity Common

(s/def :e/id ::id)
;; These should really all be e.g. :e.type/shop not :type/shop. ; TODO
(s/def :e/type #{:type/shop
                 :type/register
                 :type/register-count
                 :type/register-adjustment
                 :type/item
                 :type/sku
                 :type/category
                 :type/manufacturer
                 :type/order
                 :type/transfer
                 :type/count
                 :type/customer
                 :type/customer-type
                 :type/credit-account
                 :type/gift-card
                 :type/employee
                 :type/timesheet
                 :type/sale
                 :type/refund
                 :type/payment-type
                 :type/active-register
                 :type/clock
                 :type/transaction ; Stub inclusion of a component entity in types.
                 :type/settings})
(s/def :e/date ::date)

(s/def :entity/common (s/keys :req [:e/id
                                    :e/type]))

;; TODO not sure where this should really go
(s/def :archivable/archived? boolean?)




;;;; "Component" Entities

;;; Sale Line

(s/def :sale-line/line-type #{:sku
                              :credit-account
                              :gift-card})
(s/def :sale-line/price ::any-money)
(s/def :sale-line/employee ::id)
(s/def :sale-line/sku ::id)
(s/def :sale-line/credit-account ::id)
(s/def :sale-line/code ::code)

(defmulti sale-line-type :line-type)

(s/def :sale-line/common (s/keys :req-un [:e/id
                                          :sale-line/line-type
                                          :sale-line/price
                                          :sale-line/employee]))

(defmethod sale-line-type :sku [_]
  (s/merge :sale-line/common
           (s/keys :req-un [:sale-line/sku])))

(defmethod sale-line-type :credit-account [_]
  (s/merge :sale-line/common
           (s/keys :req-un [:sale-line/credit-account])))

(defmethod sale-line-type :gift-card [_]
  (s/merge :sale-line/common
           (s/keys :req-un [:sale-line/code])))

(s/def ::sale-line (s/multi-spec sale-line-type :line-type))


;;; Payment

;; There attributes are named a bit inconsistently, e.g. {:transaction/line-type :type/credit-accout} should really be {:transaction/type :transaction.type/credit-account} ; TODO

(s/def :transaction/line-type #{:type/payment-type
                                :type/credit-account
                                :type/gift-card
                                :type/exchange})
(s/def :transaction/source ::id)
(s/def :transaction/amount ::money)
(s/def :transaction/register ::id)
(s/def :transaction/employee ::id)

(s/def ::transaction (s/merge :entity/common
                              (s/keys :req [:transaction/line-type
                                            :transaction/amount
                                            :transaction/register
                                            :transaction/employee]
                                      :opt [:transaction/source]))) ; TODO Making this optional for now so :type/exchange transactions that don't have this work




;;;; Shared entity and new entity

(s/def ::sale-skus (s/coll-of ::sale-line :kind vector? :min-count 1))

(s/def :shop/name ::generic)
(s/def :register/name ::generic)
(s/def :register/shop ::id)
(s/def :register-count/register ::id)
(s/def :register-count/open-amounts ::money-map)
(s/def :register-count/open-employee ::id)
(s/def :register-count/open-date ::date)
(s/def :register-count/amounts ::money-map)
(s/def :register-count/left-in-drawer ::money)
(s/def :register-count/employee ::id)
(s/def :register-count/notes ::generic)
(s/def :register-adjustment/type #{:add
                                   :payout})
(s/def :register-adjustment/register ::id)
(s/def :register-adjustment/employee ::id)
(s/def :register-adjustment/payment-type ::id)
(s/def :register-adjustment/amount ::money)
(s/def :register-adjustment/notes ::generic)
(s/def :item/name ::generic)
(s/def :item/manufacturer ::id)
(s/def :item/category ::id)
(s/def :item/price ::any-money)
(s/def :item/msrp-price ::any-money)
(s/def :item/online-price ::any-money)
(s/def :item/wholesale-price ::any-money)
(s/def :item/default-cost ::any-money)
(s/def :item/images ::list)
(s/def :item/colors ::unique-list)
(s/def :item/sizes ::unique-list)
(s/def :item/tags ::unique-list)
(s/def :item/notes ::generic)
(s/def :item/max-commission ::decimal-percent)
(s/def :item/employee ::id)
(s/def :sku/item ::id)
(s/def :sku/color string?)
(s/def :sku/size string?)
(s/def :sku/upc ::code)
(s/def :sku/ean ::code)
(s/def :sku/code ::code)
(s/def :sku/style-number string?)
(s/def :sku/custom-sku-number string?)
(s/def :sku/price ::any-money)
(s/def :sku/msrp-price ::any-money)
(s/def :sku/online-price ::any-money)
(s/def :sku/wholesale-price ::any-money)
(s/def :sku/default-cost ::any-money)
(s/def :sku/images ::list)
(s/def :category/name ::generic)
(s/def :category/parent ::id)
(s/def :manufacturer/name ::generic)
(s/def :manufacturer/address ::generic)
(s/def :manufacturer/address2 ::generic)
(s/def :manufacturer/city ::generic)
(s/def :manufacturer/state ::generic)
(s/def :manufacturer/zipcode ::generic)
(s/def :manufacturer/country ::generic)
(s/def :manufacturer/mobile-phone ::generic)
(s/def :manufacturer/home-phone ::generic)
(s/def :manufacturer/work-phone ::generic)
(s/def :manufacturer/email string?)
(s/def :manufacturer/email2 string?)
(s/def :manufacturer/website ::generic)
(s/def :order/status #{:open
                       :finished})
(s/def :order/shop ::id)
(s/def :order/vendor ::generic)
(s/def :order/items ::id-list)
(s/def :order/costs ::money-map)
(s/def :order/skus ::qty-map)
(s/def :order/received-skus ::qty-map)
(s/def :order/expected-on ::date)
(s/def :order/notes ::generic)
(s/def :order/shipping ::money)
(s/def :order/due ::date)
(s/def :order/payment-method #{:order.payment-method/bank
                               :order.payment-method/wire
                               :order.payment-method/cod
                               :order.payment-method/net
                               :order.payment-method/credit-card})
(s/def :order/received-on ::date)
(s/def :order/employee ::id)
(s/def :transfer/status #{:sent
                          :received})
(s/def :transfer/from ::id)
(s/def :transfer/to ::id)
(s/def :transfer/items ::id-list)
(s/def :transfer/skus ::qty-map)
(s/def :transfer/received-skus ::qty-map)
(s/def :transfer/notes ::generic)
(s/def :transfer/received-on ::date)
(s/def :transfer/employee ::id)
(s/def :count/name ::generic)
(s/def :count/shop ::id)
(s/def :count/manufacturer ::id)
(s/def :count/category ::id)
(s/def :count/items ::id-list)
(s/def :count/counted ::qty-map)
(s/def :count/expected ::any-qty-map)
(s/def :count/employee ::id)
(s/def :customer/type ::id)
(s/def :customer/first-name ::generic)
(s/def :customer/last-name ::generic)
(s/def :customer/title ::generic)
(s/def :customer/company ::generic)
(s/def :customer/birthday ::generic)
(s/def :customer/address ::generic)
(s/def :customer/address2 ::generic)
(s/def :customer/city ::generic)
(s/def :customer/state ::generic)
(s/def :customer/zipcode ::generic)
(s/def :customer/country ::generic)
(s/def :customer/mobile-phone ::generic)
(s/def :customer/home-phone ::generic)
(s/def :customer/work-phone ::generic)
(s/def :customer/email string?)
(s/def :customer/email2 string?)
(s/def :customer/website ::generic)
(s/def :customer/assigned-tier ::pos-int)
(s/def :customer/notes ::generic)
(s/def :customer/employee ::id)
(s/def :customer-type/name ::generic)
(s/def :customer-type/discount ::percent)
(s/def :credit-account/bot-balance number?)
(s/def :credit-account/customer ::id)
(s/def :credit-account/limit ::any-money)
(s/def :credit-account/employee ::id)
(s/def :gift-card/bot-balance ::money)
(s/def :gift-card/code ::code)
(s/def :gift-card/customer ::id)
(s/def :gift-card/notes ::generic)
(s/def :gift-card/employee ::id)
(s/def :employee/email string?)
(s/def :employee/role #{:associate
                        :manager
                        :admin})
(s/def :employee/name ::generic)
(s/def :employee/pin string?)
(s/def :employee/locked? boolean?)
(s/def :timesheet/employee ::id)
(s/def :timesheet/in ::date)
(s/def :timesheet/out ::date)
(s/def :sale/sale-type #{:sale.type/in-store
                         :sale.type/ecom})
(s/def :sale/lines ::sale-skus)
(s/def :sale/items ::id-list)
(s/def :sale/code ::code)
(s/def :sale/bulk? boolean?)
(s/def :sale/layaway? boolean?)
(s/def :sale/customer ::id)
(s/def :sale/no-tax? boolean?)
(s/def :sale/shipping-cost ::any-money)
(s/def :sale/transactions (s/coll-of ::transaction :kind vector? :min-count 1))
(s/def :sale/discount ::percent)
(s/def :sale/refund ::id)
(s/def :sale/register ::id)
(s/def :sale/employee ::id)
(s/def :sale/ecom-status #{:not-shipped
                           :shipped
                           :canceled})
(s/def :sale/shopify-order-id pos-int?)
(s/def :refund/sale-lines ::id-list)
(s/def :refund/transactions (s/coll-of ::transaction :kind vector? :min-count 1))
(s/def :refund/register ::id)
(s/def :refund/employee ::id)
(s/def :payment-type/name ::generic)
(s/def :active-register/register ::id)
(s/def :active-register/amounts ::money-map)
(s/def :active-register/employee ::id)
(s/def :active-register/activated ::date)
(s/def :clock/employee ::id)
(s/def :clock/in ::date)

(s/def :entity.transfer/to-from-different (fn [transfer]
                                            (not= (:transfer/to transfer) (:transfer/from transfer))))
(s/def :entity.category/not-own-parent (fn [category]
                                         (not= (:e/id category) (:category/parent category))))
(s/def :entity.timesheet/in-before-out (fn [timesheet]
                                         (<= (:timesheet/in timesheet) (:timesheet/out timesheet))))





;;;; Entity

(defmulti entity-type :e/type)

(defmethod entity-type :type/shop [_]
  (s/merge :entity/common
           (s/keys :opt [:shop/name
                         :e/date])))

(defmethod entity-type :type/register [_]
  (s/merge :entity/common
           (s/keys :req [:register/shop]
                   :opt [:regsiter/name
                         :e/date])))

(defmethod entity-type :type/register-count [_]
  (s/merge :entity/common
           (s/keys :req [:register-count/register
                         :register-count/open-employee
                         :register-count/open-date
                         :register-count/employee
                         :e/date]
                   :opt [:register-count/open-amounts
                         :register-count/amounts
                         :register-count/left-in-drawer
                         :register-count/notes])))

(defmethod entity-type :type/register-adjustment [_]
  (s/merge :entity/common
           (s/keys :req [:register-adjustment/type
                         :register-adjustment/register
                         :register-adjustment/employee
                         :register-adjustment/payment-type
                         :register-adjustment/amount
                         :e/date]
                   :opt [:register-adjustment/notes])))

(defmethod entity-type :type/item [_]
  (s/merge :entity/common
           (s/keys :req [:item/price
                         :item/default-cost]
                   :opt [:item/name
                         :item/manufacturer
                         :item/category
                         :item/msrp-price
                         :item/online-price
                         :item/wholesale-price
                         :item/images
                         :item/colors
                         :item/sizes
                         :item/tags
                         :item/notes
                         :item/max-commission
                         :item/employee ; Optional because not in migrated.
                         :e/date])))

(defmethod entity-type :type/sku [_]
  (s/merge :entity/common
           (s/keys :req [:sku/item
                         :sku/color
                         :sku/size
                         :sku/code]
                   :opt [:sku/upc
                         :sku/ean
                         :sku/style-number
                         :sku/custom-sku-number
                         :sku/price
                         :sku/msrp-price
                         :sku/online-price
                         :sku/wholesale-price
                         :sku/default-cost
                         :sku/images
                         :e/date])))

(defmethod entity-type :type/category [_]
  (s/and (s/merge :entity/common
                  (s/keys :opt [:category/name
                                :category/parent
                                :e/date]))
         :entity.category/not-own-parent))

(defmethod entity-type :type/manufacturer [_]
  (s/merge :entity/common
           (s/keys :opt [:manufacturer/name
                         :manufacturer/address
                         :manufacturer/address2
                         :manufacturer/city
                         :manufacturer/state
                         :manufacturer/zipcode
                         :manufacturer/country
                         :manufacturer/mobile-phone
                         :manufacturer/home-phone
                         :manufacturer/work-phone
                         :manufacturer/email
                         :manufacturer/email2
                         :manufacturer/website
                         :e/date])))

(defmethod entity-type :type/order [_]
  (s/merge :entity/common
           (s/keys :req [:order/status
                         :order/shop
                         :order/items
                         :order/skus
                         :order/employee
                         :e/date]
                   :opt [:order/vendor
                         :order/costs
                         :order/received-skus
                         :order/expected-on
                         :order/notes
                         :order/shipping
                         :order/due
                         :order/payment-method
                         :order/received-on])))

(defmethod entity-type :type/transfer [_]
  (s/and (s/merge :entity/common
                  (s/keys :req [:transfer/status
                                :transfer/from
                                :transfer/to
                                :transfer/items
                                :transfer/skus
                                :transfer/employee
                                :e/date]
                          :opt [:transfer/received-skus
                                :transfer/received-on]))
         :entity.transfer/to-from-different))

(defmethod entity-type :type/count [_]
  (s/merge :entity/common
           (s/keys :req [:count/shop
                         :count/items
                         :count/counted
                         :count/expected
                         :count/employee
                         :e/date]
                   :opt [:count/name
                         :count/manufacturer
                         :count/category])))

(defmethod entity-type :type/customer [_]
  (s/merge :entity/common
           (s/keys :req [:e/date]
                   :opt [:customer/type
                         :customer/first-name
                         :customer/last-name
                         :customer/title
                         :customer/company
                         :customer/birthday
                         :customer/address
                         :customer/address2
                         :customer/city
                         :customer/state
                         :customer/zipcode
                         :customer/country
                         :customer/mobile-phone
                         :customer/home-phone
                         :customer/work-phone
                         :customer/email
                         :customer/email2
                         :customer/website
                         :customer/assigned-tier
                         :customer/notes
                         :customer/employee]))) ; Optional because not in migrated.

(defmethod entity-type :type/customer-type [_]
  (s/merge :entity/common
           (s/keys :opt [:customer-type/name
                         :customer-type/discount
                         :e/date]))) ; Optional because not in migrated.

(defmethod entity-type :type/credit-account [_]
  (s/merge :entity/common
           (s/keys :req [:credit-account/customer
                         :credit-account/limit
                         :e/date]
                   :opt [:credit-account/bot-balance
                         :credit-account/employee]))) ; Optional because not in migrated.

(defmethod entity-type :type/gift-card [_]
  (s/merge :entity/common
           (s/keys :req [:gift-card/code
                         :e/date]
                   :opt [:gift-card/bot-balance
                         :gift-card/customer
                         :gift-card/notes
                         :gift-card/employee]))) ; Optional because not in migrated.

(defmethod entity-type :type/employee [_]
  (s/merge :entity/common
           (s/keys :req [:employee/name
                         :e/date]
                   :opt [:employee/pin
                         :employee/locked?
                         ;; These two are optional because the fixture employees won't have them. Ideally have some conditional logic that makes them required if the entity being validated isn't a fixture. ; TODO maybe TODO
                         :employee/email
                         :employee/role])))

(defmethod entity-type :type/timesheet [_]
  (s/and (s/merge :entity/common
                  (s/keys :req [:timesheet/employee
                                :timesheet/in
                                :timesheet/out]))
         :entity.timesheet/in-before-out))

(defmulti sale-type :sale/sale-type)

(s/def :sale/common (s/merge :entity/common
                             (s/keys :req [:sale/sale-type
                                           :sale/code
                                           :sale/register
                                           :sale/employee
                                           :e/date]
                                     :opt [:sale/lines
                                           :sale/items
                                           :sale/bulk?
                                           :sale/layaway?
                                           :sale/customer
                                           :sale/no-tax?
                                           :sale/shipping-cost
                                           :sale/transactions
                                           :sale/discount
                                           :sale/refund])))

(defmethod sale-type :sale.type/in-store [_]
  :sale/common)

(defmethod sale-type :sale.type/ecom [_]
  (s/merge :sale/common
           (s/keys :req [:sale/ecom-status
                         :sale/shopify-order-id])))

(defmethod entity-type :type/sale [_]
  (s/multi-spec sale-type :sale/sale-type))

(defmethod entity-type :type/refund [_]
  (s/merge :entity/common
           (s/keys :req [:refund/sale-lines
                         :refund/transactions
                         :refund/register
                         :refund/employee
                         :e/date])))

(defmethod entity-type :type/payment-type [_]
  (s/merge :entity/common
           (s/keys :req [:payment-type/name
                         :e/date])))

(defmethod entity-type :type/active-register [_]
  (s/merge :entity/common
           (s/keys :req [:active-register/register
                         :active-register/employee
                         :active-register/activated]
                   :opt [:active-register/amounts])))

(defmethod entity-type :type/clock [_]
  (s/merge :entity/common
           (s/keys :req [:clock/employee
                         :clock/in])))

(defmethod entity-type :type/settings [_]
  (s/merge :entity/common
           (s/keys :req [:settings/key
                         :settings/value])))

(s/def ::entity (s/multi-spec entity-type :e/type))





;;;; New Entity (what the entity has to look like after the user is done inputting stuff but before I add any other necessary attributes and fire the create event)

(defmulti new-entity-type :e/type)

(defmethod new-entity-type :type/shop [_]
  (s/keys :req []))

(defmethod new-entity-type :type/register [_]
  (s/keys :req []))

(defmethod new-entity-type :type/register-adjustment [_]
  (s/keys :req [:register-adjustment/payment-type
                :register-adjustment/amount]))

(defmethod new-entity-type :type/item [_]
  (s/keys :req [:item/price
                :item/default-cost]))

(defmethod new-entity-type :type/category [_]
  (s/keys :req []))

(defmethod new-entity-type :type/manufacturer [_]
  (s/keys :req []))

(defmethod new-entity-type :type/order [_]
  (s/keys :req [:order/shop
                :order/items
                :order/skus]))

(defmethod new-entity-type :type/transfer [_]
  (s/and (s/keys :req [:transfer/from
                       :transfer/to
                       :transfer/items
                       :transfer/skus])
         :entity.transfer/to-from-different))

(defmethod new-entity-type :type/count [_]
  (s/keys :req [:count/shop
                :count/items
                :count/counted]))

(defmethod new-entity-type :type/customer [_]
  (s/keys :req []))

(defmethod new-entity-type :type/customer-type [_]
  (s/keys :req []))

(defmethod new-entity-type :type/gift-card [_]
  (s/keys :req [:gift-card/code]))

(defmethod new-entity-type :type/employee [_]
  (s/keys :req [:employee/email
                :employee/name]))

(defmethod new-entity-type :type/timesheet [_]
  (s/and (s/keys :req [:timesheet/in
                       :timesheet/out])
         :entity.timesheet/in-before-out))

(defmethod new-entity-type :type/sale [_]
  (s/keys :req []))

(defmethod new-entity-type :type/refund [_]
  (s/keys :req [:refund/sale-lines]))

(defmethod new-entity-type :type/payment-type [_]
  (s/keys :req [:payment-type/name]))

(s/def ::new-entity (s/multi-spec new-entity-type :e/type))





;;;; Validation

(def valid-entity-attributes? (partial s/valid? (s/keys)))

(defn valid-entity? [entity]
  (s/valid? ::entity entity))

(defn valid-new-entity? [new-entity]
  (s/valid? ::new-entity (dissoc new-entity :e/id))) ; Way to get around new entities having a different :e/id format.

(defn- valid-entity-modification? [db statement validation-fn how]
  (let [[operation [_ id] attribute value] statement
        modify (case operation
                 :db/add assoc
                 :db.fn/retractAttribute dissoc)
        ;; Hacky way to get around new entities having a different :e/id format. This assumes the two `how` options correspond to conn and new-conn which is the way they're currently used anyway.
        entity (let [e (d/pull db '[*] [:e/id id])]
                 (if (= how :simple)
                   (dissoc e :e/id)
                   e))]
    (validation-fn (modify entity attribute value))))

(defn valid-tx? [db tx how]
  (let [validation-fn (case how
                        :whole valid-entity?
                        :simple valid-entity-attributes?)
        validations
        (for [statement tx]
          (if (map? statement)
            ;; Hacky. Ensures a sku can't be created for a item/color/size combination already in use. ; TODO bring this back?
            ;; (if (= (:e/type statement) :type/sku)
            ;;   (and (validation-fn statement)
            ;;        (let [sku statement]
            ;;          (not (item-sku-unique db (:sku/item sku) (:sku/color sku) (:sku/size sku)))))
            ;;   (validation-fn statement))
            (validation-fn statement)
            (case (first statement)
              :db/add (valid-entity-modification? db statement validation-fn how)
              :db.fn/retractAttribute (valid-entity-modification? db statement validation-fn how)
              :db/retractEntity true)))]
    (every? #(= % true) validations)))




;;;; Parsing

#?(:cljs
   (defn parse [spec x]
     (condp = (s/form spec)
       ;; (s/form ::date) ...
       (s/form ::number) (u/parse-number x)
       (s/form ::pos-int) (u/parse-int x)
       (s/form ::money) (u/parse-number x)
       (s/form ::any-money) (u/parse-number x)
       (s/form ::qty-map) (fmap u/parse-int x)
       ;; (s/form ::any-qty-map) (fmap u/parse-int x)
       (s/form ::money-map) (fmap u/parse-number x)
       ;; (s/form ::any-money-map) (fmap u/parse-number x)
       (s/form ::percent) (u/parse-int x)
       (s/form ::decimal-percent) (u/parse-number x)
       (s/form ::sale-skus) (vec (map #(update % :price u/parse-number) x))
       x)))











;; TODO temporary, at least make it :new.refund/sale ultimaitely I think
(s/def :refund/sale ::id)
