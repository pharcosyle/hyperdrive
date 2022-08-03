(ns hyperdrive.util
  #?(:cljs (:refer-clojure :exclude [find])) ; Get rid of that "being replaced by" warning. ; TODO remove if I get rid of `find`
  (:require [clojure.string :as str]
            #?@(:clj [[clojure.edn :as edn]
                      [clojure.java.io :as io]])
            #?@(:cljs [[goog.string :as gs]
                       [cljs-time.core :refer [local-date-time]]
                       [cljs-time.format :as f]
                       [re-frame.core :refer [subscribe]]
                       [re-frame.utils :as rf-utils]]))
  #?(:cljs (:import [goog.i18n NumberFormat])))


;;;; General

(defn now []
  #?(:clj (System/currentTimeMillis)
     :cljs (.now js/Date)))

(defn seqify [x]
  (if (or (nil? x) (sequential? x))
    x [x]))

(defn kmap [f m]
  (into {} (for [[k v] m] [(f k) v])))

(defn fmap [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn no-nils [m]
  (into {} (remove (comp nil? val) m)))



;;;; App-specific

(def filestack-api-key "AYUKRuzl0SuyKDFw1pMInz")
(def chat-url "https://lithium.rocket.chat")

(defn gen-id []
  (let [chars "0123456789abcdefghijklmnopqrstuvwxyz"]
    (apply str (take 8 (repeatedly (fn []
                                     (get chars (rand-int (count chars)))))))))

(defn gen-code [db]
  (loop []
    (let [code (apply str (repeatedly 12 #(rand-int 10)))]
      ;; Ideally these are unique across all barcoded things and it's important that they be unique across their own doc type.
      code
      ;; TODO bring this back!!!!!
      ;; (if (or (q/find-skus-by-code* db code)
      ;;         (q/find-sales-by-code* db code)
      ;;         (q/find-gift-cards-by-code* db code))
      ;;   (recur)
      ;;   code)
      )))

(defn new-entity [type]
  {:e/id (gen-id)
   :e/type type
   :e/date (now)})

(defn image-url [handle-or-url & {w :width h :height :keys [fit sharpen progressive? placeholder?]}]
  (cond
    handle-or-url
    (str "https://cdn.filestackcontent.com"
         (when (str/starts-with? handle-or-url "http")
           (str "/" filestack-api-key))
         (when (or w h fit)
           (str "/resize="
                (str/join
                 ","
                 (remove nil?
                         [(when w
                            (str "w:" w))
                          (when h
                            (str "h:" h))
                          (when fit
                            (str "fit:" (name fit)))]))))
         (when sharpen
           (str "/sharpen=amount:" (if (= sharpen :default) 1 sharpen)))
         (when progressive?
           (str "/pjpg"))
         "/" handle-or-url)
    placeholder?
    "/images/no_picture.png"))







;; TODO temprary housing for fixtures until I figure out what to do with them. Don't take the placement of this function outside of a specific clj/cljs section to mean any/all are required on both the frontend and backend
(defn fixture [k]
  (get {:fixture/register-ecom "5jmy4eny"
        :fixture/employee-ecom "aaii3xrt"
        :fixture/payment-type-cash "u6760uqe"
        :fixture/payment-type-paypal "yuxmufid"} k))








#?
(:clj
 (do
   (defn require-and-resolve [sym & {:keys [reload?]}]
     (apply require (symbol (namespace sym)) (when reload? [:reload]))
     (resolve sym))

   (defn read-edn-file [f]
     (-> f slurp edn/read-string))

   (defn slurp-reader [x]
     (with-open [r (io/reader x)]
       (slurp r)))

   (defn parse-date [s]
     (-> (str "#inst " (pr-str s))
         edn/read-string
         .getTime))

   (defn http-error-code? [status]
     (re-matches #"[4|5]\d\d" (str status))))









 :cljs
 (do
;;;; General

   (def dissoc-in rf-utils/dissoc-in)

   (defn seq-contains? [s x]
     (some #{x} s))

   (defn seq-contains-all?
     "Won't check that the number of occurrences of an item in `xs` is the same as in `s`."
     [s xs]
     (every? (set s) xs))

   (defn sum-key [k s]
     (apply + (map k s)))

   (defn drop-nth-v [pos coll]
     (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

   (defn find [q docs]
     (filter (fn [doc]
               (= (or q {})
                  (merge (fmap (fn [_] nil) q)
                         (select-keys doc (keys q)))))
             docs))

   (def find-one (comp first find))

   (defn parse-int [s]
     (let [i (js/parseInt s)]
       (when-not (js/isNaN i) i)))

   (defn parse-number [x]
     (let [f (js/parseFloat (if (string? x)
                              (str/replace x "," "") x))]
       (when-not (js/isNaN f) f)))

   (defn order-comp
     ([order] (order-comp order compare))
     ([order comp]
      (fn [a b]
        (let [index (fn [x order]
                      (let [i (.indexOf order x)]
                        (when-not (= i -1)
                          i)))]
          (if-let [a-idx (index a order)]
            (if-let [b-idx (index b order)]
              (compare a-idx b-idx)
              -1)
            (if-let [b-idx (index b order)]
              1
              (comp a b)))))))

   (defn multi-comp [& fns]
     (fn [a b]
       (loop [fns fns]
         (if (seq fns)
           (let [result ((first fns) a b)]
             (if (= 0 result)
               (recur (rest fns))
               result))
           0))))

   (defn keep-juxt [x fns]
     (remove nil? ((apply juxt fns) x)))

   (defn string-for
     ([doc fns] (string-for doc " " fns))
     ([doc sep fns]
      (str/join sep (keep-juxt doc fns))))

   (defn val-or-deref [x]
     (if (satisfies? IDeref x) @x x))



;;;; Time

   (def ^:private formatters
     (fmap f/formatter
           {:simple "yyyy-MM-dd" ; 2016-12-19
            :time "h:mm A" ; 3:42 AM
            :full "yyyy-MM-dd h:mm A" ; 2016-12-19 3:42 AM
            :day-of-week "EEEE" ; Wednesday
            :month-and-day "MM/dd"})) ; 12/19

   (defn date
     ([d] (date d :simple))
     ([d f]
      (when d
        (f/unparse (get formatters f) (local-date-time (js/Date. d))))))

   #_(defn parse-date [s f]
       (try
         (.getTime (f/parse-local (get formatters f) s))
         (catch :default _)))

   (def ^:private one-minute 60000)
   (def one-hour 3600000)
   (def one-day 86400000)

   (defn elapsed
     ([x] (elapsed x :hours))
     ([x f]
      (when x
        (let [[start end] (if (sequential? x)
                            x [x (now)])]
          (let [duration (- end start)]
            (case f
              :hours (.toFixed (/ duration one-hour) 2)
              :numbers (str (int (/ duration one-hour)) ":" (let [minutes (rem (int (/ duration one-minute)) 60)]
                                                              (str (when (< minutes 10) "0")
                                                                   minutes)))))))))

   ;; (-> (local-date-time d)
   ;;       t/at-midnight
   ;;       (t/plus (t/days 1))
   ;;       (tc/to-date))

   (defn- before? [d1 d2]
     (or (not d1) (<= d1 d2)))

   (defn- after? [d1 d2]
     (or (not d1) (> d1 d2)))

   (defn between?
     ([[start end] d]
      (between? start end d))
     ([start end d]
      (when d
        (and (before? start d)
             (after? end d)))))



;;;; Frontend helpers

   ;; (defn tag [& kws]
   ;;   (keyword (apply str (map #(when % (name %))
   ;;                            kws))))

   (defn truncate [s n & {:keys [ellipsis?] :or {ellipsis? false}}]
     (when s
       (if (> (count s) n)
         (if ellipsis?
           (str (subs s 0 (- n 3)) "...")
           (subs s 0 n))
         s)))

   (defn spaces [n]
     (gs/unescapeEntities (str/join (repeat n "&nbsp;"))))

   (defn yes-no [bool]
     (if bool "Yes" "No"))

   (defn plus-minus
     ([n] (plus-minus n identity))
     ([n render]
      [:span {:class (cond
                       (neg? n) "text-danger"
                       (pos? n) "text-success")}
       (when (pos? n) "+")
       (render n)]))

   (defn percent-change [n]
     (let [percent (* (or n 0) 100)]
       (plus-minus n #(str (.toFixed percent 0) "%"))))

   (let [formatter (NumberFormat. NumberFormat.Format.CURRENCY)]
     (defn currency [n & {:keys [no-sign? coerce-nil?] :or {coerce-nil? true}}]
       (when (or n coerce-nil?)
         (let [s (.format formatter n)]
           (if no-sign?
             (subs s 1) s)))))

   (defn interpose-for [coll sep fns]
     (interpose sep (keep-juxt coll fns)))

   (defn thumbnail-url [handle dims]
     (image-url handle :width dims :height dims :fit :crop :sharpen :default :placeholder? true))



;;;; Re-frame helpers

   (defn listen [query-v]
     @(subscribe query-v))

   ;; Retreive value from the db once. Stopgap measure so I can use subscriptions I already have for this purpose and not write proper queries. ;; TODO search app for all usages and fix.
   (def fetch listen)



;;;; App-specific

   ;; TODO should be able to get rid of this by the end
   (defn strip [doc]
     (-> (into {} (for [[k v] doc]
                    (if (or (= k :e/id) (= k :e/type))
                      [k v]
                      [(keyword (name k)) v])))
         (assoc :id (:e/id doc))))

   (def roles [:associate :manager :admin]) ; In order of increasing clearance.

   (def new-id? vector?)

   (def alpha-sizes ["XS" "S" "M" "L" "XL" "2X" "2XL" "3X" "3XL" "4X" "4XL" "5X" "5XL"])

   (defn humanize [kw]
     (when kw
       (case kw
         :bot "Beginning of Time"
         :count "Inventory Count"
         :layaway? "Layaway"
         (->> (name kw)
              (#(str/split % #"-"))
              (map str/capitalize)
              (str/join " ")))))

   (def tiers
     (array-map 1 :bronze
                2 :silver
                3 :gold
                4 :platinum
                5 :diamond
                6 :premier
                7 :executive))

   (defn tier-image [tier]
     (str "/images/tiers/" (name (get tiers tier)) ".png"))

   (def local-storage-passkey-key "hyperdrive-customer-display-passkey")



;;; Select option helpers

   (def nil-opt "--nil--")

   (defn select-opts [{:keys [items text-fn value-fn sorting sort? none? all? all-text]
                       :or {text-fn humanize, value-fn identity}}]
     (as-> items $
       (if sort?
         (let [sorting (or sorting {:keyfn text-fn})]
           (sort-by (or (:keyfn sorting) identity)
                    (or (:comp sorting) compare)
                    $))
         $)
       (map (juxt text-fn value-fn) $)
       (if (or none? all?)
         (cons [(cond
                  all? all-text
                  none? "None") nil-opt] $)
         $)))

;;; Select options ; TODO maybe clean these up a bit, there's no reason they couldn't be inline in views and they don't rely much on `select-opts` which also could be cleaner and probably teased apart to serve these opts and the subscription ones with different functions.

   (def size-opts
     [["All Sizes" nil-opt]
      ["Shirts, dresses, etc" alpha-sizes]
      ["Shoes, hats, etc" (map str (concat (range 4 13 0.5) [13 14 15]))]
      ["Pants" (map str (concat (range 28 34) (range 34 48 2)))]
      ["Women's Jeans" (map str (range 1 23 2))]])

   (def role-opts
     (select-opts {:items roles}))

   (def ^:private botless-event-opts-config
     {:items [:sale :order :transfer :count]
      :all? true
      :all-text "All Events"})

   (def botless-event-opts
     (select-opts
      botless-event-opts-config))

   (def event-opts
     (select-opts
      (update botless-event-opts-config :items #(cons :bot %))))

   (def customer-tier-opts
     (cons ["None" nil-opt]
           (->> (seq tiers)
                (map (fn [[k v]]
                       [k (humanize v)]))
                (map reverse))))

   (def order-payment-method-opts
     (select-opts
      {:items [:order.payment-method/bank
               :order.payment-method/wire
               :order.payment-method/cod
               :order.payment-method/net
               :order.payment-method/credit-card]
       :none? true}))))
