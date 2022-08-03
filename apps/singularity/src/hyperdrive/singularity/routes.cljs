(ns hyperdrive.singularity.routes
  (:require [bidi.bidi :refer [match-route path-for]]
            [hyperdrive.queries :as q]))

(def ^:private routes
  ["/" {"" :page/home

        "landing/sales" :page/sales-landing
        "new-sale" :page/new-sale
        "choose-shop" :page/choose-shop
        ["sale-complete/" :id] :page/sale-complete
        ["register-count-complete/" :id] :page/register-count-complete
        ["register-adjustment-complete/" :id] :page/register-adjustment-complete

        "items" {"" :page/items
                 ["/" :id] {"" :page/item
                            "/history" :page/item-history}}
        "categories" {"" :page/categories
                      "/browse" :page/categories-browse}
        "manufacturers" :page/manufacturers
        "orders" [["" :page/orders]
                  ["/new" :page/new-order]
                  [["/" :id] :page/order]]
        "transfers" [["" :page/transfers]
                     ["/new" :page/new-transfer]
                     [["/" :id] :page/transfer]]
        "counts" [["" :page/counts]
                  ["/new" :page/new-count]
                  [["/" :id] :page/count]]

        "customers" {"" :page/customers
                     ["/" :id] {"" :page/customer
                                "/sales" :page/customer-sales
                                "/credit-account" :page/credit-account}}
        "customer-types" :page/customer-types
        "credit-accounts" :page/credit-accounts
        "gift-cards" {"" :page/gift-cards
                      ["/" :id] :page/gift-card}
        
        "employees" {"" :page/employees
                     ["/" :id] {"" :page/employee
                                "/timesheets" :page/employee-timesheets
                                "/sales" :page/employee-sales}}
        
        "sales" {"" :page/sales
                 ["/" :id] :page/sale}
        "grouped-sales" :page/grouped-sales
        "assets" :page/assets
        "grouped-assets" :page/grouped-assets
        "employee-performance" :page/employee-performance
        "stock-history" :page/stock-history
        "closing-counts" {"" :page/closing-counts
                          ["/" :id] {"" :page/closing-count
                                     "/sales" :page/closing-count-sales
                                     "/adjustments" :page/closing-count-adjustments}}
        "register-adjustments" :page/register-adjustments

        "shops" {"" :page/shops
                 ["/" :id] :page/shop}
        "payment-types" :page/payment-types

        "messages" :page/messages
        "commission" :page/commission

        "customer-display" :page/customer-display}])

(defn match [& args]
  (when-let [handler (apply match-route routes args)]
    (let [id (get-in handler [:route-params :id])]
      (when (or (not id) (q/entity id))
        handler))))

(defn path
  ([route] (path route nil))
  ([route params]
   {:post [(some? %)]}
   (let [unmatch (fn [params]
                   (apply path-for routes route (apply concat params)))] ; Make an associative-variadic function take a map.
     (cond
       (and params (map? params)) (unmatch {:id (:id params)}) ; Whole doc passed for convenience, get the id.
       params (unmatch {:id params}) ; Just id passed.
       :else (unmatch nil)))))
