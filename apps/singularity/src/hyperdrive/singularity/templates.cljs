(ns hyperdrive.singularity.templates
  (:require [clojure.string :as str]
            [hyperdrive.db :as db :refer [conn]]
            [hyperdrive.queries :as q]
            [hyperdrive.util :refer [fetch currency date image-url thumbnail-url]]
            [hyperdrive.singularity.models :as m]))

(def table-fix
  "table {font-size: 10pt;}") ; Override user-agent-stylesheet styling for tables. I don't know why this is necessary to make the lightspeed styling look right.

(defn head-common [title]
  (list
   [:title title]
   [:meta
    {:content
     "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no",
     :name "viewport"}]
   [:meta {:content "yes", :name "apple-mobile-web-app-capable"}]))

(defn- logo []
  (image-url (q/get-setting :settings/logo-image)))

(defn sale-receipt [{:keys [url-type title sale body-top body-bottom gift-receipt?]}]
  (if-not (:bulk? sale)
    [:html
     [:head
      (head-common title)
      [:style (str table-fix
                   "
@page {
    margin: 0;
}

body {
    background-color: white; /* ME: Fix that weird Chrome bug where the iframe content duplicates when printed. */
    font: normal 10pt 'Helvetica Neue', Helvetica, Arial, sans-serif;
    margin: 0;
            margin-right: .13in;
            padding: 1px; 
}

.store {
    page-break-after: always;
    margin-bottom: 40px;
}

.receipt {
    font: normal 10pt 'Helvetica Neue', Helvetica, Arial, sans-serif;
}

h1 {
    margin: .5em 0 0;
    font-size: 12pt;
    text-align: center;
}

p.date, p.copy {
    font-size: 9pt;
    margin: 0;
    text-align: center;
}

p.details {
    font-size: 10pt;
    text-align: left;
}

h2 {
    border-bottom: 1px solid black;
    text-transform: uppercase;
    font-size: 10pt;
    margin: .5em 0 0;
}

.header {
    text-align: center;
}

.header h3 {
    font-size: 12pt;
    margin: 0;
}

.header img {
    display: block;
    margin: 8px auto 4px;
    text-align: center;
}

table {
    margin: 0 0;
    width: 100%;
    border-collapse:collapse;
}

table thead th { text-align: left; }

table tbody th {
    font-weight: normal;
    text-align: left;
}

table td.amount, table th.amount {
    text-align: right;
    white-space: nowrap
}

table td.quantity, table th.quantity {
    text-align: center;
    white-space: nowrap;
}

table td.sku, table th.sku {
    padding-right: 10px;
    text-align: center;
    white-space: nowrap;
}

table th.description {
    width: 100%;
}

table td.amount {
    white-space: nowrap;
    padding-left: 10px;
}

table.totals { text-align: right; }
table.payments { text-align: right; }
table.spacer { margin-top: 1em; }
table tr.total td { font-weight: bold; }

table.sale { border-bottom: 1px solid black; }
table.sale thead th { border-bottom: 1px solid black; }

table div.line_description {
    text-align: left;
    font-weight: bold;
}

table div.line_note {
    text-align: left;
    padding-left: 10px;
}

table div.line_serial {
    text-align: left;
    font-weight: normal;
    padding-left: 10px;
}

table.workorders div.line_description {
    font-weight: normal;
    padding-left: 10px;
}

table.workorders div.line_note {
    font-weight: normal;
    padding-left: 10px;
}

table.workorders div.line_serial {
    font-weight: normal;
    padding-left: 20px;
}

table.workorders td.workorder div.line_note {
    font-weight: bold;
    padding-left: 0px;
}

p.thankyou {
    margin: 0;
    text-align: center;
}

.note { text-align: center; }

img.barcode {
    display: block;
    margin: 0 auto;
}

dl {
    overflow: hidden
}

dl dt {
    font-weight: bold;
    width: 80px;
    float: left
}

dl dd {
    border-top: 2px solid black;
    padding-top: 2px;
    margin: 1em 0 0;
    float: left;
    width: 180px
}

dl dd p { margin: 0; }

.strike {
  position: relative;
}
.strike:before {
  position: absolute;
  content: \"\";
  left: 0;
  top: 50%;
  right: 0;
  border-top: 1px solid;
}")]]
     (let [webstore-url (q/get-setting :settings/webstore-url)]
       [:body
        body-top
        [:div
         [:div.header
          [:img.logo
           {:width "225px",
            :src (logo)}]
          [:p webstore-url]]
         [:h1#receiptTypeTitle
          (if gift-receipt? "Gift" "Sales") " Receipt"]
         [:p.date (date (:date sale) :full)]
         [:p#receiptInfo.details
          "\n        Ticket: "
          [:span#receiptTicketId (m/sale:barcode sale)]
          [:br]
          "\n    Register: "
          [:span#receiptRegisterName (:name (fetch [:register (:register sale)]))]
          [:br]
          "    Employee: "
          [:span#receiptEmployeeName (m/employee:name (fetch [:employee (:employee sale)]))]
          [:br]
          ;; ME
          (when (:customer sale)
            [:span
             "Customer: "
             [:span#receiptCustomerName (m/customer:name (fetch [:customer (:customer sale)]))]
             [:br]])]
         (let [lines-table
               (fn [lines refund?]
                 [:table.sale.lines
                  [:thead
                   [:tr
                    [:th.description (if refund? "Refunded Item" "Item")]
                    [:th.quantity "#"]
                    (when-not gift-receipt?
                      [:th.amount "Price"])]]
                  [:tbody
                   (for [{:keys [price line-type] :as line} lines]
                     [:tr
                      [:th.description {:data-automation "lineItemDescription"}
                       [:div.line_description
                        (case line-type
                          :sku
                          (str (:name (fetch [:manufacturer (:manufacturer (fetch [:item (:item (fetch [:sku (:sku line)]))]))])) " " (fetch [:sku/name (:sku line) :full? true]))
                          ;; [:br]
                          ;; "\nClearance ATHLETICS SNAPBACK        "
                          :credit-account
                          "Credit Account Deposit"
                          :gift-card
                          (str "Gift Card: " (:code line)))]]
                      [:td.quantity {:data-automation "lineItemQuantity"} "1"]
                      (when-not gift-receipt?
                        [:td.amount {:data-automation "lineItemPrice"}
                         (case line-type
                           :sku
                           (let [original-price (fetch [:sku/price (:sku line) :inherit? true])]
                             (if (< price original-price)
                               [:span
                                [:span {:style "text-decoration: line-through"}
                                 (currency original-price)]
                                [:br]
                                (currency price)]
                               [:span (currency price)]))
                           :credit-account
                           (currency price)
                           :gift-card
                           (currency price))])])]])]
           [:div
            (when-let [refund-lines (seq (db/refund-expanded-lines (q/fact (:refund sale) :refund/sale-lines)))]
              [:div
               (lines-table refund-lines true)
               [:br]])
            (when-let [lines (:lines sale)]
              (lines-table lines false))])
         (when-not gift-receipt?
           [:div
            [:table.totals
             [:tbody#receiptSaleTotals
              [:tr
               [:td {:width "100%"} "Subtotal"]
               [:td#receiptSaleTotalsSubtotal.amount (currency (fetch [:sale/subtotal (:id sale)]))]]
              (let [discount-amount (fetch [:sale/discount-amount (:id sale)])]
                (when-not (zero? discount-amount)
                  [:tr
                   [:td {:width "100%"} "Discount"]
                   [:td.amount (currency discount-amount)]]))
              [:tr
               [:td
                {:width "100%", :data-automation "receiptSaleTotalsTaxName"}
                "Tax (8%)"]
               [:td.amount
                {:data-automation "receiptSaleTotalsTaxValue"}
                (currency (fetch [:sale/tax (:id sale)]))]]
              ;; [:tr
              ;;  [:td {:width "100%"} "Total Tax"]
              ;;  [:td#receiptSaleTotalsTax.amount "$1.76"]]
              [:tr.total
               [:td "Total"]
               [:td#receiptSaleTotalsTotal.amount (currency (fetch [:sale/total (:id sale)]))]]]]
            (let [transactions-table
                  (fn [payments-or-refunds transactions]
                    [:div
                     [:h2 (case payments-or-refunds
                            :payments "Payments"
                            :refunds "Refunds")]
                     [:table#receiptPayments.payments
                      (let [row (fn [text amount]
                                  [:tr
                                   [:td {:width "100%"} text]
                                   [:td.amount (currency amount)]])]
                        [:tbody
                         "<!-- NOT Cash Payment -->"
                         "<!--  NOT Customer Account -->"
                         (for [{:keys [:transaction/line-type :transaction/source :transaction/amount]}
                               (if (= payments-or-refunds :refunds)
                                 (remove #(= (:transaction/line-type %) :type/exchange) transactions)
                                 transactions)]
                           (row
                            (case line-type
                              :type/payment-type (:name (fetch [:payment-type source]))
                              :type/credit-account "Credit Account"
                              :type/gift-card "Gift Card"
                              :type/exchange "Exchange")
                            amount))
                         [:tr [:td {:colspan "2"}]]])]])]
              [:div
               (when-let [sale-transactions (:transactions sale)]
                 [:div
                  (transactions-table :payments sale-transactions)
                  [:br]])
               (when-let [refund-transactions (q/fact (:refund sale) :refund/transactions)]
                 (transactions-table :refunds refund-transactions))])])
         [:p#receiptNote.note
          "Thanks for shopping with us!"
          [:br]
          webstore-url
          [:br]
          [:br]
          (q/get-setting :settings/sale-receipt-footer)]
         [:p#receiptThankYouNote.thankyou
          "\n            Thank You!\n        "]]
        body-bottom])]
    ;; TODO!! All of this is unacceptably sloppy, clean it up after I change sales to have quantities and I do the bulk buy app.
    ;; - have different lines for item color like brandboom does
    ;; - the styles and fonts don't work. You can't link to external stylesheets but you can an import of fonts within a <style> element or something: https://storyports.com/tips-and-tricks-for-using-google-fonts-in-emails-bonus-5-beautiful-font-pairings
    ;;   - Update: hardhoded XXXXXXX after I got rid of `make-url`. I wouldn't want to link to hyperdrive from a receipt anyway though, so figure out another way when I fix this.
    ;; - I can't use the placeholder image externally so I just have a when-let around the item image but when it's missing the rest of the line is too far left
    [:html
     ;; TODO head is copypasta from singularity.frontend
     [:head
      [:title title]
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1, shrink-to-fit=no"}]
      [:link {:type "text/css" :href "https://fonts.googleapis.com/css?family=Nunito:200,200i,300,300i,400,400i,600,600i,700,700i,800,800i,900,900i" :rel "stylesheet"}]
      [:link {:type "text/css" :href "https://XXXXXXXX/css/main.css" :rel "stylesheet"}]]
     [:body
      body-top
      [:div.row
       [:div.col
        [:div.text-center
         [:img {:width "225px"
                :src (logo)}]]
        [:br]
        (when (:customer sale)
          [:div [:strong "Customer: "] (m/customer:name (fetch [:customer (:customer sale)]))])
        [:div [:strong "Date: "] (date (:date sale))]]]
      [:br]
      [:div.row
       [:div.col
        [:table.table.table-bordered.table-sm
         [:tbody
          (apply
           concat
           (let [sku-lines (filter #(= (:line-type %) :sku) (:lines sale))]
             (for [item-id (q/items-for-skus (map :sku sku-lines))
                   :let [item (fetch [:item item-id])]]
               (let [span (fn [x]
                            {:style (str "width: " x "%")})]
                 (list
                  [:tr
                   [:td {:colspan "100%"}
                    (when-let [image (fetch [:item/primary-image (:id item)])]
                      [:img.d-inline-block {:src (thumbnail-url image 72)}])
                    [:div.d-inline-block.align-middle.ml-4.small
                     (:name (fetch [:manufacturer (:manufacturer item)]))
                     [:br]
                     (:name item)
                     [:br]
                     [:strong "MSRP: "] (currency (or (:msrp-price item) (:price item)))]]] ; TODO inline price inheritance logic
                  [:tr
                   [:th "Options"]
                   (for [size (:sizes item)]
                     [:th.text-center (span 6) size])
                   [:th (span 9) "Quantity"]
                   [:th (span 9) "Price"]
                   [:th (span 9) "Total"]]
                  (let [price (or (:msrp-price item) (:price item)) ; TODO inline price inheritance logic
                        quantity (count (filter #(= item-id (:item %)) (fetch [:skus (map :sku sku-lines)])))]
                    [:tr
                     [:td]
                     (for [size (:sizes item)]
                       [:td.text-center (span 6)
                        (count (filter #(and (= item-id (:item %)) (= size (:size %))) (fetch [:skus (map :sku sku-lines)])))])
                     [:td quantity]
                     [:td (currency price)]
                     [:td (currency (* quantity price))]]))))))]]]]
      [:br]
      [:div.row
       [:div.col-6.offset-6
        [:table.table.table-sm
         [:tbody
          (for [[label value] [["Subtotal" (fetch [:sale/subtotal (:id sale)])]
                               ["Tax" (fetch [:sale/tax (:id sale)])]
                               ["Shipping" (:sale/shipping-cost sale)]
                               ["Total" (fetch [:sale/total (:id sale)])]]]
            [:tr
             [:td.font-weight-bold label]
             [:td.text-right (currency value)]])]]]]
      body-bottom]]))
