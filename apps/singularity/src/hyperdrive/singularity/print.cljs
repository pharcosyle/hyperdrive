(ns hyperdrive.singularity.print
  (:require-macros [hiccups.core :refer [html]])
  (:require [clojure.string :as str]
            hiccups.runtime
            [hyperdrive.queries :as q]
            [hyperdrive.util :as u :refer [fetch currency date]]
            [hyperdrive.singularity.models :as m]
            [hyperdrive.singularity.templates :as t :refer [table-fix]]))

(def ^:private title "Print")
(def ^:private head-common (t/head-common title))
(def ^:private trigger-print "window.focus(); print();")

(defn sale [sale & {:keys [gift-receipt?]}]
  (html
   (t/sale-receipt
    {:url-type :relative
     :title title
     :sale sale
     :body-bottom
     (when-not (:bulk? sale)
       (list
        ;; REMOVED
        ;; [:img#barcodeImage.barcode
        ;;  {:src "/barcode.php?type=receipt&number=220000112033",
        ;;   :width "250",
        ;;   :height "50"}]
        ;; [:script
        ;;  {:src
        ;;   "//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"}]
        ;; [:script
        ;;  "window.jQuery || document.write('<script src=\"https://d2o5po5b88zacb.cloudfront.net/dist/assets/js/jquery-2.1.1.min.js.gz\"><\\/script>')"]
        ;; [:script
        ;;  {:src
        ;;   "https://d2o5po5b88zacb.cloudfront.net/dist/assets/lshash-756da99/js/printtemplates.js.gz",
        ;;   :type "text/javascript"}]
        ;; [:script
        ;;  {:type "text/javascript"}
        ;;  "\n\t\t\t\t\tvar printer_name = \"report\";\n\t\t\n\t\ttry {\n\t\t\twindow.focus();\n\t\t\tmerchantos.print.print(printer_name);\n\t\t} catch (e) {\n\t\t\t// do nothing\n\t\t}\n\t"]

        ;; ME
        [:br]
        [:div {:style "text-align: center;"}
         [:svg#my-barcode.barcode {:style "margin: 0 auto; display: block;"}]]

        [:script {:src "/js/shared.js"}]
        [:script {:src "/js/print.js"}]
        [:script
         (str "hyperdrive.singularity.modules.print.render_barcode(\"#my-barcode\", " (m/sale:barcode sale) ", {height: 75});")]))
     :gift-receipt? gift-receipt?})))

;; Template: https://us.merchantos.com/API/Account/57195/DisplayTemplate/OrderLineAsLabel.html?template=ItemLabel&print=1&shopID=1&labelSize=2.25x1.25&all_labels=1&orderID=3478
(defn labels [sku-amounts]
  (html
   [:html
    [:head
     head-common
     ;; REMOVED: replaced with head styles below.
     ;; [:link
     ;;  {:type "text/css",
     ;;   :rel "stylesheet",
     ;;   :media "all",
     ;;   :href "/assets/css/labels.css"}]
     [:style "
@page {
  margin: 0; }

body {
  font-size: 100%;
  font-family: \"proxima-nova-alt\", Arial,Helvetica,sans-serif;
  margin: 0;
  color: #000; }

* {
  box-sizing: border-box; }

.label {
  width: 2.25in;
  height: 1.25in;
  margin: 0;
  position: relative;
  overflow: hidden;
  page-break-after: always; }
  .label:last-child {
    page-break-after: auto; }
  .label article {
    padding: .01in .08in 0 .01in;
    overflow: hidden;
    height: 56pt; }
  .label h1 {
    margin: 0 0 .05in;
    text-align: center;
    font-weight: 400;
    font-size: .75em;
    text-transform: uppercase;
    position: relative;
    padding: 5px 5px 2px;
    border-bottom: 2px solid #000;
    background-color: #000;
    color: #fff; }
  .label .price {
    float: left;
    margin-right: .1in; }
    .label .price p {
      margin: 0;
      text-align: center;
      line-height: 1; }
      .label .price p.msrp {
        font-size: .7em; }
      .label .price p.saleprice {
        font-size: 2em;
        line-height: 1;
        font-weight: 400; }
    .label .price sup {
      line-height: 1;
      top: auto; }
      .label .price sup.currency {
        font-weight: 400;
        font-size: .55em;
        margin: -2px 1px 0 0;
        vertical-align: top;
        display: inline-block; }
      .label .price sup.cents {
        font-weight: 600;
        font-size: .45em;
        margin: -8px 0 0 3px;
        vertical-align: super;
        border-bottom: 2px solid #000; }
  .label .description {
    font-size: 9pt;
    line-height: 1.2;
    margin: 0 0;
    overflow: hidden;
    font-weight: 500;
    word-wrap: break-word;
    text-overflow: ellipsis; }
  .label.notitle article {
    height: 56pt; }
  .label.notitle h1 {
    display: none; }
  .label .barcode {
    width: 100%;
    height: 0.4in;
    padding: .02in 0;
    text-align: center;
    z-index: 10; }
    .label .barcode img {
      margin: 0 auto;
      height: 0.4in; }
      .label .barcode img.ean8 {
        display: none; }
  .label.size200x100 {
    width: 2in;
    height: 1in; }
    .label.size200x100 .msrp {
      display: none; }
    .label.size200x100.notitle .msrp {
      display: block; }
    .label.size200x100 h1 {
      padding: 2px 1px 1px;
      border-bottom-width: 1px;
      font-size: .65em; }
    .label.size200x100 article {
      height: 40pt; }
  .label.size125x100 {
    width: 1.25in;
    height: 1in; }
    .label.size125x100 h1, .label.size125x100 .msrp {
      display: none; }
    .label.size125x100 .price {
      float: left;
      font-size: 7pt;
      margin: 2px 5px 0 0; }
      .label.size125x100 .price sup.currency {
        vertical-align: inherit;
        margin: 0;
        font-size: 1em; }
      .label.size125x100 .price sup.cents {
        margin: 0 0 0 2px;
        font-size: .6em;
        vertical-align: text-top; }
    .label.size125x100 article {
      height: 41pt;
      overflow: hidden; }
    .label.size125x100 .description {
      margin: 0;
      line-height: 1.1;
      overflow: visible; }
    .label.size125x100 .barcode img {
      width: 1.25in;
      height: 30px;
      margin: 0 0 0 -8px; }
      .label.size125x100 .barcode img.ean {
        display: none; }
      .label.size125x100 .barcode img.ean8 {
        display: block; }
  .label.size220x50 {
    width: 2.20in;
    height: .5in;
    position: relative; }
    .label.size220x50:not(:first-of-type) {
      margin-top: 1px;
    }
    .label.size220x50 h1, .label.size220x50 .msrp {
      display: none; }
    .label.size220x50 article {
      position: absolute;
      right: 0;
      bottom: 0;
      top: 0;
      width: 1in;
      height: auto;
      padding: 0;
      overflow: hidden; }
    .label.size220x50 .price {
      position: absolute;
      bottom: 6px;
      right: 0;
      font-size: 6pt;
      margin: 0; }
      .label.size220x50 .price sup.currency {
        vertical-align: inherit;
        margin: 0;
        font-size: 1em; }
      .label.size220x50 .price sup.cents {
        margin: 0 0 0 2px;
        font-size: .6em;
        vertical-align: text-top; }
    .label.size220x50 .description {
      position: absolute;
      top: 0;
      right: 0;
      width: .7in;
      font-size: 7.5pt;
      max-height: 2.2em;
      margin: 0;
      line-height: 1.1;
      overflow: hidden; }
    .label.size220x50 .barcode {
      position: absolute;
      width: .75in;
      top: 0;
      left: 0;
      bottom: 0;
      background-color: #fff;
      overflow: hidden; }
      .label.size220x50 .barcode img {
        position: absolute;
        left: -11px;
        top: 2px;
        width: 96px; }
        .label.size220x50 .barcode img.ean {
          display: none; }
        .label.size220x50 .barcode img.ean8 {
          display: block; }"]]
    [:body
     [:div.labels
      (for [sku (->> sku-amounts
                     (mapcat #(repeat (val %) (key %))) ;; Turn the frequency map into a collection of sku IDs with multiples.
                     (map #(fetch [:sku %])))]
        [:div.label.size225x125.notitle
         [:article
          [:h1 "none"]
          (let [price (fetch [:sku/price (:id sku) :inherit? true])]
            [:div.price
             (let [[dollars cents] (-> price
                                       str
                                       (str/split #"\."))]
               [:p.saleprice [:sup.currency "$"] dollars [:sup.cents (or cents "00")]])
             (let [msrp (or (q/fact (:id sku) :sku/msrp-price)
                            (q/fact (:id sku) :sku/item :item/msrp-price))]
               (when (< price msrp)
                 [:p.msrp "MSRP "(currency msrp)]))])
          [:p.description
           (:name (fetch [:manufacturer (:manufacturer (fetch [:item (:item sku)]))])) " "
           (:name (fetch [:item (:item sku)])) " "
           [:strong (fetch [:sku/name (:id sku)])]]]
         ;; Entirely ME
         (when-let [image (u/image-url (fetch [:sku/primary-image (:id sku) :inherit? true]) :width 60 :height 60 :fit :crop :sharpen :default)]
           [:div {:style "position: absolute; left: 5px; bottom: 5px;"}
            [:img {:src image}]])
         [:footer.barcode
          [:img.ean.my-barcode
           {:style "position: absolute; right: 5px; bottom: 5px; height: 60px; width: 140px;"
            :jsbarcode-value (m/sku:barcode sku)
            ;; :jsbarcode-width 2
            ;; :jsbarcode-fontsize 28
            :jsbarcode-margin 0}]
          ;; REMOVED
          ;; [:img.ean8
          ;;  {:src
          ;;   "/barcode.php?type=label&number=210000180148&ean8=1&noframe=1"}]
          ;; [:img.ean
          ;;  {:src
          ;;   "/barcode.php?type=label&number=210000180148&noframe=1"}]
          ]])]

     ;; ME
     [:script {:src "/js/shared.js"}]
     [:script {:src "/js/print.js"}]
     [:script "hyperdrive.singularity.modules.print.render_barcode('.my-barcode').init();"]
     [:script
      "setTimeout(function() { " trigger-print "}, 1000); // Give JsBarcode time to do its thing."]

     ;; REMOVED
     ;; [:script
     ;;  {:src
     ;;   "//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"}]
     ;; [:script
     ;;  "window.jQuery || document.write('<script src=\"https://d2o5po5b88zacb.cloudfront.net/dist/assets/js/jquery-2.1.1.min.js.gz\"><\\/script>')"]
     ;; [:script
     ;;  {:src
     ;;   "https://d2o5po5b88zacb.cloudfront.net/dist/assets/lshash-d19f040/js/printtemplates.js.gz",
     ;;   :type "text/javascript"}]
     ;; [:script
     ;;  {:type "text/javascript"}
     ;;  "\n\t\t\t\t\tvar printer_name = \"report\";\n\t\t\n\t\ttry {\n\t\t\twindow.focus();\n\t\t\tmerchantos.print.print(printer_name);\n\t\t} catch (e) {\n\t\t\t// do nothing\n\t\t}\n\t"]
     ]]))

(def ^:private register-styles "
@page { margin: 0px; }
body {
  font: normal 10pt 'Helvetica Neue', Helvetica, Arial, sans-serif;
  margin: 0;
  padding: 1px; <!-- You need this to make the printer behave -->
}
h2 {
  text-align: center;
  font-size: 12pt;
  margin-bottom: 5px;
}
h3 {
  text-align: center;
  font-size: 10pt;
}")

;; Template: https://us.merchantos.com//API/Account/57195/DisplayTemplate/Employee/65.html?template=RegisterOpen&print=1&page_width=auto&page_height=2000mm
(defn open-drawer [register]
  (html
   [:html
    [:head
     head-common
     [:style register-styles]]
    [:body
     [:h2 "Open Cash Drawer"]
     [:h3 (date (u/now) :full)]
     [:p
      "Location: " (:name (fetch [:shop (:shop (fetch [:register register]))]))
      [:br]
      "Register: " (:name (fetch [:register register]))
      [:br]
      "Employee: " (m/employee:name (fetch [:user]))
      [:br]]
     [:script trigger-print]]]))

;; Template: https://us.merchantos.com/API/Account/57195/DisplayTemplate/RegisterWithdraw/9586.html?template=RegisterWithdraw&print=1&no_auto_print=1&page_width=auto&page_height=2000mm
(defn register-adjustment [reg-adj]
  (html
   [:html
    [:head
     head-common
     [:style register-styles]]
    [:body
     [:h2#receiptTypeTitle "Cash Drawer Adjustment"]
     [:h3 (date (:date reg-adj) :full)]
     [:p#receiptInfo
      "Added: "
      [:span#receiptAdded (currency (:amount reg-adj)) " " (:name (fetch [:payment-type (:payment-type reg-adj)]))]
      [:br]
      "Location: "
      [:span#receiptLocation (:name (fetch [:shop (:shop (fetch [:register (:register reg-adj)]))]))]
      [:br]
      "Register: "
      [:span#receiptRegisterName (:name (fetch [:register (:register reg-adj)]))]
      [:br]
      "Employee: "
      [:span#receiptEmployeeName (m/employee:name (fetch [:employee (:employee reg-adj)]))]
      [:br]]]]))

;; Template: https://us.merchantos.com/API/Account/57195/DisplayTemplate/RegisterCount/4760.html?template=RegisterCount&print=1&no_auto_print=1&page_width=auto&page_height=2000mm
(defn closing-count [reg-count]
  (html
   [:html
    [:head
     head-common
     [:style (str register-styles table-fix "
table th {
  font-weight: normal;
}
table td {
  border-top: 1px solid #888;
}")]]
    [:body
     [:h2#receiptTypeTitle "Register Count"]
     [:p
      "\nLocation: "
      [:span#receiptLocation {:notranslate "notranslate"} (:name (fetch [:shop (:shop (fetch [:register (:register reg-count)]))]))]
      [:br]
      "\nRegister: "
      [:span#receiptRegisterName
       {:notranslate "notranslate"}
       (:name (fetch [:register (:register reg-count)]))]
      [:br]
      [:br]
      [:span#openedSection
       [:u "Opened"]
       [:br]
       "\nDate: " (date (:open-date reg-count) :full)
       [:br]
       "\nEmployee: "
       [:span#receiptEmployeeNameOpened
        {:notranslate "notranslate"}
        (m/employee:name (fetch [:employee (:open-employee reg-count)]))]
       [:br]
       [:br]]
      [:span#closedSection
       [:u "Closed"]
       [:br]
       "\nDate: " (date (:date reg-count) :full)
       [:br]
       "\nEmployee: "
       [:span#receiptEmployeeNameClosed
        {:notranslate "notranslate"}
        (m/employee:name (fetch [:employee (:employee reg-count)]))]
       [:br]
       [:br]
       "\n\nNotes: "
       [:span#registerCountNotes {:notranslate "notranslate"}
        (:notes reg-count)]
       [:br]]]
     [:span#countsSection [:u "Counts"]]
     [:p
      [:table
       [:tr [:th "Type"] [:th "Total"] [:th "Counted"] [:th "+/-"]]
       (for [line (q/report:register-count (:id reg-count))]
         [:tr
          [:td (:name (fetch [:payment-type (:payment-type line)]))]
          [:td (currency (:total line))]
          [:td (currency (:counted line))]
          [:td (currency (:diff line))]])]]]]))
