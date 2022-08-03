(ns hyperdrive.singularity.views
  (:require-macros [hiccups.core :refer [html]])
  (:require [clojure.string :as str]
            [cljs.reader :refer [read-string]]
            hiccups.runtime
            [reagent.core :as r]
            [reagent.dom :as dom]
            [re-frame.core :refer [subscribe dispatch dispatch-sync] :rename {subscribe sub}]
            [re-frame.utils :refer [dissoc-in]]
            [hyperdrive.queries :as q]
            [hyperdrive.util :as u :refer [val-or-deref listen fetch spaces yes-no plus-minus percent-change currency date elapsed interpose-for humanize image-url thumbnail-url chat-url fixture]]
            [hyperdrive.singularity.models :as m]
            [hyperdrive.singularity.print :as print]
            [hyperdrive.singularity.routes :refer [path]]
            [goog.functions :refer [debounce throttle]]
            ["jquery" :as jq]
            ["startbootstrap-sb-admin-2/vendor/bootstrap/js/bootstrap.bundle"]
            ["bootstrap4-tagsinput-douglasanpa/tagsinput"]
            ["flatpickr" :as flatpickr]
            ["chart.js" :as Chart]
            ["aws-amplify-react" :refer [Authenticator SignUp Greetings]]))


(def no-href "#")

(def gen-id!
  (let [counter (atom 0)]
    (fn []
      (str "gen-" (swap! counter inc)))))

(defn jq-node [react-cmp]
  (-> react-cmp dom/dom-node jq))

(defn prop [react-cmp key]
  (-> react-cmp r/props key))

(defn atom-bindings [a]
  {:val a
   :save #(reset! a %)})



;;;; Input components

;;; Text

(defn text-input []
  (let [editing? (r/atom false)
        internal (r/atom nil)]
    (fn [{:keys [tag val save save-on-blur? displayize] :or {tag :input, displayize identity}} attrs]
      (let [focus (fn [focused?]
                    (reset! editing? focused?))
            save-fn (fn [v]
                      (save (when-not (str/blank? v) v)))]
        [tag (merge {:type "text"}
                    attrs
                    {:class (str "form-control " (:class attrs))
                     :value (if @editing?
                              @internal
                              (displayize (val-or-deref val)))
                     :on-focus (fn []
                                 (reset! internal (displayize (val-or-deref val)))
                                 (focus true))
                     :on-change (fn [e]
                                  (reset! internal (-> e .-target .-value))
                                  (when-not save-on-blur?
                                    (save-fn @internal)))
                     :on-blur (fn []
                                (when save-on-blur?
                                  (save-fn @internal))
                                (focus false))})]))))

(defn input-addon [position addon-body bindings input-attrs attrs]
  (let [addon [:div {:class (case position
                              :prepend "input-group-prepend"
                              :append "input-group-append")}
               [:span.input-group-text addon-body]]]
    [:div.input-group attrs
     (when (= position :prepend) addon)
     [text-input bindings input-attrs]
     (when (= position :append) addon)]))

(defn textarea [bindings attrs]
  [text-input (assoc bindings :tag :textarea) attrs])

(defn search [bindings]
  [input-addon :append [:i.fas.fa-search]
   (update bindings :save debounce 100)
   {:placeholder "Search for..."
    :type "search"}])

(defn money-input [bindings attrs]
  [input-addon
   :prepend [:i.fas.fa-dollar-sign]
   (assoc bindings :displayize #(currency % :no-sign? true :coerce-nil? false))
   attrs])

(defn email-input [bindings]
  [input-addon :append [:i.fas.fa-envelope] bindings {:type "email"}])

(defn phone-input [bindings]
  [input-addon :append [:i.fas.fa-phone] bindings {:type "tel"}])

(defn url-input [bindings]
  [input-addon :append [:i.fas.fa-link] bindings {:type "url"}])

(defn percent-input [bindings]
  [input-addon :append [:i.fas.fa-percent] bindings])



;;; Other

(defn checkbox* []
  (let [id (gen-id!)]
    (fn [{:keys [val save inline? show-as]} child attrs]
      [:div.custom-control
       (merge attrs
              {:class (str (:class attrs) " "
                           (case show-as
                             :checkbox "custom-checkbox"
                             :switch "custom-switch")
                           " "
                           (when inline?
                             "custom-control-inline"))})
       [:input.custom-control-input
        {:id id
         :type "checkbox"
         :on-change #(save (-> % .-target .-checked))
         :checked (boolean (val-or-deref val))}]
       [:label.custom-control-label {:for id}
        child]])))

(defn checkbox [bindings child attrs]
  [checkbox* (assoc bindings :show-as :checkbox) child attrs])

#_(defn switch [bindings child attrs]
    [checkbox* (assoc bindings :show-as :switch) child attrs])

(defn radio []
  (let [id (gen-id!)]
    (fn [{:keys [val save name option inline?]} child]
      [:div.custom-control.custom-radio (when inline?
                                          {:class "custom-control-inline"})
       [:input.custom-control-input
        {:id id
         :type "radio"
         :name name
         :on-change #(save option)
         :checked (= (val-or-deref val) option)}]
       [:label.custom-control-label {:for id}
        child]])))

;; - HTML select elements will, by default, use the text of a selected option as the -val if there isn't one, thus I have to use a special `nil-opt` to normalize.
(defn select [{:keys [val save opts multiple?]}]
  (let [placeholder "--placeholder--"]
    (into
     [:select.custom-select
      (merge {:value (let [v (val-or-deref val)]
                       (if multiple?
                         (map pr-str v)
                         (if v
                           (pr-str v)
                           (if (not= (second (first opts)) u/nil-opt)
                             placeholder u/nil-opt))))
              :on-change (let [this (r/current-component)]
                           (fn [e]
                             (save (if multiple?
                                     (let [v (.val (jq-node this))]
                                       (if (seq v) (mapv read-string v) nil))
                                     (let [v (-> e .-target .-value)]
                                       (if (= v u/nil-opt) nil (read-string v)))))))}
             (when multiple?
               {:multiple true}))]
     (letfn [(option [opt]
               ;; Can pass value->text pair or just a single value used for both. 
               (if-not (sequential? opt)
                 (option [opt opt])
                 ;; Can nest options and create optgroups.
                 (let [[text value-or-more] opt]
                   (if (sequential? value-or-more)
                     (into
                      [:optgroup {:label text}]
                      (map option value-or-more))
                     [:option {:value ((if (= value-or-more u/nil-opt)
                                         identity pr-str)
                                       value-or-more)}
                      text]))))]
       (let [options (map option opts)]
         (if multiple?
           options
           (cons [:option {:value placeholder
                           :disabled true}
                  "Select..."]
                 options)))))))

(defn tags-input* []
  (let [tags-in-refresh (atom #{})
        sync (fn [this]
               (let [$el (jq-node this)]
                 (.tagsinput $el "removeAll")
                 (doseq [tag (prop this :val)]
                   (swap! tags-in-refresh conj tag)
                   (.tagsinput $el "add" tag))))
        save-fn (fn [this]
                  ((prop this :save) (let [tags (.val (jq-node this))]
                                       (when-not (= tags "") (str/split tags #",")))))]
    (r/create-class
     {:component-did-mount
      (fn [this]
        (doto (jq-node this)
          (.tagsinput (clj->js {:trimValue true
                                :tagClass "bg-primary border-primary rounded-sm mr-1 mb-1"
                                ;; :allowDuplicates false ; This is the default.
                                :onTagExists (fn [_ $tag]
                                               (-> $tag .hide .fadeIn))}))
          (.on "itemAdded"
               (fn [e]
                 (let [tag (.-item e)]
                   (if (contains? @tags-in-refresh tag)
                     (swap! tags-in-refresh disj tag)
                     (save-fn this)))))
          (.on "itemRemoved"
               (fn [_]
                 (save-fn this))))
        (sync this))
      :component-did-update sync
      :reagent-render
      (fn []
        [:input {:type "text"}])})))

(defn tags-input [props]
  [tags-input* (update props :val val-or-deref)])

(defn datepicker* []
  (let [sync (fn [this]
               ;; Works with timestamps and nil.
               (.setDate (.-_flatpickr (dom/dom-node this))
                         ((if (prop this :range?) clj->js identity) ; Convert to js array for range picker.
                          (prop this :val))))]
    (r/create-class
     {:component-did-mount
      (fn [this]
        (flatpickr
         (dom/dom-node this)
         (clj->js (merge
                   ;; Use this instead of onChange to make sure there's only one save event fired, it just makes life a little simpler (saving won't update for intermediate values, etc.)
                   {:onClose (fn [dates]
                               ((prop this :save)
                                (when-let [v (seq (map #(.getTime %) dates))] ; Save nil if nothing was selected for both regular pickers and range pickers.
                                  (if (prop this :range?)
                                    (as-> v $
                                      ;; Might be one if the picker was closed before selecting the end of the range or the user selected the same day for the start and end.
                                      (if (= (count $) 1)
                                        [(first $) nil] (vec $))
                                      ;; Add a day so the range goes to the end but subtract one millisecond so the day part is still what the user expects to see.
                                      (update $ 1 (fn [end]
                                                    (when end
                                                      (if (str/ends-with? (str end) "999") ; Unchanged end date will keep incrememing on save/close. Hacky way to prevent this, though it does mean that a date of exactly 00:00 and 00 milliseconds won't appear in either of two back-to-back ranges.
                                                        end
                                                        (+ end (- u/one-day 1)))))))
                                    (first v)))))
                    ;; Create an ad-hoc clear button.
                    :onReady (fn [_ _ instance]
                               (let [$cal (jq (.-calendarContainer instance))]
                                 (when (-> $cal (.find ".flatpickr-clear") .-length (< 1))
                                   (.append $cal (html [:a.flatpickr-clear {:href no-href} "Clear"]))
                                   (.on (.find $cal ".flatpickr-clear") "click" (fn []
                                                                                  (.clear instance)
                                                                                  (.close instance))))))}
                   (when (prop this :time?)
                     {:enableTime true
                      :dateFormat "Y-m-d h:i K"}) ; Match the :full date formatter from util.
                   (when (prop this :range?)
                     {:mode "range"})
                   ;; As of this writing this option is only necessary as a workaround to make the picker not error when in a bootstrap modal. https://github.com/flatpickr/flatpickr/issues/1730
                   ;; Update: still true event after the BS v4 upgrade.
                   (when (prop this :static?)
                     {:static true}))))
        (sync this))
      :component-did-update sync
      :reagent-render
      (fn []
        [:input.form-control])})))

(defn datepicker [bindings]
  [datepicker* (update bindings :val val-or-deref)])

(defn datepicker-range [bindings]
  [datepicker (assoc bindings :range? true)])

(defn file-input []
  (let [id (gen-id!)]
    (fn [on-change]
      [:div.custom-file
       [:input.custom-file-input {:id id
                                  :type "file"
                                  :on-change on-change}]
       [:label.custom-file-label {:for id}
        "Choose file..."]])))






;;;; Generic components

(defn form-group
  ([child] (form-group nil child))
  ([label-text child]
   [:div.form-group
    (when label-text
      [:label.mb-1 label-text])
    child]))

(defn tooltip []
  (r/create-class
   {:component-did-mount
    (fn [this]
      (.tooltip (jq-node this)
                (clj->js (merge {:title (prop this :text)}
                                (when-let [html? (prop this :html?)]
                                  {:html html?})
                                (when-let [placement (prop this :placement)]
                                  {:placement placement})))))
    :component-did-update
    ;; Doesn't sync :html or :placement props, no need at present.
    (fn [this]
      (-> (jq-node this)
          (.tooltip "hide") ; Hide the tooltip in case it's shown when the update happens.
          (.attr "title" (prop this :text))
          (.tooltip "_fixTitle")))
    :reagent-render
    (fn [{:keys [cmp]}]
      cmp)}))

(defn help [options]
  [tooltip (assoc options :cmp [:i.fas.fa-info-circle])])

;; It doesn't seem like I need this to make dropdowns work but I'm suspicous and keeping it around.
(defn dropdown-toggle []
  (r/create-class
   {:component-did-mount
    (fn [this]
      (.dropdown (jq-node this)))
    :reagent-render
    identity}))

(defn modal [& {:keys [size title body footer]}]
  [:div.modal-dialog {:class (case size
                               :lg "modal-lg"
                               :xl "modal-xl"
                               nil)} ; .modal-md is the BS default.
   [:div.modal-content
    [:button.close.ml-auto.pr-3.pt-2 {:type "button"
                                      :data-dismiss "modal"}
     [:span "×"]]
    [:div.modal-header.justify-content-center.pt-0
     (when title
       [:h1.h4.modal-title title])]
    [:div.modal-body
     [:div.container-fluid
      body]]
    (if footer
      [:div.modal-footer.justify-content-center.mb-3
       footer])]])

(defn panel [header & body]
  [:div.card.shadow.mb-4
   [:div.card-header
    [:h2.h6.m-0.font-weight-bold.text-primary header]]
   (into
    [:div.card-body]
    body)])

(defn tile [attrs text icon small-text]
  [:a.tile.card.shadow.py-2.text-decoration-none attrs
   [:div.card-body
    [:div.row.no-gutters.align-items-center
     [:div.col.mr-2
      (when small-text
        [:div.text-xs.font-weight-bold.text-primary.text-uppercase.mb-1
         small-text])
      [:div.h5.mb-0.font-weight-bold.text-gray-800 text]]
     [:div.col-auto [:i.fas.fa-2x.text-gray-300 {:class icon}]]]]])

(defn loading-button [{:keys [tag loading? hide-children?] :or {tag :button}} attrs child]
  [tag (merge {:type "button"}
              attrs
              {:class (str "btn " (:class attrs))
               :disabled (or (val-or-deref loading?) (val-or-deref (:disabled attrs)))
               :on-click (fn [& args]
                           ;; Can't simply reply on the button's "disabled" attribute being changed in time to prevent additional clicks.
                           (when-not (val-or-deref loading?)
                             (when-let [on-click (:on-click attrs)]
                               (apply on-click args))))})
   (let [spinner [:i.fas.fa-spinner.fa-spin.fa-lg]]
     (if (and hide-children? (val-or-deref loading?))
       spinner
       [:span child (when (val-or-deref loading?)
                      [:span (spaces 3) spinner])]))])

;; Useful for "create" buttons and buttons with non-idempotent effects. Prefer `loading-button` when there's some way to know that the action is complete.
(defn one-time-button []
  (let [used? (r/atom false)]
    (fn [attrs child]
      [loading-button
       {:loading? used?}
       (-> attrs
           (dissoc :tag)
           (assoc :on-click (fn [& args]
                              (reset! used? true)
                              (when-let [on-click (:on-click attrs)]
                                (apply on-click args)))))
       child])))

(defn refresher []
  (let [refresh (r/atom 0)
        interval-id (js/setInterval #(swap! refresh inc) 30000)]
    (r/create-class
     {:component-will-unmount #(js/clearInterval interval-id)
      :reagent-render (fn [render]
                        @refresh
                        [:span (render)])})))

(defn pill-tabs [initial]
  (let [active-tab (r/atom initial)]
    (fn [_ & tabs]
      [:div
       (let [tab-button (fn [tab body]
                          [:button.btn.btn-wide {:class (if (= @active-tab tab)
                                                          "btn-info"
                                                          "btn-secondary")
                                                 :on-click #(reset! active-tab tab)}
                           body])]
         [:div
          [:div.bg-light.rounded.p-3
           (interpose (spaces 3) (doall
                                  (for [{:keys [name tab-body tab-badge-body]} tabs]
                                    ^{:key name}
                                    [tab-button name [:span tab-body (when tab-badge-body
                                                                       [:span (spaces 2) [:span.badge.badge-pill.badge-light (val-or-deref tab-badge-body)]])]])))]
          [:br]
          [:div.tiny-br]])
       (:pane (u/find-one {:name @active-tab} tabs))])))

(defn breadcrumb [& segments]
  (into [:span] (interpose " / " segments)))

(defn matrix [id cell]
  (let [rows (listen [:item/full-attributes id :colors])
        columns (listen [:item/full-attributes id :sizes])
        skus-index (listen [:item/skus-index id])] ; Get all the skus for the item at once for better performance.
    [:table.table.table-bordered.table-sm.mx-auto.border-0.mb-0.matrix
     [:thead
      (into [:tr]
            (cons [:th.border-left-0.border-top-0]
                  (for [col columns]
                    [:th (spaces 2) col (spaces 2)])))]
     (into [:tbody]
           (for [row rows]
             (into [:tr]
                   (cons [:td.row-header.font-weight-bold (spaces 2) row (spaces 2)]
                         (for [col columns]
                           [:td
                            (let [color row
                                  size col
                                  sku (when-let [sku-id (get skus-index [color size])]
                                        (sub [:sku sku-id]))]
                              [cell sku id color size])])))))]))

(defn matrix-responsive [id cell]
  [:div.table-responsive
   [matrix id cell]])

(let [colors {:sale {:layaway? "purple"}
              :ecom-status {:shipped "green"
                            :not-shipped "orange"
                            :canceled "red"}
              :register-adjustment {:add "green"
                                    :payout "red"}
              :order {:open "red"
                      :finished "green"}
              :transfer {:sent "red"
                         :received "green"}
              :account {:charge "green"
                        :usage "red"
                        :bot "blue"}
              :event {:bot "blue"
                      :sale "green"
                      :refund "red"
                      :order "purple"
                      :transfer "yellow"
                      :count "orange"}}]
  (defn stamp [type kw]
    [:span.badge.rounded-sm.px-3.py-2 {:class (str (get-in colors [type kw])
                                                   "-stamp")}
     (humanize kw)]))

(defn mailto
  ([args] (mailto args nil))
  ([{:keys [email text subject] :or {text email}} attrs]
   (when email
     [:a (merge {:href (str "mailto:" (if subject
                                        (str email "?subject=" subject)
                                        email))}
                attrs)
      text])))

(defn property [label cmp]
  [:div.mb-1 [:strong label ":"] (spaces 2) cmp])

(defn fields [id entity type properties]
  (into
   [:div]
   (for [[label key input] properties]
     [form-group label [(or input text-input)
                        {:val (get entity key)
                         :save #(dispatch [:change id type key %])}]])))

(defn employee-link [employee link-attrs]
  (when employee
    (let [name (m/employee:name (listen [:employee employee]))]
      (if (and (listen [:auth? :page/employee])
               (not= employee (fixture :fixture/employee-ecom)))
        [:a (merge {:href (path :page/employee employee)}
                   link-attrs)
         name]
        [:span name]))))

(defn hoverzoom* [& {:keys [tag attrs full-src] :or {tag :span}}]
  [tag {:class "hoverzoom"}
   [:img attrs]
   [:img.big-image.rounded {:src (or full-src (:src attrs))}]])

(defn hoverzoom [handle dims & {:keys [attrs]}]
  [hoverzoom*
   :attrs (assoc attrs
                 :src (thumbnail-url handle dims)
                 ;; In case there's no img src specify height/width so the broken image box takes up the correct amount of space.
                 :height dims
                 :width dims)
   :full-src (image-url handle :placeholder? true)])

(defn nested-table [& {:keys [class columns]}]
  [:table.table.mb-0.border-bottom-0 {:class class}
   [:tbody
    (into
     [:tr]
     (for [{:keys [span center? cmp]} columns]
       [:td.border-0.py-0 (merge (when center?
                                   {:class "text-center"})
                                 (when span
                                   {:style {:width (str (* span 8.34) "%")}}))
        cmp]))]])

(defn plus-button [text attrs]
  [:button.btn.btn-primary attrs
   [:i.fas.fa-plus] (spaces 2) text])

(defn chart-opts [{:keys [type labels data]}]
  ;; Do these locally instead of globally.
  (set! (-> Chart .-defaults .-global .-defaultFontFamily) "'Nunito', '-apple-system,system-ui,BlinkMacSystemFont,\"Segoe UI\",Roboto,\"Helvetica Neue\",Arial,sans-serif'")
  (set! (-> Chart .-defaults .-global .-defaultFontColor) "#858796")
  ;; When I start doing charts in earnest consider requiring the colors defined in styles.clj (make it cljc an and restrict the rules/styles defs to clj only if necessary)
  (case type
    :line
    {:type "line"
     :data
     {:labels labels
      :datasets
      [{:borderColor "rgba(78, 115, 223, 1)"
        :pointRadius 3
        :pointHoverRadius 3
        :lineTension 0.3
        :pointBorderColor "rgba(78, 115, 223, 1)"
        :label "Earnings"
        :pointHitRadius 10
        :pointBorderWidth 2
        :backgroundColor "rgba(78, 115, 223, 0.05)"
        :pointBackgroundColor "rgba(78, 115, 223, 1)"
        :pointHoverBackgroundColor "rgba(78, 115, 223, 1)"
        :pointHoverBorderColor "rgba(78, 115, 223, 1)"
        :data data}]}
     :options
     {:maintainAspectRatio false
      :layout {:padding {:left 10 :right 25 :top 25 :bottom 0}}
      :scales
      {:xAxes
       [{:time {:unit "date"}
         :gridLines {:display false
                     :drawBorder false}
         :ticks {:maxTicksLimit 7}}]
       :yAxes
       [{:ticks {:maxTicksLimit 5
                 :padding 10
                 :callback (fn [value _ _]
                             (str "$" value))}
         :gridLines
         {:color "rgb(234, 236, 244)"
          :zeroLineColor "rgb(234, 236, 244)"
          :drawBorder false
          :borderDash [2]
          :zeroLineBorderDash [2]}}]}
      :legend {:display false}
      :tooltips
      {:backgroundColor "rgb(255, 255, 255)"
       :bodyFontColor "#858796"
       :titleMarginBottom 10
       :titleFontSize 14
       :titleFontColor "#6e707e"
       :borderColor "#dddfeb"
       :borderWidth 1
       :xPadding 15
       :yPadding 15
       :intersect false
       :displayColors false
       :mode "index"
       :caretPadding 10
       :callbacks
       {:label (fn [tooltip-item chart]
                 (str (-> chart .-datasets (aget (.-datasetIndex tooltip-item)) .-label (or ""))
                      ": $"
                      (.-yLabel tooltip-item)))}}}}
    :doughnut
    {:type "doughnut"
     :data
     {:labels labels
      :datasets
      [{:data data
        :backgroundColor ["#4e73df" "#1cc88a" "#36b9cc"]
        :hoverBackgroundColor ["#2e59d9" "#17a673" "#2c9faf"]
        :hoverBorderColor "rgba(234, 236, 244, 1)"}]}
     :options
     {:maintainAspectRatio false
      :tooltips
      {:backgroundColor "rgb(255, 255, 255)"
       :bodyFontColor "#858796"
       :borderColor "#dddfeb"
       :borderWidth 1
       :xPadding 15
       :yPadding 15
       :displayColors false
       :caretPadding 10}
      :legend {:display false}
      :cutoutPercentage 80}}))

(defn chart* []
  (r/create-class
   {:component-did-mount
    (fn [this]
      (Chart.
       (dom/dom-node this)
       (clj->js (chart-opts (r/props this)))))
    ;; :component-did-update ... https://www.chartjs.org/docs/latest/developers/updates.html
    :reagent-render
    (fn []
      [:canvas])}))

(defn chart [bindings]
  [chart* (update bindings :val val-or-deref)])






;;;; Table

(defn sorter [id initial-sort label sort-fn]
  (let [[curr-fn curr-order] (listen [:table/sort id initial-sort])
        active? (= curr-fn sort-fn)]
    [:a.text-decoration-none.text-reset
     {:href no-href
      ;; TODO Can't change sort order for a property when the default sort is that property and :asc (see Commissions table Name column), fix this.
      :on-click #(dispatch [:table/set-sort id (if active?
                                                 (case curr-order
                                                   :desc [sort-fn :asc]
                                                   :asc nil
                                                   nil [sort-fn :desc])
                                                 [sort-fn :desc])])}
     label (when active?
             [:span
              (spaces 2)
              [:i.fas {:class (case curr-order
                                :desc "fa-caret-down"
                                :asc "fa-caret-up")}]])]))

;; There's a bunch of logic in the paginator component that should be in subscription handlers but it's self-contained so I'm not going to rewrite it.
(defn paginator [id source initial-sort search-keyfns]
  (let [page (listen [:table/page id])
        last-page (listen [:table/last-page id source initial-sort search-keyfns])
        first? (= page 1)
        last? (= page last-page)]
    [:div.text-center
     (when-not (= last-page 1)
       [:div
        [:div.tiny-br]
        [:div
         [:ul.pagination.justify-content-center.mb-2
          [:li.page-item (when first?
                           {:class "disabled"})
           [:a.page-link {:href no-href
                          :on-click #(dispatch [:table/set-page id 1])}
            "First"]]
          [:li.page-item (when first?
                           {:class "disabled"})
           [:a.page-link {:href no-href
                          :on-click #(when-not first?
                                       (dispatch [:table/set-page id (dec page)]))}
            [:i.fas.fa-chevron-left]]]
          (let [either-side 4
                left (max 1 (- page either-side))
                right (min last-page (+ left (* either-side 2)))
                ;; Messy and I didn't bother to think about how it works.
                left (let [slack (- right page)]
                       (if (< slack either-side)
                         (max 1 (- left (- either-side slack)))
                         left))]
            (for [i (range left (inc right))
                  :let [current? (= page i)]]
              ^{:key i}
              [:li.page-item (when current?
                               {:class "active"})
               [:a.page-link {:href no-href
                              :on-click #(dispatch [:table/set-page id i])}
                i]]))
          [:li.page-item (when last?
                           {:class "disabled"})
           [:a.page-link {:href no-href
                          :on-click #(when-not last?
                                       (dispatch [:table/set-page id (inc page)]))}
            [:i.fas.fa-chevron-right]]]
          [:li.page-item (when last?
                           {:class "disabled"})
           [:a.page-link {:href no-href
                          :on-click #(dispatch [:table/set-page id last-page])}
            "Last"]]]]])
     [:a {:href no-href
          :on-click #(dispatch [:table/set-limit id])}
      [:small (listen [:table/limit id]) " per page"]]]))

(def table-cols
  {:name {:label "Name" :key :name}
   :date {:label "Created" :key :date :center? true :render date}
   :email {:label "Email" :key :email :center? true
           :render (fn [email]
                     [mailto {:email email} {:on-click (fn [e]
                                                         (.stopPropagation e))}])}
   :shop {:label "Shop" :key #(:name (listen [:shop (:shop %)]))}
   :shop* {:label "Shop" :key #(:name (listen [:shop (:shop (listen [:register (:register %)]))]))}
   :register {:label "Register" :key #(:name (listen [:register (:register %)]))}
   ;; TODO proper sku comparator
   :sku {:label "Sku" :key #(listen [:sku/name (:sku %) :full? true])
         :render (fn [sku-name i]
                   [:a {:href no-href
                        :on-click (fn [e]
                                    (.stopPropagation e)
                                    (dispatch [:navigate-sku (:sku i)]))}
                    sku-name])}
   :category {:label "Category" :key #(listen [:category/path (:category %)])}
   :manufacturer {:label "Manufacturer" :key #(:name (listen [:manufacturer (:manufacturer %)]))}
   :customer {:label "Customer" :key #(m/customer:name (listen [:customer (:customer %)]))
              :render (fn [name i]
                        (when (:customer i)
                          [:a {:href (path :page/customer (:customer i))
                               :on-click (fn [e]
                                           (.stopPropagation e))}
                           name]))}
   :employee {:label "Employee" :key #(m/employee:name (listen [:employee (:employee %)]))
              :render (fn [_ i]
                        [employee-link (:employee i) {:on-click (fn [e]
                                                                  (.stopPropagation e))}])}
   :payment-type [:label "Payment Type" :key #(:name (listen [:payment-type (:payment-type %)]))]})

(defn table [& {search-keyfns :search initial-sort :sort
                :keys [id source filters extra class row-attrs row-on-click row-route columns item-key subrow item export bottom-child]
                :or {item-key :id}}]
  (let [id (or id source)]
    [:div.row
     [:div.col
      (when (or search-keyfns filters)
        [:div
         [:div.float-right
          [:button.btn.btn-secondary.btn-wide {:on-click #(dispatch [:table/clear id])
                                               :disabled (not (listen [:table/clearable? id]))}
           "Clear"]
          (when export
            [:span
             (spaces 2)
             [:button.btn.btn-dark.btn-wide {:on-click #(dispatch [:export export (listen [:table/all-results id source initial-sort search-keyfns])])}
              "Export"]])]
         (when search-keyfns
           [:div.row
            [:div.col-md-7.col-lg-5
             [search {:val (sub [:table/search id])
                      :save #(dispatch [:table/set-search id %])}]]])
         (when filters
           [:div
            (when search-keyfns
              [:div.tiny-br])
            [filters id]])
         (when extra
           [:div
            [:div.tiny-br]
            [:div.row
             [:div.col
              (let [extra? (listen [:table/extra-shown? id])]
                [:a.text-decoration-none
                 {:href no-href
                  :on-click #(dispatch [:table/toggle-extra-shown? id])}
                 [:i.fas {:class (if extra? "fa-caret-up" "fa-caret-down")}]
                 (spaces 2)
                 (str (if extra? "Fewer" "More") " filters")])]]
            (when (listen [:table/extra-shown? id])
              [:div
               [:div.tiny-br]
               [extra id]])])
         [:br]])
      (let [total-results (listen [:table/total-results id source initial-sort search-keyfns])]
        (if-not (zero? total-results)
          [:div
           (let [columns (->> columns
                              (map #(or (get table-cols %) %))
                              ;; TODO temporary until I convert all old vector column definitions into map ones. Update: maybe I could do this easily with an emacs macro.
                              (map (fn [col]
                                     (if (vector? col)
                                       (apply hash-map col) col)))
                              ;; For seq expansion and ignoring nils.
                              (map (fn [col]
                                     (if (or (seq? col) (nil? col))
                                       col [col])))
                              (apply concat))
                 clickable? (or row-on-click row-route)]
             [:div.table-responsive
              [:table.table {:class (str class (when clickable? " table-hover"))}
               [:thead
                (into
                 [:tr {:class "bg-white"}] ; HACK: Prevent nested table components nested in other table components from picking up .table-hover stripes on the header row. They'll still get them on body rows.
                 (for [{keyfn :key, :keys [label sort? sort-fn span center?] :or {sort? true}} columns]
                   [:th (merge (when center?
                                 {:class "text-center"})
                               (when span
                                 {:style {:width (str (* span 8.34) "%")}}))
                    (when label
                      (if (or (and sort? keyfn) sort-fn)
                        [sorter id initial-sort label
                         (or (when sort-fn
                               (if keyfn
                                 #(sort-fn (keyfn %))
                                 sort-fn))
                             keyfn)]
                        label))]))]
               [:tbody
                (doall
                 ;; HACK: Make models subs live but you have to specifically omit query-v's for subs that don't return a collection of db entities. Additionally if the db entities are retuned by a function source or the source is a collection of entities the hack won't work and they won't be live. This is all bad.
                 (apply
                  concat
                  (for [billabong (listen [:table/results id source initial-sort search-keyfns])
                        :let [i (if (or (and (not (fn? source))
                                             (keyword? (first source))
                                             (not= (first source) :credit-account/activity)
                                             (not= (first source) :gift-card/activity)
                                             (not= (first source) :commission/leaderboard)
                                             (not= (first source) :commission/lines)
                                             (not= (first source) :reports/register-counts) ; Not strictly necessary since there's an :id but it's a report so subscribing to the register count entity doesn't do much to make the table live, nor is liveness necessary.
                                             (not= (first source) :reports/register-count)))
                                  (u/strip @(posh.reagent/pull hyperdrive.db/conn '[*] [:e/id (:id billabong)]))
                                  billabong)]]
                    (list
                     (into
                      ^{:key (item-key i)}
                      [:tr (let [attrs (when row-attrs
                                         (row-attrs i))]
                             (merge attrs
                                    {:class (str (:class attrs) (when clickable? " cursor-pointer"))
                                     :on-click (fn [event]
                                                 (when row-on-click
                                                   (row-on-click i event))
                                                 (when row-route
                                                   (dispatch [:navigate row-route i])))}))]
                      (for [{keyfn :key, :keys [center? render]} columns]
                        [:td (when center?
                               {:class "text-center"})
                         (let [val (if keyfn
                                     (keyfn i) i)]
                           (if render
                             (case (.-length render)
                               0 (render)
                               1 (render val)
                               2 (render val i))
                             val))]))
                     (when subrow
                       ^{:key (str "subrow-" (item-key i))}
                       [:tr (when clickable? {:class "bg-white"}) ; "background-color: initial" would be more appropriate.
                        [:td {:colSpan (count columns)}
                         (subrow i)]])))))]]])
           (when-not (= (listen [:table/limit id]) 99999999)
             [:div
              [:div.text-center.font-weight-bold total-results " Result" (when-not (= total-results 1) "s")]
              [paginator id source initial-sort search-keyfns]])
           (when bottom-child
             [bottom-child (listen [:table/results id source initial-sort search-keyfns])])]
          [:div.text-center
           [:br]
           [:h6 [:strong "None found."]]
           (when (listen [:table/filtered? id])
             [:a {:href no-href
                  :on-click #(dispatch [:table/clear id])}
              "Clear filters."])]))]]))

(defn filter-bindings [id filter-fn & {:keys [wrap? on-save]}]
  {:val (sub [:table/filter-value id filter-fn])
   :save (fn [value]
           (if value
             (dispatch [:table/set-filter id filter-fn value :wrap? wrap?])
             (dispatch [:table/remove-filter id filter-fn]))
           (when on-save
             (on-save)))})

(defn param-bindings [id param]
  {:val (sub [:table/param-value id param])
   :save (fn [value]
           (if [value]
             (dispatch [:table/set-param id param value])
             (dispatch [:table/remove-param id param])))})

(defn shop-and-register-filters [tid]
  (list
   ^{:key :shop}
   [:div.col-md-4.col-lg-3
    [form-group "Shop"
     [select (merge (filter-bindings tid q/in-shop?
                                     :on-save #(dispatch [:table/remove-filter tid :register]))
                    {:opts (listen [:select/shops :all? true])})]]]
   ^{:key :register}
   [:div.col-md-4.col-lg-3
    [form-group "Register"
     [select (merge (filter-bindings tid :register :wrap? true)
                    {:opts (listen [:select/registers
                                    :shop (listen [:table/filter-value tid q/in-shop?])
                                    :all? true])})]]]))

(defn table-summary [columns summary]
  [:div
   [:br]
   [:br]
   [:div.row
    [:div.col-md-6.col-lg-4
     [:table.table.table-sm
      (into
       [:tbody]
       (for [{keyfn :key, :keys [label render]} columns]
         [:tr
          [:td.font-weight-bold label]
          [:td ((or render identity) (keyfn summary))]]))]]]])






;;;; Shared Components

;;; Input

(defn employee-name-input [id employee]
  [form-group "Name"
   [text-input {:val (:name employee)
                :save #(dispatch [:employee/change id :name %])}]])

(defn address-fields [id entity type]
  [panel "Address"
   [fields id entity type
    [["Address" :address]
     ["Address 2" :address2]
     ["City" :city]
     ["State" :state]
     ["Zipcode" :zipcode]
     ["Country" :country]]]])

(defn phone-fields [id entity type]
  [panel "Phones"
   [fields id entity type
    [["Mobile" :mobile-phone phone-input]
     ["Home" :home-phone phone-input]
     ["Work" :work-phone phone-input]]]])

(defn other-contact-fields [id entity type]
  [panel "Other"
   [fields id entity type
    [["Email" :email email-input]
     ["Secondary Email" :email2 email-input]
     ["Website" :website url-input]]]])









(defn add-item-buttons [on-item-click on-sku-click]
  [:div
   [plus-button "Add Item"
    {:on-click (fn []
                 (dispatch [:modal/show :item-search
                            (fn [item]
                              (dispatch (conj on-item-click (:id item)))
                              (dispatch [:modal/close]))
                            (fn [sku]
                              (dispatch (conj on-sku-click (:id sku)))
                              (dispatch [:modal/close]))]))}]
   (spaces 3)
   [plus-button "Create and Add Item"
    {:on-click #(dispatch [:modal/show :new-item on-item-click])}]])


(defn add-customer-buttons [on-click]
  [:div
   [plus-button "Add Customer"
    {:class "btn-success"
     :on-click (fn []
                 (dispatch [:modal/show :customer-search (fn [customer]
                                                           (dispatch (conj on-click (:id customer)))
                                                           (dispatch [:modal/close]))]))}]
   (spaces 3)
   [plus-button "Create and Add Customer"
    {:class "btn-success"
     :on-click (fn []
                 (dispatch [:modal/show :new-customer on-click]))}]])

(defn customer-with-remove-button [customer-id on-remove]
  [:span
   [:a {:href (path :page/customer customer-id)}
    (listen [:customer/name customer-id])]
   (spaces 2)
   [:a {:href no-href
        :on-click #(dispatch on-remove)}
    [:i.fas.fa-times]]])







;;; Generic

(defn print-preview []
  (let [setup
        (fn [this]
          (let [node (dom/dom-node this)
                document (.-contentDocument node)]
            (.write document ((prop this :render) (prop this :doc)))
            (.close document) ; Stop the tab spinny.
            (reset! (prop this :print-fn)
                    (fn []
                      (when-let [cw (.-contentWindow node)] ; A not-ideal way to ensure there's no nullpointerexception since this is called with a timeout and the component could be gone.
                        (.focus cw)
                        (.print cw))))))]
    (r/create-class
     {:component-did-mount
      (fn [this]
        (setup this)
        (when (prop this :print-prompt?)
          ;; Give JsBarcode time to do its thing. Unnecessary when the sale receipt is for a bulk sale since there's no barcode generation but do the delay anyway because it's easier and consistent.
          (js/setTimeout
           @(prop this :print-fn)
           1500)))
      :component-did-update setup
      :reagent-render
      (fn []
        [:iframe.iframe-preview {:src "about:blank"}])}))) ; Ideally the height would be dynamically set the the height of the content.

(defn password-input [label state]
  [form-group label
   [input-addon
    :append [:i.fas.fa-lock]
    (atom-bindings state)
    {:type "password"}]])

(defn change-sensitive-form []
  (let [current (r/atom nil)
        new (r/atom nil)
        retype (r/atom nil)]
    (fn [title name loading? save]
      [panel title
       (let [disabled (not (and @new
                                @retype
                                (= @new @retype)))]
         [:form {:on-submit (fn [e]
                              (when (not disabled)
                                (save @current @new)
                                (doseq [field [current new retype]]
                                  (reset! field nil))
                                (.preventDefault e)))}
          [password-input (str "Current " name) current]
          [password-input (str "New " name) new]
          [password-input (str "Re-type "name) retype]
          [:div.tiny-br]
          [:div.text-center
           [loading-button
            {:loading? loading?}
            {:class "btn-dark btn-wide"
             :disabled disabled
             :type "submit"}
            "Save"]]])])))

(defn render-denom [denom]
  (if (< denom 1)
    (str (* 100 denom) "¢")
    (str "$" denom)))

(defn denominations [denom-amounts on-click]
  [:div.text-center
   (for [denom denom-amounts]
     ^{:key denom}
     [:button.btn.btn-primary.btn-sm.mt-2.mr-2 {:on-click #(on-click denom)}
      (render-denom denom)])])

(defn denomination-inputs [denom-amounts on-change]
  (let [counts (r/atom {})
        on-denom-change (fn []
                          (on-change (->> @counts
                                          (map (fn [[k v]]
                                                 (if (= k :extra)
                                                   v (* k v))))
                                          (apply +))))]
    (fn []
      [:div.text-center
       (doall
        (for [denom (conj denom-amounts :extra)
              :let [is-extra? (= denom :extra)]]
          ^{:key denom}
          [:div
           [:div.input-group.input-group-sm
            [:div.input-group-prepend
             (if is-extra?
               [:span.input-group-text "Extra"]
               [:button.btn.btn-primary {:type "button"
                                         :on-click (fn []
                                                     (swap! counts update denom (fnil inc 0))
                                                     (on-denom-change))}
                (render-denom denom)])]
            [text-input (merge {:val (get @counts denom)
                                :save (fn [raw]
                                        (if (nil? raw)
                                          (swap! counts dissoc denom)
                                          (let [count ((if is-extra? u/parse-number u/parse-int) raw)]
                                            (when ((if is-extra? pos? pos-int?) count)
                                              (swap! counts assoc denom count))))
                                        (on-denom-change))}
                               (when is-extra?
                                 {:displayize #(currency % :no-sign? true :coerce-nil? false)}))
             {:class "form-control-sm text-right"}]]
           [:div.tiny-br]]))])))

(defn tier-img [tier-rank attrs]
  [:img (merge {:src (u/tier-image tier-rank)
                :height "33px"
                :width "30px"}
               attrs)])

(def tier-info-html
  [:div
   [:strong "Customer Tiers"]
   [:br][:br]
   [:div.text-left
    (for [[rank tier] (reverse u/tiers)]
      [:div
       [:div (tier-img rank nil) (spaces 5) (humanize tier)]
       [:div.tiny-br]])]])

(defn tier-info [cmp]
  [tooltip {:text (html tier-info-html)
            :html? true
            :placement :right
            :cmp cmp}])

(defn tier-badge [customer-id attrs]
  [tier-info [tier-img (listen [:customer/tier customer-id]) attrs]])



;;; Tables

(def sale-lines-columns [{:label "Qty" :key :qty}
                         {:label "Total" :key :total :render currency}
                         {:label "Cost" :key :cost :render currency}
                         {:label "Profit" :key :profit :render #(plus-minus % currency)}
                         {:label "Margin" :key :margin :render percent-change}])

(defn report-summary [summary-sub-id by table-id columns]
  [table-summary columns (listen [summary-sub-id by (listen [:table/params table-id])])])

(defn breakout-shops-columns [show?]
  (when show?
    (for [shop (listen [:shops])]
      {:label (:name shop) :key #(listen [:item/stock (:id %) (:id shop)]) :center? true})))

(defn item-skus-table [item-id breakout-shops? on-sku-click]
  [table
   :source [:item/skus item-id]
   :sort [#(fetch [:sku/name (:id %)]) :asc]
   :class "minimally-padded-table"
   :columns [[:center? true
              :render (fn [i]
                        [hoverzoom (listen [:sku/primary-image (:id i) :inherit? true]) 40])]
             [:label "Color / Size" :key #(listen [:sku/name (:id %)])]
             [:label "Style Number" :key :style-number]
             [:label "UPC" :key :upc]
             ;; [:label "Default Barcode" :key :code]
             (breakout-shops-columns breakout-shops?)
             [:label "Stock" :key #(listen [:sku/stock (:id %)]) :center? true]
             [:label "Price" :key #(listen [:sku/price (:id %) :inherit? true]) :center? true :render currency]]
   :row-on-click on-sku-click])

;; TODO HACK What I really want is for tables to have other supplemental settings that persist like filters/sorting/params/etc but don't do anything except be readable/writable. This hack works it's slower than it could be because you have to iterate through all the table results for no reason and it makes the page reset which isn't necessary. Get rid of this and then I won't need table-id here anymore either
(let [show-skus-dummy-filter (constantly true)
      breakout-shops-dummy-filter (constantly true)]
  (defn items-table [on-item-click on-sku-click bottom-child] ; TODO Having `bottom-child` be an arg is a hack so I can get the Export Items modal done
    (let [table-id :items
          breakout-shops? (listen [:table/filter-value table-id breakout-shops-dummy-filter])]
      [table
       :id table-id
       :source [:items]
       :search [:name]
       :filters (fn [tid]
                  [:div.row
                   [:div.col
                    [checkbox (assoc (filter-bindings tid show-skus-dummy-filter)
                                     :inline? true)
                     "Show skus"]]])
       :extra (fn [tid]
                [:div
                 [:div.row
                  [:div.col-md-4.col-lg-3
                   [form-group "Category"
                    [select (merge (filter-bindings tid :category :wrap? true)
                                   {:opts (listen [:select/categories :all? true])})]]]
                  [:div.col-md-4.col-lg-3
                   [form-group "Manufacturer"
                    [select (merge (filter-bindings tid :manufacturer :wrap? true)
                                   {:opts (listen [:select/manufacturers :all? true])})]]]
                  [:div.col-md-4.col-lg-3
                   [form-group "Size"
                    [select (merge (filter-bindings tid q/item:has-size?)
                                   {:opts u/size-opts})]]]]
                 [:div.row
                  [:div.col-md-4.col-lg-3
                   [form-group "Created"
                    [datepicker-range (filter-bindings tid q/date-within?)]]]
                  [:div.col-md-4.col-lg-3
                   [form-group "Tags"
                    [tags-input (filter-bindings tid q/item:has-tags?)]]]]
                 [:div.row
                  [:div.col
                   [checkbox (assoc (filter-bindings tid q/item:in-stock?)
                                    :inline? true)
                    "In stock?"]
                   [checkbox (assoc (filter-bindings tid breakout-shops-dummy-filter)
                                    :inline? true)
                    "Breakout shops"]]]])
       :class "minimally-padded-table"
       :columns [[:center? true
                  :render (fn [i]
                            [hoverzoom (listen [:item/primary-image (:id i)]) 40])]
                 :name
                 :manufacturer
                 (breakout-shops-columns breakout-shops?)
                 [:label "Stock" :key #(listen [:item/stock (:id %)]) :center? true]
                 ;; TODO If I end up rewriting tables know that the only reason I use an explicity :sort-fn here is for performance
                 [:label "Price" :center? true :sort-fn :price :render (fn [_ i]
                                                                         (currency (listen [:item/price (:id i)])))]
                 :date]
       :subrow (when (listen [:table/filter-value table-id show-skus-dummy-filter])
                 (fn [i]
                   [:div.pl-5
                    [item-skus-table (:id i) breakout-shops? (or on-sku-click #(dispatch [:navigate-sku (:id %)]))]]))
       :row-on-click (or on-item-click #(dispatch [:navigate :page/item %]))
       :export :export/items
       :bottom-child bottom-child])))

(defn customers-table [on-selection]
  [table
   :source [:customers]
   :search [m/customer:name
            :mobile-phone
            :home-phone
            :work-phone
            ;; (fn [customer]
            ;;   (when-let [phone (:phone customer)]
            ;;     (str/replace phone #"-" "")))
            :email
            :email2]
   :extra (fn [tid]
            [:div.row
             [:div.col-md-4.col-lg-3
              [form-group "Created"
               [datepicker-range (filter-bindings tid q/date-within?)]]]
             [:div.col-md-4.col-lg-3
              [form-group "Customer Type" [select (merge (filter-bindings tid :type :wrap? true)
                                                         {:opts (listen [:select/customer-types :all? true])})]]]])
   :columns [{:label "Tier" :render (fn [customer]
                                      [tier-badge (:id customer)])} ; No sorting, calculating all customer tiers takes too long.
             {:label "Name" :sort-fn m/customer:name :render #(listen [:customer/name (:id %)])}
             {:label "Type" :key #(:name (listen [:customer-type (:type %)]))}
             {:label "Mobile" :key :mobile-phone}
             {:label "Home" :key :home-phone}
             {:label "Work" :key :work-phone}
             :email
             :date]
   :row-on-click on-selection])

;; TODO move this somewhere more appropriate, it's used elsewhere
(defn sale-line-refunded? [sale-line]
  (some #{(:id sale-line)} (listen [:refunded-sale-lines])))

(defn sale-lines-table []
  [:div
   [table
    :id :table-id/sale-lines
    :source (fn [params]
              [:reports/sale-lines params])
    :sort [#(:date (fetch [:sale (:sale %)])) :desc]
    :search [#(:code (fetch [:sale (:sale %)]))
             #(fetch [:sku/name (:sku %) :full? true])]
    :filters (fn [tid]
               [:div
                [:div.row
                 [:div.col-md-4.col-lg-3
                  [form-group "Date"
                   [datepicker-range (param-bindings tid :date-range)]]]
                 [:div.col-md-4.col-lg-3
                  [form-group "Category"
                   [select (merge (param-bindings tid :category)
                                  {:opts (listen [:select/categories :all? true])})]]]
                 [:div.col-md-4.col-lg-3
                  [form-group "Manufacturer"
                   [select (merge (param-bindings tid :manufacturer)
                                  {:opts (listen [:select/manufacturers :all? true])})]]]]
                [:div.row
                 [:div.col-md-4.col-lg-3
                  [form-group "Shop"
                   [select (merge (param-bindings tid :shop)
                                  {:opts (listen [:select/shops :all? true])})]]]
                 [:div.col-md-4.col-lg-3
                  [form-group "Employee"
                   [select (merge (param-bindings tid :employee)
                                  {:opts (listen [:select/employees :all? true])})]]]]])
    :columns [{:label "Sale Code" :key #(:code (listen [:sale (:sale %)]))}
              :sku
              {:label "Shop" :key #(:name (listen [:shop (:shop (listen [:register (:register (listen [:sale (:sale %)]))]))]))}
              :employee
              {:label "Price" :key :price :render currency}
              {:label "Commission" :key q/commission :render currency}
              {:label "Date" :key #(:date (listen [:sale (:sale %)])) :render date}]
    :row-on-click #(dispatch [:navigate :page/sale (:sale %)])]
   [report-summary :reports/grouped-sales-summary nil :table-id/sale-lines sale-lines-columns]])

(defn sales-table [& {:keys [source include] :or {source [:sales]
                                                  include #{:shop-and-register :employee}}}]
  (let [shop-and-register? (contains? include :shop-and-register)
        employee? (contains? include :employee)]
    [table
     :source source
     :search [:code
              #(:customer/name (fetch [:customer (:customer %)]))
              #(fetch [:sale/total (:id %)])]
     :extra (fn [tid]
              [:div
               [:div.row
                [:div.col-md-4.col-lg-3
                 [form-group "Date"
                  [datepicker-range (filter-bindings tid q/date-within?)]]]
                (when shop-and-register?
                  (shop-and-register-filters tid))]
               (when employee?
                 [:div.row
                  [:div.col-md-4.col-lg-3
                   [form-group "Employee"
                    [select (merge (filter-bindings tid :employee :wrap? true)
                                   {:opts (listen [:select/employees :all? true])})]]]
                  [:div.col-md-8.col-lg-6
                   [form-group "Type"
                    [:div
                     [checkbox (assoc (filter-bindings tid :layaway? :wrap? true)
                                      :inline? true)
                      "Layaway?"]]]]])])
     :columns [[:label "Source" :key :sale-type :render humanize]
               [:label "Type" :center? true
                :render (fn [i]
                          [:div
                           (when (:layaway? i)
                             [stamp :sale :layaway?])])]
               [:label "Code" :key :code]
               (when shop-and-register? :shop*)
               (when shop-and-register? :register)
               [:label "Total" :key #(listen [:sale/total (:id %)]) :render currency]
               :customer
               (when employee? :employee)
               (merge (:date table-cols) {:label "Date"})]
     :row-route :page/sale
     :bottom-child (fn [results]
                     [table-summary
                      [{:label "Total" :key :total :render currency}]
                      {:total (apply + (map #(listen [:sale/total (:id %)]) results))}])]))

(defn register-adjustments-table [& {:keys [source filters extra] :or {source [:register-adjustments]}}]
  [table
   :source source
   :filters filters
   :extra extra
   :columns [[:label "Type" :key :type :center? true :render (fn [type]
                                                               [stamp :register-adjustment type])]
             [:label "Amount" :key m/register-adjustment:change :render #(plus-minus % currency)]
             :payment-type
             :shop*
             :register
             :employee
             [:label "Notes" :key :notes]
             (merge (:date table-cols) {:label "Date"})]])

(def item-name-and-picture-cols
  [{:center? true
    :render (fn [item]
              [:a {:href (path :page/item item)}
               [hoverzoom (listen [:item/primary-image (:id item)]) 80]])}
   (merge (:name table-cols) {:render (fn [name i]
                                        [:a {:href (path :page/item i)}
                                         name])})])

(defn stock-management-items-table [& {:keys [entity cols]}]
  [table
   :id (str/join "-" ["stock-managemnt" (:e/type @entity) (:id @entity)])
   :source [:stock-management-items entity]
   :sort [::dummy :asc] ; Dummy sort key that always results in nil so items aren't sorted.
   :search [:name]
   :columns (concat item-name-and-picture-cols cols)])

(defn bulk-sale-items-table [& {:keys [sale editable?]}]
  (let [sale-id (if (u/new-id? (:id @sale)) :new (:id @sale)) ; HACK
        sku-cell
        (fn [sku]
          (when sku
            (let [qty (sub [:sale.bulk/sku-qty sale-id (:id @sku)])]
              (if editable?
                [text-input {:val qty
                             :save #(dispatch [:sale.bulk/change-sku-qty sale-id (:id @sku) %])}
                 {:class "form-control-sm qty-input"}]
                [:span @qty]))))]
    (fn [& {:keys [sale columns]}]
      [stock-management-items-table
       :entity sale
       :cols [(cons {:label "Skus" :center? true
                     :render (fn [item]
                               [matrix (:id item) sku-cell])}
                    columns)]])))

(defn account-activity-table [source]
  [table
   :source source
   :search [:code]
   :columns [{:label "Action" :key :action :center? true :render (fn [action]
                                                                   [stamp :account action])}
             {:label "Sale" :key :code}
             {:label "Amount" :key :amount :render #(plus-minus % currency)}
             (merge (:date table-cols) {:label "Date"})]
   :row-on-click (fn [i]
                   (when-not (= (:action i) :bot)
                     (dispatch [:navigate :page/sale (:sale-id i)])))
   :row-attrs (fn [i]
                (when (= (:action i) :bot)
                  {:style {:cursor "initial"}}))])







;;;; Shared item/sku components

(defn images []
  (let [thumbs-shown? (r/atom false)]
    (fn [model inv]
      [panel [:span "Images" (when (= model :sku)
                               [:span (spaces 2) [help {:text "When a sku doesn't have any images, the images of the parent item will be used instead."}]])]
       (let [primary-image (listen [(keyword model :primary-image) (:id @inv)])
             [image inherited?] (if (and (= model :sku) (not primary-image))
                                  [(listen [:sku/primary-image (:id @inv) :inherit? true]) true]
                                  [primary-image false])]
         [:img.rounded.img-fluid.mx-auto.d-block {:src (image-url image :placeholder? true)
                                                  :style (when inherited?
                                                           {:opacity 0.3})}])
       [:br]
       [:div.text-center
        [loading-button
         {:loading? (sub [:filepicker-working?])}
         {:class "btn-primary btn-wide"
          :on-click (fn []
                      (dispatch [(keyword model :upload-images) (:id @inv)])
                      (reset! thumbs-shown? true))} ; So the user can see the new images when they're done. Ideally this would happen after the filepicker closed and only if new images were uploaded.
         "Upload Images"]]
       (when (pos? (count (:images @inv)))
         [:div
          [:br]
          [:a.text-decoration-none
           {:href no-href
            :on-click #(swap! thumbs-shown? not)}
           [:i.fas {:class (if @thumbs-shown? "fa-caret-down" "fa-caret-up")}]
           (spaces 2)
           (str (if @thumbs-shown? "Hide" "Show") " all")]
          (when @thumbs-shown?
            (into
             [:div.mt-2]
             (for [handle (:images @inv)]
               [:div.d-inline-block.mb-3.mr-4
                [:a {:href no-href
                     :on-click #(dispatch [(keyword model :set-primary-image) (:id @inv) handle])}
                 [hoverzoom handle 64 :attrs {:class "img-thumbnail"}]]
                [:a.text-decoration-none {:href no-href
                                          :on-click #(dispatch [(keyword model :remove-image) (:id @inv) handle])}
                 (spaces 2) [:i.fas.fa-times]]])))])])))

(defn pricing [model inv]
  [panel [:span "Pricing" (when (= model :sku)
                            [:span (spaces 2) [help {:text "When a sku doesn't have a price set, the price of the parent item will be used instead."}]])] 
   [:div
    [:table.table.table-sm
     [:thead
      [:tr
       [:th "Name"]
       [:th.w-50 "Price"]]]
     [:tbody
      (concat
       (doall
        (for [[attribute name] [[:price "Default"]
                                [:msrp-price "MSRP"]
                                [:online-price "Online"]
                                [:wholesale-price "Wholesale"]
                                [:default-cost "Default Cost"]]]
          ^{:key attribute}
          [:tr
           [:td.font-weight-bold name]
           [:td [money-input {:val (get @inv attribute)
                              :save #(dispatch [(keyword model :change) (:id @inv) attribute %])}
                 (if (= attribute :default-cost)
                   {:disabled (not (listen [:auth? :set-default-cost]))}
                   {:placeholder (currency (listen [(keyword model :price) (:id @inv) :price-type attribute :inherit? true])
                                           :no-sign? true)})]]]))
       (when (= model :sku)
         (list
          ^{:key :sku/average-cost}
          [:tr
           [:td.font-weight-bold.py-3 "Average Cost"]
           [:td (currency (listen [:sku/average-cost (:id @inv)]))]])))]]]])






;;;; Modals

(defn time-clocks []
  (let [pin (r/atom nil)]
    (fn []
      [modal
       :title "Time Clock"
       :body
       [:div
        [:div.row
         [:form.col {:on-submit (fn [e]
                                  (dispatch [:clock-in-out :pin @pin])
                                  (reset! pin nil)
                                  (.preventDefault e))}
          [:div.text-center
           [:small "Enter your access PIN to clock in or out."]
           [:br]
           [:div.tiny-br]
           [:div.row
            [:div.col-6.offset-3
             [input-addon
              :append [:i.fas.fa-key]
              (atom-bindings pin)
              {:type "password"}]]]
           [:br]
           [loading-button
            {:loading? (sub [:clock/working? :pin])}
            {:class "btn-dark btn-wide"
             :type "submit"}
            "Clock In/Out"]
           ;; [:div.tiny-br]
           ;; [refresher #(elapsed (listen [:clock/in-time (:id (listen [:user]))]))]
           ]]]
        [:br]
        [:div.tiny-br]
        [:div.row.text-center
         (doall
          (for [[sub-id text label-class] [[:clocks/employees-in "Clocked In" "badge-clocked-in"]
                                           [:clocks/employees-out "Clocked Out" "badge-clocked-out"]]]
            ^{:key sub-id}
            [:div.col-6
             [:span.badge {:class label-class} text]
             [:div.tiny-br]
             (doall
              (for [employee-id (listen [sub-id])
                    :let [employee (listen [:employee employee-id])]]
                ^{:key employee-id}
                [:div (m/employee:name employee)]))]))]]])))

(defn change-password-form [id]
  [change-sensitive-form
   "Change Password"
   "password"
   false
   (fn [current new]
     (dispatch [:auth/change-password current new]))])

(defn change-pin-form []
  [change-sensitive-form
   "Change PIN"
   "PIN"
   (sub [:change-pin/working?])
   (fn [current new]
     (dispatch [:change-pin current new]))])

(defn user-settings []
  [modal
   :size :lg
   :title "My Settings"
   :body [:div.row
          [:div.col-lg-5.offset-lg-1
           [change-password-form (:id (listen [:user]))]]
          [:div.col-lg-5
           [change-pin-form (:id (listen [:user]))]]]
   :footer [:button.btn.btn-dark.btn-wide
            {:type "button"
             :on-click #(dispatch [:modal/close])}
            "Done"]])

(let [upc-cell
      (fn [sku]
        (when sku
          [text-input {:val (:upc @sku)
                       :save #(dispatch [:sku/change (:id @sku) :upc %])}
           {:class "form-control-sm code-input"}]))]
  (defn upcs [id]
    [modal
     :size :xl
     :title "Enter UPCs"
     :body [:div.row
            [:div.col-lg-10.offset-lg-1
             [matrix-responsive id upc-cell]]]
     :footer [:button.btn.btn-dark.btn-wide
              {:type "button"
               :on-click #(dispatch [:modal/close])}
              "Done"]]))

(defn print-labels [item-or-sku-amounts]
  (let [items (if ((some-fn nil? map?) item-or-sku-amounts)
                (q/items-for-skus (keys item-or-sku-amounts))
                [item-or-sku-amounts])
        sku-amounts (r/atom (when (map? item-or-sku-amounts)
                              item-or-sku-amounts))
        qty-cell
        (fn [sku]
          (when sku
            [text-input {:val (get @sku-amounts (:id @sku))
                         :save (fn [raw]
                                 (if (nil? raw)
                                   (swap! sku-amounts dissoc (:id @sku))
                                   (let [amount (u/parse-int raw)]
                                     (when (pos-int? amount)
                                       (swap! sku-amounts assoc (:id @sku) amount)))))}
             {:class "form-control-sm qty-input"}]))]
    (fn []
      [modal
       :size :xl
       :title "Print Labels"
       :body [table
              :id :labels
              :source [:items items]
              :sort [:name :asc]
              :columns
              [[:center? true
                :render (fn [i]
                          [:a {:href no-href
                               :on-click (fn []
                                           (dispatch [:navigate :page/item i])
                                           (dispatch [:modal/close]))}
                           [hoverzoom (listen [:item/primary-image (:id i)]) 80]])]
               (merge {:render (fn [name i]
                                 [:a {:href no-href
                                      :on-click (fn []
                                                  (dispatch [:navigate :page/item i])
                                                  (dispatch [:modal/close]))}
                                  name])}
                      (:name table-cols))
               [:label "# Labels per sku" :center? true
                :render (fn [i]
                          [matrix (:id i) qty-cell])]]]
       :footer [:button.btn.btn-dark.btn-wide
                {:type "button"
                 :on-click (fn []
                             (dispatch-sync [:open-tab (print/labels @sku-amounts)])
                             (dispatch [:modal/close]))}
                "Print"]])))

;; TODO why doesn't this use `new-or-edit`?
(defn new-item [on-create]
  (let [create-all? (r/atom false)]
    (fn []
      (let [item (sub [:item/new])]
        [modal
         :size :lg
         :title "Create a New Item"
         :body [:div.new-item
                [:div.row
                 [:div.col-lg-6.offset-lg-1
                  [form-group "Name" [text-input {:val (:name @item)
                                                  :save #(dispatch [:item/change :new :name %])}]]]
                 [:div.col-lg-2
                  [form-group "Price" [money-input {:val (:price @item)
                                                    :save #(dispatch [:item/change :new :price %])}]]]
                 [:div.col-lg-2
                  [form-group "Default Cost" [money-input {:val (:default-cost @item)
                                                           :save #(dispatch [:item/change :new :default-cost %])}]]]]
                [:div.row
                 [:div.col-lg-5.offset-lg-1
                  [form-group "Manufacturer" [select {:val (:manufacturer @item)
                                                      :save #(dispatch [:item/change :new :manufacturer %])
                                                      :opts (listen [:select/manufacturers :none? true])}]]
                  [form-group "Colors" [tags-input {:val (:colors @item)
                                                    :save #(dispatch [:item/change :new :colors %])}]]]
                 [:div.col-lg-5
                  [form-group "Category" [select {:val (:category @item)
                                                  :save #(dispatch [:item/change :new :category %])
                                                  :opts (listen [:select/categories :none? true])}]]
                  [form-group "Sizes" [tags-input {:val (:sizes @item)
                                                   :save #(dispatch [:item/change :new :sizes %])}]]]]
                [:br]
                [:div.row.justify-content-center
                 [:div.col-auto
                  [checkbox (atom-bindings create-all?)
                   "Create all skus?"]]]]
         :footer [one-time-button
                  {:class "btn-dark btn-wide"
                   :disabled (listen [:new/invalid? :type/item])
                   :on-click #(dispatch [:item/create @create-all? on-create])}
                  "Save"]]))))

(defn apply-to-all-button [id color-or-size text attribute]
  [:button.btn.btn-danger
   {:disabled (not @attribute)
    :on-click #(dispatch [:sku/apply-to-all id color-or-size @attribute])}
   text])

(defn apply-to-all []
  (let [attribute (r/atom nil)]
    (fn [id]
      [:div.row
       [:div.col-md-6.col-lg-4
        [select (merge (atom-bindings attribute)
                       {:opts [["Images" :images]
                               ["Style Number" :style-number]
                               ["Custom SKU Number" :custom-sku-number]]})]]
       [:div.col-md-auto.ml-auto
        [apply-to-all-button id :color "...apply to all in color" attribute]
        (spaces 1)
        [apply-to-all-button id :size "...apply to all in size" attribute]]])))

(defn edit-sku [id]
  (let [sku (sub [:sku id])]
    [modal
     :size :xl
     :title [:span
             (listen [:sku/name (:id @sku)])
             (spaces 2)
             [:span.badge.badge-pill.badge-stock.badge-little (listen [:sku/stock (:id @sku)])]]
     :body [:div
            [:div.row
             [:div.col-md
              [panel "IDs"
               (into [:div]
                     (for [[text k] [["UPC" :upc]
                                     ["EAN" :ean]
                                     ["Style Number" :style-number]
                                     ["Custom SKU Number" :custom-sku-number]]]
                       [form-group text
                        [text-input {:val (get @sku k)
                                     :save #(dispatch [:sku/change id k %])}]]))
               [:strong "Default Barcode"] (spaces 2) [help {:text "This is a generated barcode that will be used when printing labels if there's no UPC or EAN."}]
               [:br]
               (:code @sku)]]
             [:div.col-md
              [images :sku sku]]
             [:div.col-md
              [pricing :sku sku]]]
            [:div.row
             [:div.col
              [panel "Actions"
               [apply-to-all id]]]]]]))

(defn new-or-edit [& {:keys [id type body on-done create-param title-text size button-text additional-invalid?]}]
  (let [new? (= :new id)
        entity (if new?
                 (listen [(keyword type :new)])
                 (listen [type id]))]
    [modal
     :size size
     :title (or title-text (str (if new?
                                  "Create a New" "Edit")
                                " " (humanize type)))
     :body [:div.row
            [:div.col-md-10.offset-md-1
             [body entity]]]
     :footer (let [text (or button-text (if new?
                                          "Save" "Done"))
                   on-click (fn []
                              (if on-done
                                (on-done)
                                (when new?
                                  (dispatch [(keyword type :create) create-param])))
                              (dispatch [:modal/close]))]
               (if new?
                 [one-time-button
                  {:class "btn-dark btn-wide"
                   :disabled (or (listen [:new/invalid? (keyword :type type)])
                                 (when additional-invalid?
                                   (additional-invalid?)))
                   :on-click on-click}
                  text]
                 [:button.btn.btn-dark.btn-wide
                  {:type "button"
                   :on-click on-click}
                  text]))]))

(defn edit-category [id]
  [new-or-edit
   :id id
   :type :category
   :body (fn [category]
           [:div
            [form-group "Name" [text-input {:val (:name category)
                                            :save #(dispatch [:category/change id :name %])}]]
            [:br]
            [form-group "Parent Category"
             [select {:val (:parent category)
                      :save (fn [parent]
                              (dispatch [:category/change id :parent parent]))
                      :opts (listen [:select/categories :none? true])}]]])])

(defn edit-manufacturer [id]
  [new-or-edit
   :id id
   :type :manufacturer
   :size :lg
   :body (fn [manufacturer]
           [:div
            [form-group "Name" [text-input {:val (:name manufacturer)
                                            :save #(dispatch [:manufacturer/change id :name %])}]]
            [:br]
            [:div.row
             [:div.col-md
              [address-fields id manufacturer :type/manufacturer]]
             [:div.col-md
              [phone-fields id manufacturer :type/manufacturer]
              [other-contact-fields id manufacturer :type/manufacturer]]]])])

(defn new-customer [on-create]
  (let [id :new]
    [new-or-edit
     :id id
     :type :customer
     :body (fn [customer]
             [:div
              [:div.row
               [:div.col-md
                [form-group "First Name" [text-input {:val (:first-name customer)
                                                      :save #(dispatch [:customer/change id :first-name %])}]]]
               [:div.col-md
                [form-group "Last Name" [text-input {:val (:last-name customer)
                                                     :save #(dispatch [:customer/change id :last-name %])}]]]]
              [:div.row
               [:div.col
                [form-group "Email" [email-input {:val (:email customer)
                                                  :save #(dispatch [:customer/change id :email %])}]]
                [form-group "Mobile Phone" [phone-input {:val (:mobile-phone customer)
                                                         :save #(dispatch [:customer/change id :mobile-phone %])}]]]]])
     :create-param on-create]))

(defn edit-customer-type [id]
  [new-or-edit
   :id id
   :type :customer-type
   :body (fn [customer-type]
           [:div
            [:div.row
             [:div.col
              [form-group "Name"
               [text-input {:val (:name customer-type)
                            :save #(dispatch [:customer-type/change id :name %])}]]]]
            [:div.row
             [:div.col-md-6.col-lg-4
              [form-group "Discount"
               [percent-input {:val (:discount customer-type)
                               :save #(dispatch [:customer-type/change id :discount %])}]]]]])])

(defn new-employee []
  (let [id :new]
    [new-or-edit
     :id id
     :type :employee
     :body (fn [employee]
             [:div
              [form-group "Email"
               [email-input
                {:val (:email employee)
                 :save #(dispatch [:employee/change id :email %])}]]
              [employee-name-input id employee]])
     :on-done #(dispatch [:sign-up (:email (fetch [:employee/new]))])])) ; Hack: Fetching here is a hack and probably easy to clean up

(defn edit-timesheet [id employee]
  [new-or-edit
   :id id
   :type :timesheet
   :body (fn [timesheet]
           [:div.text-center
            [form-group [:span "In" (spaces 8)]
             [datepicker {:val (:in timesheet)
                          :save #(dispatch [:timesheet/change id :in %])
                          :time? true
                          :static? true}]]
            [form-group [:span "Out" (spaces 4)]
             [datepicker {:val (:out timesheet)
                          :save #(dispatch [:timesheet/change id :out %])
                          :time? true
                          :static? true}]]
            [:div
             [:div.tiny-br]
             (when (and (:in timesheet) (:out timesheet) (<= (:in timesheet) (:out timesheet)))
               [:strong (elapsed [(:in timesheet) (:out timesheet)]) " hours"])]])
   :create-param employee])

(defn new-shop []
  (let [id :new]
    [new-or-edit
     :id id
     :type :shop
     :body (fn [shop]
             [form-group "Name" [text-input {:val (:name shop)
                                             :save #(dispatch [:shop/change id :name %])}]])]))

(defn edit-register [id shop]
  [new-or-edit
   :id id
   :type :register
   :body (fn [register]
           [form-group "Name" [text-input {:val (:name register)
                                           :save #(dispatch [:register/change id :name %])}]])
   :create-param shop])

(defn edit-payment-type [id]
  [new-or-edit
   :id id
   :type :payment-type
   :body (fn [payment-type]
           [form-group "Name" [text-input {:val (:name payment-type)
                                           :save #(dispatch [:payment-type/change id :name %])}]])])

(defn item-search [on-item-click on-sku-click]
  [modal
   :size :xl
   :title "Item Search"
   :body [items-table on-item-click on-sku-click]])

(defn customer-search [on-selection]
  [modal
   :size :xl
   :title "Customer Search"
   :body [customers-table on-selection]])

(defn sku-picker [item-id on-sku-click on-back]
  [modal
   :size :xl
   :body [item-skus-table item-id false on-sku-click]
   :footer (when on-back
             [:button.btn.btn-dark.btn-wide {:on-click on-back} "Back"])])











(defn payment-input [payments sale-id payment-type-id name fill-total]
  [:div
   [:label name]
   [:div.row
    [:div.col-9.mr-auto
     [money-input {:val (get payments payment-type-id)
                   :save #(dispatch [:payment/set sale-id payment-type-id %])}]]
    [:div.col
     [:button.btn.btn-primary {:on-click #(dispatch [:payment/max sale-id payment-type-id fill-total])}
      "Max"]]]
   (when (= payment-type-id (fixture :fixture/payment-type-cash))
     [:div.row
      [:div.col-md-10.offset-md-1
       [denominations
        [100 50 20 10 5 1 0.25]
        #(dispatch [:payment/add sale-id payment-type-id %])]]])])

(defn payment [sale-id]
  (let [payments (listen [:payment/payments sale-id])
        sale (listen [:sale sale-id])
        sale-is-refund-with-refundable-money (listen [:sale-is-refund-with-refundable-money sale-id])
        fill-total ((if sale-is-refund-with-refundable-money - +)
                    (listen [:sale/total sale-id]))]
    [modal
     :title (if sale-is-refund-with-refundable-money
              [:span.text-orange "Refund"] "Payment")
     :body [:div.row
            [:div.col-md-10.offset-md-1
             [:div.text-center
              [plus-button "Gift Card"
               {:on-click (fn []
                            (dispatch [:modal/show :use-gift-card-1 sale-id fill-total]))}]]
             [:br]
             (when-let [gift-card-ids (seq (filter (fn [payment-type-id]
                                                     (= :type/gift-card (:e/type (listen [:gift-card payment-type-id]))))
                                                   (keys payments)))]
               [:div
                (doall
                 (for [gift-card-id gift-card-ids]
                   (let [gift-card (listen [:gift-card gift-card-id])]
                     ^{:key gift-card-id}
                     [:div.clearfix
                      [:div.float-left "Gift Card: " (:code gift-card)]
                      [:div.float-right
                       (currency (get payments gift-card-id))
                       (spaces 3)
                       [:a {:href no-href
                            :on-click #(dispatch [:payment/set sale-id gift-card-id nil])}
                        [:i.fas.fa-times]]]])))
                [:br]])
             (when-let [credit-account-id (listen [:customer/credit-account (:customer sale)])]
               [:div
                [payment-input payments sale-id credit-account-id "Account" fill-total]
                [:small "Available:" (spaces 2) (currency (listen [:credit-account/available credit-account-id]))]
                [:br]
                [:div.tiny-br]])
             (doall
              (for [payment-type-id (listen [:payment-types/sorted])]
                ^{:key payment-type-id}
                [:div
                 [payment-input payments sale-id payment-type-id (:name (listen [:payment-type payment-type-id])) fill-total]
                 [:div.tiny-br]]))
             [:hr]
             [:div
              "Total"
              [:div.float-right
               (currency fill-total)]]
             [:div
              "Payments"
              [:div.float-right
               (currency (listen [:payment/total sale-id]))]]
             (let [balance (listen [:payment/balance sale-id fill-total])
                   has-change? (neg? balance)]
               [:div
                [:div.font-weight-bold {:class (when has-change? "text-warning")}
                 "Balance"
                 [:div.float-right
                  (currency balance)]]
                (when has-change?
                  [:div
                   [:a {:href no-href
                        :on-click #(dispatch [:modal/show :activate-gift-card sale-id (- balance)])}
                    [:i.fas.fa-plus] (spaces 2) "Put balance on gift card."]])])]]
     :footer (let [new-sale? (u/new-id? sale-id)
                   layaway? (:layaway? sale)]
               [one-time-button
                {:class "btn-dark btn-wide"
                 :disabled (or (when new-sale?
                                 (listen [:new/invalid? :type/sale]))
                               (when-not layaway?
                                 (not (listen [:new.sale/sufficient-payment sale-id]))))
                 :on-click (fn []
                             (cond
                               new-sale? (dispatch [:sale/finish])
                               layaway? (dispatch [:sale.layaway/take-payment sale-id]))
                             (dispatch [:modal/close]))}
                (cond
                  (and new-sale? layaway?) "Create Layaway"
                  new-sale? "Finish Sale"
                  layaway? "Take Layaway Payment")])]))

(defn use-gift-card-1 [sale-id fill-total]
  (let [code (r/atom nil)]
    (fn []
      [modal
       :title "Use Gift Card"
       :body [:div.row
              [:div.col-lg-6.offset-lg-3
               [form-group "Code"
                [text-input (atom-bindings code)]]]]
       :footer [:div
                [:button.btn.btn-secondary.btn-wide
                 {:type "button"
                  :on-click #(dispatch [:modal/show :payment sale-id])}
                 "Back"]
                (spaces 3)
                [:button.btn.btn-dark.btn-wide
                 {:type "button"
                  :disabled (not @code)
                  :on-click #(dispatch [:payment/use-gift-card-next sale-id @code fill-total])}
                 "Go"]]])))

(defn use-gift-card-2 [sale-id gift-card-id initial-amount fill-total]
  (let [amount (r/atom initial-amount)]
    (fn []
      [modal
       :title "Use Gift Card"
       :body [:div.row
              [:div.col-lg-6.offset-lg-3
               [property "Code" (:code (listen [:gift-card gift-card-id]))]
               [:br]
               [property "Card Balance" (currency (listen [:gift-card/balance gift-card-id]))]
               [property "Sale Balance" (currency (listen [:payment/balance sale-id fill-total]))]
               [:br]
               [form-group "Charge"
                [money-input {:val amount
                              :save (fn [raw]
                                      (if (nil? raw)
                                        (reset! amount nil)
                                        (let [parsed (u/parse-number raw)]
                                          (when (and (number? parsed) (pos? parsed))
                                            (reset! amount (min parsed
                                                                (max 0 (fetch [:gift-card/balance gift-card-id]))))))))}]]]] ; max 0 just in case the gift card balance somehow gets negative. This could be a sub / pulled-out logic like :credit-account/available but whatever.
       :footer [:div
                [:button.btn.btn-secondary.btn-wide
                 {:type "button"
                  :on-click #(dispatch [:modal/show :payment sale-id])}
                 "Cancel"]
                (spaces 3)
                [:button.btn.btn-dark.btn-wide
                 {:type "button"
                  :on-click (fn []
                              (dispatch [:payment/set sale-id gift-card-id @amount])
                              (dispatch [:modal/show :payment sale-id]))}
                 "Add to Payment"]]])))







(defn activate-gift-card [sale-id amount]
  (let [id :new
        amount (r/atom amount)]
    (fn []
      [new-or-edit
       :id id
       :type :gift-card
       :title-text "Activate / Recharge Gift Card"
       :body (fn [gift-card]
               [:div
                [:div.row
                 [:div.col-lg-6
                  [form-group "Amount"
                   [money-input {:val amount
                                 :save (fn [raw]
                                         (if (nil? raw)
                                           (reset! amount nil)
                                           (let [parsed (u/parse-number raw)]
                                             (when (and (number? parsed) (pos? parsed))
                                               (reset! amount parsed)))))}]]]]
                [form-group "Code"
                 [text-input {:val (:code gift-card)
                              :save #(dispatch [:change id :type/gift-card :code %])}]]])
       :on-done #(dispatch [:sale/add-gift-card-charge sale-id @amount])
       :button-text "Go"
       :additional-invalid? #(not @amount)])))

(defn drawer [& {:keys [register title body submit]}]
  [modal
   :title title
   :body [:div.row
          [:div.col-md-8.offset-md-2
           body]]
   :footer [:div
            submit
            (spaces 8)
            [:button.btn.btn-secondary {:type "button"
                                        :on-click #(dispatch-sync [:open-tab (print/open-drawer register)])}
             "Open Drawer"]
            (spaces 2)
            [:button.btn.btn-secondary {:type "button"
                                        :on-click #(dispatch [:modal/close])}
             "Cancel"]]])

(defn atom-map-bindings [a k]
  {:val (get @a k)
   :save #(swap! a assoc k %)})

(defn open-close-register []
  (let [tally (r/atom {})]
    (fn [open-or-close register]
      [drawer
       :register register
       :title (case open-or-close
                :open "Open Register"
                :close "Close Register")
       :body [:div
              [:div.row
               [:div.col-md-8.offset-md-2
                (doall
                 (for [payment-type-id (listen [:payment-types/sorted])
                       :let [payment-type (listen [:payment-type payment-type-id])
                             cash? (= payment-type-id (fixture :fixture/payment-type-cash))]]
                   ^{:key (:id payment-type)}
                   [:div
                    [:label (:name payment-type)]
                    [money-input {:val (get-in @tally [:amounts (:id payment-type)])
                                  :save (fn [raw]
                                          (if (nil? raw)
                                            (swap! tally dissoc-in [:amounts (:id payment-type)])
                                            (let [amount (u/parse-number raw)]
                                              (when (and (number? amount) (pos? amount))
                                                (swap! tally assoc-in [:amounts (:id payment-type)] amount)))))}
                     (when cash?
                       {:disabled true})]
                    (when cash?
                      [:div
                       (when (= open-or-close :open)
                         [:small "Expected in drawer:" (spaces 2) (currency (listen [:expected-in-drawer register]))])
                       [:div.row.mt-2
                        [:div.:div.col-md-10.offset-md-1
                         [denomination-inputs
                          [100 50 20 10 5 1 0.25 0.10 0.05 0.01]
                          #(swap! tally assoc-in [:amounts (:id payment-type)] %)]]]])
                    [:div.tiny-br]]))
                (when (= open-or-close :close)
                  [:div
                   [:hr]
                   [:div.mb-3
                    [:label "Left in Drawer"]
                    [money-input {:val (:left-in-drawer @tally)
                                  :save (fn [raw]
                                          (if (nil? raw)
                                            (swap! tally dissoc :left-in-drawer)
                                            (let [amount (u/parse-number raw)]
                                              (when (and (number? amount) (pos? amount))
                                                (swap! tally assoc :left-in-drawer amount)))))}]]])]]
              (when (= open-or-close :close)
                [form-group "Notes"
                 [textarea (atom-map-bindings tally :notes)
                  {:rows 3}]])]
       :submit [loading-button
                {:loading? (sub [:active-register/working? register])}
                {:class "btn-dark btn-wide"
                 :on-click #(dispatch [:open-close-register [:modal/close] open-or-close register (select-keys @tally [:amounts :left-in-drawer :notes])])}
                "Submit"]])))

(defn open-register [register]
  [open-close-register :open register])

(defn close-register [register]
  [open-close-register :close register])

(defn drawer-adjustment [type register]
  (let [reg-adj (listen [:register-adjustment/new])]
    [drawer
     :register register
     :title (case type
              :add "Add Amount"
              :payout "Payout")
     :body [:div
            [:div.row
             [:div.col-md-8.offset-md-2
              [form-group
               [money-input {:val (:amount reg-adj)
                             :save #(dispatch [:register-adjustment/change :new :amount %])}]]
              [form-group "Type"
               [select {:val (:payment-type reg-adj)
                        :save #(dispatch [:register-adjustment/change :new :payment-type %])
                        :opts (listen [:select/payment-types])}]]]]
            [form-group "Notes"
             [textarea {:val (:notes reg-adj)
                        :save #(dispatch [:register-adjustment/change :new :notes %])}
              {:rows 3}]]]
     :submit [one-time-button
              {:class "btn-dark btn-wide"
               :disabled (listen [:new/invalid? :type/register-adjustment])
               :on-click (fn []
                           (dispatch [:register-adjustment/create type register])
                           (dispatch [:modal/close]))}
              "Submit"]]))

(defn email-receipt [sale-id prefill]
  (let [opts (r/atom (merge prefill {:subject "Sale Receipt"
                                     :header "Thank you for shopping with us!"}))]
    (fn []
      [modal
       :title "Email Receipt"
       :body [:div.row
              [:div.col-lg-8.offset-lg-2
               (let [bindings (fn [k]
                                {:val (get @opts k)
                                 :save #(swap! opts assoc k %)})]
                 [:div
                  [form-group "To Email"
                   [email-input (bindings :email)]]
                  [form-group "To Name"
                   [text-input (bindings :name)]]
                  [form-group "Subject"
                   [text-input (bindings :subject)]]
                  [form-group "Header Message"
                   [textarea (bindings :header)
                    {:rows 3}]]
                  [form-group "Footer Message"
                   [textarea (bindings :footer)
                    {:rows 3}]]])]]
       :footer [one-time-button
                {:class "btn-dark btn-wide"
                 :disabled (not (:email @opts))
                 :on-click (fn []
                             (dispatch [:send-email-receipt sale-id @opts])
                             (dispatch [:modal/close]))}
                "Send"]])))

(defn export-items []
  [modal
   :size :xl
   :title "Filter Your Items"
   :body [items-table nil nil
          (fn [results]
            [:div
             [:br]
             [:div.tiny-br]
             [:div.text-center
              [:button.btn.btn-dark.btn-wide {:on-click #(dispatch [:export-items results])}
               "Download Export File"]]])]])

(defn showstopper [{:keys [heading text bad?]}]
  [modal
   :size :lg
   :body [:div.text-center
          [:h1.h2.mb-4 {:class (when bad? "text-danger")}
           heading]
          [:p text]]
   :footer [:button.btn.btn-dark.btn-wide {:on-click #(js/window.location.reload)}
            "Reload"]])






;;;; Pages

;;; Helpers

;; Due to this theme's dark background text doesn't look very good against it, use this to put all page bodies not already structured as panels into a single big one.
(defn lamina [& body]
  [:div.container-fluid
   [:div.row
    [:div.col
     [:div.card.shadow
      (into
       [:div.card-body]
       body)]]]])

;;; Navbar

(defn navbar
  ([header] (navbar header nil nil))
  ([header right] (navbar header nil right))
  ([header left right]
   (let [navbar-collapse-id "navbarCollapse"]
     [:nav.navbar.navbar-expand-lg.navbar-light.bg-white.topbar.mb-4.static-top.shadow
      [:button#sidebarToggleTop.btn.btn-link.d-md-none.rounded-circle.mr-3 {:on-click #(dispatch [:toggle-sidebar-shown])}
       [:i.fas.fa-bars]]
      [:h1.h3.mb-0.text-gray-800.mr-3 header]
      (when (or left right)
        [:button.navbar-toggler {:data-toggle "collapse"
                                 :data-target (str "#" navbar-collapse-id)}
         [:span.navbar-toggler-icon]])
      (into
       [:div.collapse.navbar-collapse {:id navbar-collapse-id}]
       (for [side [left right]]
         (when side
           (into
            [:ul.navbar-nav {:class (when (= side right)
                                      "ml-auto")}]
            (for [item (if (vector? side)
                         [side] (remove nil? side))]
              item)))))])))

(defn navbar-link [attrs & children]
  [:li.nav-item
   (into
    [:a.nav-link attrs]
    children)])

(defn navbar-button [attrs & children]
  [:li.nav-item.nav-link
   (into
    [:a.btn.btn-primary.text-white.text-nowrap attrs]
    children)])

(defn navbar-new-button [attrs child]
  [navbar-button attrs
   [:i.fas.fa-plus] (spaces 2) child])

(defn navbar-one-time-button [attrs child]
  [:li.nav-item.nav-link
   [one-time-button (merge attrs
                           {:class (str "btn-primary text-white text-nowrap" (:class attrs))})
    child]])

(def navbar-divider [:div.topbar-divider.mx-2.d-none.d-lg-block])

(defn append-delete-button
  ([id after] (append-delete-button id after nil))
  ([id after items]
   (when (listen [:auth? :delete-stock-management-entities])
     (concat items
             (list (when items
                     navbar-divider)
                   [navbar-button {:href no-href
                                   :class "btn-danger"
                                   :on-click #(dispatch [:store/delete id after])}
                    "Delete"])))))



;;; Pages

(defn dashboard []
  [:div
   [navbar "Dashboard"]
   [:div.container-fluid
    [:div.row
     [:div.col-xl-8.col-lg-7
      [panel "Earnings Last 7 Days"
       [:div.chart-area [chart (merge {:type :line}
                                      (listen [:charts/earnings-this-week]))]]]]
     [:div.col-xl-4.col-lg-5
      [panel "Sales Today by Brand"
       (let [opts (merge {:type :doughnut}
                         (listen [:charts/items-sold-today-by-designer]))]
         [:div
          [:div.chart-pie.pt-4.pb-2 [chart opts]]
          [:div.mt-4.text-center.small
           (when-let [x (nth (:labels opts) 0 nil)]
             [:span.mr-2
              [:i.fas.fa-circle.text-primary] " " x])
           (when-let [x (nth (:labels opts) 1 nil)]
             [:span.mr-2
              [:i.fas.fa-circle.text-success] " " x])
           (when-let [x (nth (:labels opts) 2 nil)]
             [:span.mr-2
              [:i.fas.fa-circle.text-info] " " x])]])]]]
    (when (listen [:auth? :any-settings])
      [:div.row
       [:div.col-lg-6
        [panel "Import/Export"
         [:button.btn.btn-primary.btn-wide {:on-click #(dispatch [:modal/show :export-items])}
          "Export Items"]
         [:br]
         [:br]
         [:label "Import Items"]
         [:ul
          [:li "Only use the file generated by" [:code " Export Items "] "for imoprting."]
          [:li "New items will be created for everything in the file."]]
         [file-input
          (fn [e]
            (let [file (-> e .-target .-files array-seq first)
                  reader (js/FileReader.)]
              (set! (.-onload reader)
                    #(dispatch [:import-items (-> % .-target .-result)]))
              (.readAsText reader file)))]]]])]])

(def home-page dashboard)

(defn signed-into-shop [continue-to]
  (when (listen [:register-chosen?])
    [navbar-link {:href no-href
                  :on-click (fn []
                              (when continue-to
                                (dispatch [:choose-shop/set-continue continue-to]))
                              (dispatch [:navigate :page/choose-shop]))}
     "Signed into " (:name (listen [:current-shop])) " — " (:name (listen [:current-register]))]))

(defn choose-shop-page []
  (let [selected-shop (r/atom (:id (fetch [:current-shop])))]
    (fn []
      [:div
       [navbar
        [breadcrumb [:a {:href (path :page/sales-landing)} "Sales"] "Shops"]
        [signed-into-shop]
        nil]
       [:div.container-fluid
        [:div.row
         [:div.col
          [panel "Choose Your Shop"
           [:div.row.justify-content-start
            (doall
             (for [shop (listen [:shops])]
               ^{:key (:id shop)}
               [:div.col-lg-3
                [tile {:href no-href
                       :class (if (= (:id shop) @selected-shop) "operative" "inoperative")
                       :on-click (fn []
                                   (dispatch [:unset-current-register])
                                   (reset! selected-shop (:id shop)))}
                 (:name shop) "fa-map-marker-alt"]]))]]
          (when @selected-shop
            [panel "Choose a Register"
             [:div.row.justify-content-start
              (doall
               (for [register (listen [:shop/registers @selected-shop])]
                 ^{:key (:id register)}
                 [:div.col-lg-3
                  [tile {:href no-href
                         :class (when (= (:id register) (:id (listen [:current-register]))) "operative")
                         :on-click #(dispatch [:choose-shop/register-chosen (:id register)])}
                   (:name register) "fa-cash-register" (if (listen [:active-register/open? (:id register)])
                                                         "OPEN" "CLOSED")]]))]])]]]])))

(defn sales-landing-page []
  [:div
   [navbar "Sales" [signed-into-shop (path :page/sales-landing)] nil]
   [:div.container-fluid
    [:div.row
     [:div.col
      [panel "Sales"
       [:div.row.justify-content-start
        [:div.col-lg-3
         [tile {:href (path :page/new-sale)} "New Sale" "fa-plus"]]
        #_[:div.col-lg-3
           [tile {:href (path :page/customer-display)
                  :target "_blank"}
            "Customer Display" "fa-tablet-alt"
            (str "Passkey: " (listen [:passkey]))]]]
       (when-let [held-receipts (listen [:held-receipts/sorted])]
         [:div
          [:br]
          [:strong "Held Receipts"]
          [:ul
           (doall
            (for [i held-receipts]
              ^{:key (:e/id i)}
              [:li
               [:button.btn.btn-primary.btn-xs {:on-click #(dispatch [:held-receipts/continue (:e/id i)])}
                "Continue"]
               " "
               [:button.btn.btn-primary.btn-xs {:on-click #(dispatch [:held-receipts/cancel (:e/id i)])}
                "Cancel"]
               (spaces 2)
               (date (:e/date i) :full)
               " — "
               (if-let [customer (:sale/customer i)]
                 (listen [:customer/name customer])
                 "No customer")
               (when-let [lines (seq (:sale/lines i))]
                 (str
                  " — "
                  (let [preview-num 3]
                    (str
                     (str/join
                      ", "
                      (for [sku (->> lines
                                     (filter #(= (:line-type %) :sku)) ; Make things simple and just show items.
                                     (map :sku)
                                     (take preview-num))]
                        (listen [:sku/name sku :full? true])))
                     (when (< preview-num (count lines))
                       "...")))))]))]])]
      [panel "Register"
       (let [drawer-auth? (listen [:auth? :modal/drawer-adjustment])]
         [:div.row.justify-content-start
          [:div.col-lg-3
           [tile {:href no-href
                  :on-click #(dispatch [:modal/show :close-register (:id (fetch [:current-register]))])} "Close Register" "fa-power-off"]]
          (when drawer-auth?
            [:div.col-lg-3
             [tile {:href no-href
                    :on-click #(dispatch [:modal/show :drawer-adjustment :payout (:id (fetch [:current-register]))])} "Payout" "fa-minus-square"]])
          (when drawer-auth?
            [:div.col-lg-3
             [tile {:href no-href
                    :on-click #(dispatch [:modal/show :drawer-adjustment :add (:id (fetch [:current-register]))])} "Add Amount" "fa-plus-square"]])])]]]]])

(let [register-complete-page
      (fn []
        (let [print-fn (atom nil)]
          (fn [type id]
            (let [doc (listen [type id])]
              [:div
               [navbar
                [:a {:href (path :page/sales-landing)} "Sales"]
                [signed-into-shop]
                nil]
               [:div.container-fluid
                [:div.row
                 [:div.col
                  (case type
                    :sale
                    [:div
                     [:h1
                      [:i.fas.fa-star.text-warning] (spaces 2) "Thank you!"]
                     [:br]
                     [:small "The sale has been recorded."]
                     [:br]
                     [:p "Change: " [:span.text-success (currency (q/sale-cash-change id))]]]

                    :register-count
                    [:p "Register closed and counts submitted. " (when (listen [:auth? :page/closing-count])
                                                                   [:a {:href (path :page/closing-count id)}
                                                                    "View/edit closing count."])]

                    :register-adjustment
                    (let [amt (:amount doc)]
                      [:p
                       (plus-minus amt currency)
                       (spaces 2)
                       (cond
                         (pos? amt) "added to"
                         (neg? amt) "withdrawn from")
                       " register."]))
                  [:br]
                  [panel "Next Steps"
                   [:div.row.justify-content-start
                    (when-not (= type :register-count)
                      [:div.col-lg-3
                       [tile {:href (path :page/new-sale)} "New Sale" "fa-plus"]])
                    [:div.col-lg-3
                     [tile {:href no-href
                            :on-click #(@print-fn)} "Print Receipt" "fa-print"]]
                    (when (= type :sale)
                      [:div.col-lg-3
                       [tile {:href no-href
                              :on-click #(dispatch [:show-email-receipt-modal id])} "Email Receipt" "fa-envelope"]])
                    (when (= type :sale)
                      [:div.col-lg-3
                       [tile {:href no-href
                              :on-click #(dispatch-sync [:open-tab (print/sale (fetch [:sale id]) :gift-receipt? true)])} "Gift Receipt" "fa-gift"]])]]
                  [:br]
                  [panel "Receipt"
                   [:div.row
                    [:div.col-md-8.offset-md-2
                     [print-preview {:doc doc
                                     :render (case type
                                               :sale print/sale
                                               :register-count print/closing-count
                                               :register-adjustment print/register-adjustment)
                                     :print-prompt? (= type :sale)
                                     :print-fn print-fn}]]]]]]]]))))]

  (defn sale-complete-page [id]
    [register-complete-page :sale id])

  (defn register-count-complete-page [id]
    [register-complete-page :register-count id])

  (defn register-adjustment-complete-page [id]
    [register-complete-page :register-adjustment id]))

(defn sale-line-options [sale index]
  (let [options-shown? (r/atom false)]
    (fn []
      [:span
       [:a.text-decoration-none
        {:href no-href
         :on-click #(swap! options-shown? not)}
        [:i.fas {:class (if @options-shown? "fa-caret-down" "fa-caret-up")}]
        (spaces 2)
        (str (if @options-shown? "Hide" "More") " line options")]
       (when @options-shown?
         [nested-table
          :columns
          [{:span 6
            :cmp [form-group "Employee"
                  [select {:val (get-in @sale [:lines index :employee])
                           :save #(dispatch [:sale/change-line-employee* :new index %])
                           :opts (listen [:select/employees])}]]}
           {:span 3
            :cmp (when (= (get-in @sale [:lines index :line-type]) :sku)
                   [form-group "Discount"
                    [percent-input {:val (sub [:sale/sku-line-discount :new index])
                                    :save #(dispatch [:sale/set-line-discount :new index %])}]])}
           {:span 3}]])]))) ; The other columns fill up all width despite having spans set without this empty column to finish off the 12 columns.

(defn sale-tab-individually [sale]
  [:div
   [plus-button "Add Sku"
    {:on-click (letfn [(show-search []
                         (let [add-fn (fn [sku]
                                        (dispatch [:sale/add :new (:id sku)])
                                        (dispatch [:modal/close]))]
                           (dispatch [:modal/show :item-search
                                      (fn [item]
                                        (dispatch [:modal/show :sku-picker (:id item) add-fn show-search]))
                                      add-fn])))]
                 show-search)}]
   (spaces 3)
   [plus-button "Activate / Recharge Gift Card"
    {:on-click #(dispatch [:modal/show :activate-gift-card (:id @sale)])}]
   (when-let [credit-account-id (listen [:customer/credit-account (:customer @sale)])]
     [:span
      (spaces 3)
      [plus-button "Credit Account Deposit"
       {:on-click #(dispatch [:sale/add-credit-account-deposit (:id @sale) credit-account-id])}]])
   [:br]
   [:br]
   (when-let [lines (seq (:lines @sale))]
     [table
      :id :table/sale-tab
      :source (map-indexed vector lines)
      :sort [::dummy :asc] ; Dummy sort key that always results in nil so items aren't sorted.
      :columns [{:label "Description"
                 :render
                 (fn [[index line]]
                   [:span
                    (case (:line-type line)
                      :sku
                      (let [sku (listen [:sku (:sku line)])
                            item (listen [:item (:item sku)])]
                        [nested-table
                         :columns
                         [{:span 2 :center? true
                           :cmp [:a {:href (path :page/item item)}
                                 [hoverzoom (listen [:sku/primary-image (:id sku) :inherit? true]) 70]]}
                          {:span 6
                           :cmp [:span
                                 [:a {:href (path :page/item item)}
                                  (:name item)]
                                 [:br]
                                 [:div.small (if (:date item)
                                                       (str "Created on " (date (:date item)))
                                                       "Unknown creation date")] ; I think items always have a create date (even the ones from the migration) so this may never happen.
                                 [:div.small "Shop stock" (spaces 2) [:strong.text-stock (listen [:sku/stock (:id sku) (:id (listen [:current-shop]))])]]
                                 
                                 [:div.small "Total stock" (spaces 2) [:strong.text-stock (listen [:sku/stock (:id sku)])]]
                                 [:div.small
                                  [:a {:href no-href
                                       :on-click (fn []
                                                   (dispatch [:new-sale/transfer-stock (:id item) (:id sku) (:id (listen [:current-shop]))])
                                                   (dispatch [:navigate :page/new-transfer]))}
                                   "Transfer stock"] (spaces 2)
                                  [help {:text "Consider creating a transfer to this shop if there's not enough stock to cover the purchase. You can still complete the sale without doing so."}]]]}
                          {:span 2 :center? true
                           :cmp [:a {:href no-href
                                     :on-click #(dispatch [:navigate-sku (:id sku)])}
                                 [:div.small (:name (listen [:manufacturer (:manufacturer item)]))]
                                 [:div.small (listen [:sku/name (:id sku)])]]}
                          {:span 2 :center? true
                           :cmp (currency (listen [:sku/price (:id sku) :inherit? true]))}]])
                      :credit-account
                      "Credit Account Deposit"
                      :gift-card
                      (str "Gift Card Purchase: " (:code line)))
                    [:div.tiny-br]
                    [sale-line-options sale index]])}
                {:label "Price" :span 2 :center? true :sort-fn #(-> % second :price)
                 :render
                 (fn [[index _]]
                   [money-input {:val (get-in @sale [:lines index :price])
                                 :save #(dispatch [:sale/change-line-price :new index %])}])}
                {:center? true
                 :render
                 (fn [[index _]]
                   [:a {:href no-href
                        :on-click #(dispatch [:sale/remove :new index])}
                    [:i.fas.fa-times]])}]
      ;; TODO :item-key ??
      ])])

(defn sale-tab-bulk [sale]
  [:div
   [add-item-buttons [:sale.bulk/add :new] [:sale/add :new]]
   [:br]
   [:br]
   [bulk-sale-items-table
    :sale sale
    :editable? true
    :columns [{:center? true
               :render (fn [item]
                         [:a {:href no-href
                              :on-click #(dispatch [:sale.bulk/remove :new (:id item)])}
                          [:i.fas.fa-times]])}]]])

(defn sale-tab* [sale]
  [:div
   (if (:customer @sale)
     (let [customer (listen [:customer (:customer @sale)])]
       [:div.clearfix
        (when-let [credit-account-id (listen [:customer/credit-account (:id customer)])]
          [:div.float-right
           [property "Credit Limit" (currency (:limit (listen [:credit-account credit-account-id])))]
           [property "Balance" (currency (listen [:credit-account/balance credit-account-id]))]
           [property "Available" (currency (listen [:credit-account/available credit-account-id]))]])
        [tier-badge (:customer @sale) {:style {:margin-top "-5px"}}] (spaces 3)
        [customer-with-remove-button (:id customer) [:sale.new/set-customer :new nil]]
        [:div "Tier: " (spaces 2) (humanize (get u/tiers (q/customer:tier (:id customer)))) " (" (q/customer:tier-discount (:id customer)) "% off)"]
        (when-let [type (:type customer)]
          (let [customer-type (listen [:customer-type type])]
            (when-let [discount (:discount customer-type)]
              [:div "Type:" (spaces 2) (:name customer-type) " (" discount "% off)"])))
        (when-let [notes (:notes customer)]
          [:div "Notes:" (spaces 2) notes])
        [checkbox {:val (:layaway? @sale)
                   :save #(dispatch [:sale/convert-or-unconvert-to-layaway :new %])}
         "Layaway?"
         {:class "mt-3"}]])
     [add-customer-buttons [:sale.new/set-customer :new]])
   [:hr]
   (when (listen [:auth? :bulk-sale-view])
     [checkbox {:val (:bulk? @sale)
                :save #(dispatch [:new-sale.bulk/toggle-bulk-view :new %])}
       "Bulk Sale"
       {:class "mb-3"}])
   (if-not (:bulk? @sale)
     [sale-tab-individually sale]
     [sale-tab-bulk sale])])

(defn sale-tab [sale]
  (let [interval-id (js/setInterval #(dispatch [:customer-display/maybe-present-sale]) 750)] ; re-frame docs say this should ideally be done with an event/effect (also there are other js/setIntervals in the views): https://github.com/Day8/re-frame/blob/master/docs/FAQs/PollADatabaseEvery60.md
    (r/create-class
     {:component-will-unmount #(js/clearInterval interval-id)
      :reagent-render
      (fn []
        [sale-tab* sale])})))

(defn refund-tab []
  (let [refund (listen [:refund/new])
        refund-sale (listen [:sale (:sale refund)])]
    [:div
     [:div "Refunding from sale: " [:a {:href (path :page/sale (:id refund-sale))}
                                    (:code refund-sale)]]
     (let [refund-amount (listen [:refund/amount :new])]
       [:div "Amount to refund: " [:span.font-weight-bold {:class (when (pos? refund-amount) "text-success")}
                                   (currency refund-amount)]])
     [:br]
     (when-let [lines (seq (:lines refund-sale))]
       [table
        :id :table/refund-tab
        :source lines
        :sort [::dummy :asc] ; Dummy sort key that always results in nil so items aren't sorted.
        :columns [{:label "Refund?" :center? true
                   :render
                   (fn [sale-line]
                     (when (and (= (:line-type sale-line) :sku)
                                (not (sale-line-refunded? sale-line)))
                       ;; Don't allow refunding gift card lines: This makes my life simpler (no extra logic in gift card balance calculation to see if gift card lines were refunded) and because there's currently no mechanism to prevent someone from getting a full refund for a gift card from which they're already spent. Also "refunding a gfit card" is pretty much the same as just spending it. Ditto for credit account deposits.
                       [checkbox {:val (some #{(:id sale-line)} (:sale-lines refund))
                                  :save (fn [checked?]
                                          (if checked?
                                            (dispatch [:refund/add-sale-line :new (:id sale-line)])
                                            (dispatch [:refund/remove-sale-line :new (:id sale-line)])))}]))}
                  {:label "Description"
                   :render
                   (fn [sale-line]
                     (let [line-type (:line-type sale-line)]
                       (case line-type
                         :sku
                         (let [sku (listen [:sku (:sku sale-line)])
                               item (listen [:item (:item sku)])]
                           [nested-table
                            :columns
                            [{:span 3 :center? true
                              :cmp [:a {:href (path :page/item item)}
                                    [hoverzoom (listen [:sku/primary-image (:id sku) :inherit? true]) 70]]}
                             {:span 6
                              :cmp [:a {:href (path :page/item item)}
                                    (:name item)]}
                             {:span 3 :center? true
                              :cmp [:a {:href no-href
                                        :on-click #(dispatch [:navigate-sku (:id sku)])}
                                    [:span.detail.small (:name (listen [:manufacturer (:manufacturer item)]))]
                                    [:br]
                                    [:span.detail.small (listen [:sku/name (:id sku)])]]}]])
                         :credit-account
                         "Credit Account Deposit"
                         :gift-card
                         (str "Gift Card Purchase: " (:code sale-line)))))}
                  {:label "Price" :span 2 :center? true :sort-fn #(-> % second :price)
                   :render
                   (fn [sale-line]
                     (currency (:price (first (filter #(= (:id %) (:id sale-line)) (:lines refund-sale))))))}]])]))

(defn refund-section [sale]
  [pill-tabs
   :refund-tab
   {:name :refund-tab
    :tab-body "Refund"
    :tab-badge-body (count (:sale-lines (listen [:refund/new])))
    :pane [refund-tab]}
   {:name :sale-tab
    :tab-body "Sale"
    :tab-badge-body (count (:lines @sale))
    :pane [sale-tab sale]}])

(defn new-sale-page []
  (let [sale (sub [:sale/new])]
    [:div.new-sale-page
     [navbar
      [breadcrumb [:a {:href (path :page/sales-landing)} "Sales"] "New"]
      [signed-into-shop (path :page/new-sale)]
      [navbar-button {:href no-href
                      :on-click #(dispatch-sync [:webusb/connect])}
       (if (seq (listen [:webusb/device-names]))
         (str "Connected to " (listen [:webusb/device-names]))
         "Connect to Pole Display...")]]
     [:div.container-fluid
      [:div.row
       [:div.col-lg-9
        [:div.card.shadow
         [:div.card-body
          (if (q/is-sale-a-refund? @sale)
            [refund-section sale]
            [sale-tab sale])]]]
       [:div.col-lg-3
        [:div.card.shadow
         [:ul.list-group.list-group-flush
          [:li.list-group-item
           [:div.totals-line
            [:div "Subtotal"]
            [:div (currency (listen [:sale/subtotal :new]))]]
           [:div.totals-line
            [checkbox {:val (not (:no-tax? @sale))
                       :save #(dispatch [:sale/change :new :no-tax? (not %)])}
             "Tax"]
            [:div (currency (listen [:sale/tax :new]))]]
           [:div.totals-line
            [:div "Shipping"]
            [:div.w-50 [money-input {:val (:shipping-cost @sale)
                                     :save #(dispatch [:sale/change :new :shipping-cost %])}]]]]
          [:li.list-group-item
           [:strong "Total"]
           [:div.float-right
            [:strong (currency (listen [:sale/total :new]))]]]
          [:li.list-group-item
           [:button.btn.btn-primary.btn-block
            {:disabled (listen [:new/invalid? :type/sale])
             :on-click (fn []
                         (dispatch [:customer-views/send :total])
                         (dispatch [:modal/show :payment (:id @sale)]))}
            "Payment"]
           [:div.tiny-br]
           [:button.btn.btn-secondary.btn-block
            {:on-click (fn []
                         (dispatch [:clear-sale-and-refund])
                         (dispatch [:navigate :page/sales-landing]))}
            "Cancel"]]]]]]]]))

(defn items-page []
  [:div
   [navbar [breadcrumb "Inventory" "Items"]
    [navbar-new-button {:href no-href
                        :on-click (fn []
                                    (dispatch [:modal/show :new-item [:navigate :page/item]]))}
     "New Item"]]
   [lamina
    [items-table]]])

(defn item-nav [item]
  [navbar
   [breadcrumb
    [:a {:href (path :page/items)} "Items"]
    [:span
     [:a {:href (path :page/item @item)}
      (listen [:item/full-name (:id @item)])]
     (spaces 2)
     [:span.badge.badge-pill.badge-stock.badge-little (listen [:item/stock (:id @item)])]]]
   (list [navbar-link {:href (path :page/item-history @item)} "History"]
         [:li.nav-item.dropdown
          [dropdown-toggle
           [:a.nav-link.dropdown-toggle {:href no-href
                                         :data-toggle "dropdown"}
            "Add to... "]]
          [:div.dropdown-menu.shadow.animated--grow-in
           [:a.dropdown-item {:href no-href
                              :on-click (fn []
                                          (dispatch [:modal/show :sku-picker (:id @item)
                                                     (fn [sku]
                                                       (dispatch [:sale/add :new (:id sku)])
                                                       (dispatch [:modal/close])
                                                       (dispatch [:navigate :page/new-sale]))]))}
            "Sale"]
           [:a.dropdown-item {:href no-href
                              :on-click (fn []
                                          (dispatch [:order/add :new (:id @item)])
                                          (dispatch [:navigate :page/new-order]))}
            "Order"]
           [:a.dropdown-item {:href no-href
                              :on-click (fn []
                                          (dispatch [:transfer/add :new (:id @item)])
                                          (dispatch [:navigate :page/new-transfer]))}
            "Transfer"]]])
   (list [navbar-button {:href no-href
                         :on-click #(dispatch [:modal/show :print-labels (:id @item)])}
          "Print Labels"]
         [navbar-button {:href no-href
                         :on-click #(dispatch [:item/duplicate (:id @item)])}
          "Duplicate"])])

(let [sku-cell
      (fn [sku item-id color size]
        (if sku
          [nested-table
           :class "m-2"
           :columns (if sku
                      [{:cmp [:a.text-decoration-none
                              {:href no-href
                               :on-click #(dispatch [:modal/show :edit-sku (:id @sku)])}
                              [:img {:src (thumbnail-url (listen [:sku/primary-image (:id @sku) :inherit? true]) 80)}]
                              ;; Got rid of this because it will be deceptive if there are negative and positive sku stocks that cancel out.
                              ;; (if (zero? (listen [:sku/stock (:id @sku)]))
                              ;;   [:strong "No stock"]
                              ;;   ...)
                              [:table.table.table-sm.mt-1.mb-0.border-bottom-0
                               [:tbody
                                (doall
                                 (for [shop (cons nil (listen [:shops]))
                                       :let [td :td.text-left.text-nowrap.border-0.py-0]]
                                   ^{:key (or (:id shop) :total)}
                                   [:tr
                                    [td (or (:name shop) [:strong "Total"])]
                                    [td
                                     [:strong.text-stock (listen [:sku/stock (:id @sku) (:id shop)])]]]))]]]}
                       {:cmp [:span
                              (doall
                               (for [[text k] [["UPC" :upc]
                                               ["EAN" :ean]
                                               ["Style Number" :style-number]
                                               ["Custom SKU" :custom-sku-number]]]
                                 ^{:key k}
                                 [text-input {:val (get @sku k)
                                              :save #(dispatch [:sku/change (:id @sku) k %])}
                                  {:class "form-control-sm code-input mb-2"
                                   :placeholder text}]))]}])]
          [:button.btn.btn-primary.m-4 {:on-click #(dispatch [:sku/create item-id color size])}
           " " [:i.fas.fa-plus] " "]))]
  (defn item-page [id]
    (let [item (sub [:item id])]
      [:div.item-page
       [item-nav item]
       [:div.container-fluid
        [:div.row
         [:div.col-md
          [images :item item]
          [panel "Properties"
           [property "Created on" (date (:date @item))]
           (when-let [employee (:employee @item)]
             [property "Created by" [employee-link employee]])
           [:div.tiny-br]
           [form-group "Name" [text-input {:val (:name @item)
                                           :save #(dispatch [:item/change id :name %])}]]
           [form-group "Manufacturer" [select {:val (:manufacturer @item)
                                               :save #(dispatch [:item/change id :manufacturer %])
                                               :opts (listen [:select/manufacturers :none? true])}]]
           [form-group "Category" [select {:val (:category @item)
                                           :save #(dispatch [:item/change id :category %])
                                           :opts (listen [:select/categories :none? true])}]]]]
         [:div.col-md
          [pricing :item item]
          [panel "Notes"
           [textarea
            {:val (:notes @item)
             :save #(dispatch [:item/change id :notes %])}
            {:rows 14}]]]
         [:div.col-md
          [panel "Sku Attributes"
           [form-group "Colors" [tags-input {:val (:colors @item)
                                             :save #(dispatch [:item/change id :colors %])}]]
           [form-group "Sizes" [tags-input {:val (:sizes @item)
                                            :save #(dispatch [:item/change id :sizes %])}]]]
          [panel "Tags"
           [tags-input {:val (:tags @item)
                        :save #(dispatch [:item/change id :tags %])}]]
          (when (listen [:auth? :set-max-commission])
            [panel "Settings"
             [form-group [:span "Maximum Commission" (spaces 2) [help {:text "Item commission increases with age (the time since it was most recently ordered). If set the item will not have a commission greater than this value."}]]
              [percent-input {:val (:max-commission @item)
                              :save #(dispatch [:item/change id :max-commission %])}]]])]]
        [:div.row
         [:div.col
          [panel "Skus"
           [:button.btn.btn-primary {:on-click #(dispatch [:modal/show :upcs (:id @item)])}
            "Enter UPCs"]
           [:br][:br]
           [matrix-responsive id sku-cell]]]]]])))

(defn item-history-page [id]
  [:div
   [item-nav (sub [:item id])]
   [:div.container-fluid
    [panel "History"
     [table
      :id (str :item-stock-history "-" id)
      :source (fn [params]
                [:item/stock-history id params])
      :sort [:order :desc]
      :filters (fn [tid]
                 [:div.row
                  [:div.col-md-4.col-lg-3
                   [form-group "Date"
                    [datepicker-range (param-bindings tid :date-range)]]]
                  [:div.col-md-4.col-lg-3
                   [form-group "Sku"
                    [select (merge (param-bindings tid :sku)
                                   {:opts (listen [:select/skus :item id :all? true])})]]]])
      :extra (fn [tid]
               [:div.row
                [:div.col-md-4.col-lg-3
                 [form-group "Event"
                  [select (merge (param-bindings tid :type)
                                 {:opts u/event-opts})]]]
                [:div.col-md-4.col-lg-3
                 [form-group "Shop"
                  [select (merge (param-bindings tid :shop)
                                 {:opts (listen [:select/shops :all? true])})]]]
                [:div.col-md-4.col-lg-3
                 [form-group "Employee"
                  [select (merge (param-bindings tid :employee)
                                 {:opts (listen [:select/employees :all? true])})]]]])
      :columns [{:label "Event" :key :type :center? true
                 :render (fn [type]
                           [stamp :event type])}
                {:label "ID" :key :id :center? true}
                (assoc (:sku table-cols) :key #(listen [:sku/name (:sku %)]))
                {:label "Change" :key :adj :center? true :render plus-minus}
                {:label "Running Total" :key :running-total :center? true}
                :shop
                :employee
                {:label "Occurred" :center? true :sort-fn :order :render (fn [_ i]
                                                                           (date (:date i) :full))}]
      :item-key (fn [i]
                  (str/join "-" ((juxt :id :shop :sku) i))) ;; :order would work too. I'm keeping this more general in case I one day merge the :stock-history-report table and this one.
      :row-on-click (fn [i]
                      (let [type (:type i)]
                        (when-not (= type :bot)
                          (dispatch [:navigate (case type
                                                 :sale :page/sale
                                                 :order :page/order
                                                 :transfer :page/transfer
                                                 :count :page/count) i]))))
      :row-attrs (fn [i]
                   (when (= :bot (:type i))
                     {:style {:cursor "initial"}}))]]]])

(defn categories-nav []
  [navbar [breadcrumb "Inventory" [:a {:href (path :page/categories)} "Categories"]]
   [navbar-link {:href (path :page/categories-browse)} "Browse"]
   [navbar-new-button {:href no-href
                       :on-click #(dispatch [:modal/show :edit-category :new])}
    "New Category"]])

(defn categories-page []
  [:div
   [categories-nav]
   [lamina
    [table
     :source [:categories]
     ;; :sort [#(fetch [:category/path (:id %)]) :asc]
     :search [#(fetch [:category/path (:id %)])]
     :columns [[:label "Full Name" :key #(listen [:category/path (:id %)])]
               :date]
     :row-on-click #(dispatch [:modal/show :edit-category (:id %)])]]])

(defn category-cmp []
  (let [expanded? (r/atom false)]
    (fn [category top-level?]
      (let [children (sub [:category/children (:id category)])]
        [:div (when-not top-level?
                {:class "ml-4"})
         (-> [:ol.breadcrumb]
             (into (for [seg (str/split (listen [:category/path (:id category)]) #" / ")] ; TODO Hacky.
                     ^{:key seg}
                     [:li.breadcrumb-item seg]))
             (conj (when (seq @children)
                     [:span (spaces 2) [:a {:href no-href
                                            :on-click #(swap! expanded? not)}
                                        [:i.fas {:class (if @expanded?
                                                          "fa-minus" "fa-plus")}]]])))
         (when @expanded?
           (for [child @children]
             ^{:key (:id child)}
             [category-cmp child false]))]))))

(defn categories-browse-page []
  [:div
   [categories-nav]
   [lamina
    (for [category (listen [:categories/top-level])]
      ^{:key (:id category)}
      [category-cmp category true])]])

(defn manufacturers-page []
  [:div
   [navbar [breadcrumb "Inventory" "Manufacturers"]
    [navbar-new-button {:href no-href
                        :on-click #(dispatch [:modal/show :edit-manufacturer :new])}
     "New Manufacturer"]]
   [lamina
    [table
     :source [:manufacturers]
     :search [:name]
     :columns [:name
               :date]
     :row-on-click #(dispatch [:modal/show :edit-manufacturer (:id %)])]]
   ;; Prospective rewrite.
   ;; [table
   ;;  :source [[:e/type :type/manufacturer]]
   ;;  :search [:manufacturer/name]
   ;;  :columns [{:label "Name" :key :manufacturer/name}
   ;;            {:label "Created" :key :e/date :center? true :render date}]]
   ])

(defn orders-page []
  [:div
   [navbar [breadcrumb "Inventory" "Orders"]
    [navbar-new-button {:href (path :page/new-order)}
     "New Order"]]
   [lamina
    [table
     :source [:orders]
     :search [:vendor]
     :extra (fn [tid]
              [:div
               [:div.row
                [:div.col-md-4.col-lg-3
                 [form-group "Ordered"
                  [datepicker-range (filter-bindings tid q/date-within?)]]]
                [:div.col-md-4.col-lg-3
                 [form-group "Received"
                  [datepicker-range (filter-bindings tid q/received-on-within?)]]]
                [:div.col-md-4.col-lg-3
                 [form-group "Expected"
                  [datepicker-range (filter-bindings tid q/expected-on-within?)]]]]
               [:div.row
                [:div.col-md-4.col-lg-3
                 [form-group "Shop"
                  [select (merge (filter-bindings tid :shop :wrap? true)
                                 {:opts (listen [:select/shops :all? true])})]]]
                [:div.col-md-4.col-lg-3
                 [form-group "Employee"
                  [select (merge (filter-bindings tid :employee :wrap? true)
                                 {:opts (listen [:select/employees :all? true])})]]]]])
     :columns [[:label "Status" :key :status :center? true
                :render (fn [status]
                          [stamp :order status])]
               [:label "Vendor" :key :vendor]
               :shop
               [:label "# Ordered" :key #(listen [:order/pieces (:id %)]) :center? true]
               [:label "# Received" :key #(listen [:order/received-pieces (:id %)]) :center? true]
               [:label "# Pending" :key #(listen [:order/pending-pieces (:id %)]) :center? true]
               [:label "Total" :key #(listen [:order/total (:id %)]) :center? true :render currency]
               [:label "Pending Total" :key #(listen [:order/pending-total (:id %)]) :center? true :render currency]
               [:label "# Sold" :key #(q/order:num-sold (:id %)) :center? true]
               :employee
               (merge (:date table-cols) {:label "Ordered"})
               (merge (:date table-cols) {:label "Excpected" :key :expected-on})
               (merge (:date table-cols) {:label "Received" :key :received-on})
               (merge (:date table-cols) {:label "Due" :key :due})]
     :row-route :page/order
     :row-attrs (fn [i]
                  (when (and (:due i)
                             (> (u/now) (:due i)))
                    {:class "table-danger"}))
     :export :export/orders
     :bottom-child (fn [results]
                     [table-summary
                      [{:label "# Ordered" :key :num-ordered}
                       {:label "# Received" :key :num-received}
                       {:label "# Pending" :key :num-pending}
                       {:label "Total" :key :total :render currency}
                       {:label "Pending Total" :key :pending-total :render currency}
                       {:label "# Sold" :key :num-sold}]
                      {:num-ordered (apply + (map #(listen [:order/pieces (:id %)]) results))
                       :num-received (apply + (map #(listen [:order/received-pieces (:id %)]) results))
                       :num-pending (apply + (map #(listen [:order/pending-pieces (:id %)]) results))
                       :total (apply + (map #(listen [:order/total (:id %)]) results))
                       :pending-total (apply + (map #(listen [:order/pending-total (:id %)]) results))
                       :num-sold (apply + (map #(q/order:num-sold (:id %)) results))}])]]])

(defn order-payment-and-notes [id order]
  [:div.col-md
   [panel "Payment"
    [:div.row
     [:div.col-lg-5
      [form-group "Due Date"
       [datepicker {:val (:due @order)
                    :save #(dispatch [:order/change id :due %])}]]]
     [:div.col-md-6
      [form-group "Method"
       [select {:val (:payment-method @order)
                :save #(dispatch [:order/change id :payment-method %])
                :opts u/order-payment-method-opts}]]]]]
   [panel "Notes"
    [textarea
     {:val (:notes @order)
      :save #(dispatch [:order/change id :notes %])}
     {:rows 6}]]])

(defn order-page [id]
  (let [order (sub [:order id])
        received-cell
        (fn [sku]
          (when (and sku (listen [:order/has-sku (:id @order) (:id @sku)]))
            (if (= (:status @order) :finished)
              [:span (listen [:order/received-qty (:id @order) (:id @sku)])]
              [text-input {:val (sub [:order/received-qty (:id @order) (:id @sku)])
                           :save #(dispatch [:order/change-received-qty (:id @order) (:id @sku) %])}
               {:class "form-control-sm"}])))]
    (fn []
      [:div
       [navbar
        [breadcrumb
         [:a {:href (path :page/orders)} "Orders"]
         (str (:vendor @order)
              (spaces 2)
              (date (:date @order)))]
        (append-delete-button
         id [:navigate :page/orders]
         (list (when-not (listen [:order/all-received? id])
                 [navbar-button {:href no-href
                                 :on-click #(dispatch [:modal/show :print-labels (listen [:order/received-skus id])])} "Print Received Labels"])
               [navbar-button {:href no-href
                               :on-click #(dispatch [:modal/show :print-labels (listen [:order/skus id])])} "Print All Labels"]
               navbar-divider
               (let [finished? (listen [:order/finished? id])]
                 [navbar-button {:href no-href
                                 :on-click (fn []
                                             (dispatch [:order/mark-finished id (not finished?)])
                                             (dispatch [:navigate :page/orders]))}
                  (str (if finished?
                         "Un-mark"
                         "Mark")
                       " Finished")])))]
       [:div.container-fluid
        [:div.row
         [:div.col-md
          [panel "Details"
           [:div [stamp :order (:status @order)]]
           [:br]
           [property "Vendor" (:vendor @order)]
           [property "Shop" (:name (listen [:shop (:shop @order)]))]
           [property "Shipping" (currency (:shipping @order))]
           [property "# Ordered" (listen [:order/pieces id])]
           [property "# Received" (listen [:order/received-pieces id])]
           [property "# Pending" (listen [:order/pending-pieces id])]
           [property "Total" (currency (listen [:order/total id]))]
           [property "Pending Total" (currency (listen [:order/pending-total id]))]
           [:br]
           [property "# Sold" (q/order:num-sold id)]
           [:br]
           [property "Employee" [employee-link (:employee @order)]]
           [property "Placed on" (date (:date @order))]
           (when-let [expected-on (:expected-on @order)]
             [property "Expected on" (date expected-on)])
           (when-let [received-on (:received-on @order)]
             [property "Finished on" (date received-on)])]]
         [order-payment-and-notes id order]]
        [:div.row
         [:div.col
          [panel "Items"
           (when-not (listen [:order/all-received? id])
             [:div
              [:button.btn.btn-primary {:on-click #(dispatch [:order/receive-all id])}
               "I Got Everything"]
              [:br]
              [:br]])
           [stock-management-items-table
            :entity order
            :cols [{:label "Ordered" :center? true
                    :render (fn [item]
                              [matrix (:id item)
                               (fn [sku]
                                 (when sku
                                   [:a {:href no-href
                                        :on-click #(dispatch [:navigate-sku (:id @sku)])}
                                    (listen [:order/sku-qty id (:id @sku)])]))])}
                   {:label "Received" :center? true
                    :render (fn [item]
                              [matrix (:id item) received-cell])}
                   {:label "Unit Cost" :key #(listen [:order/unit-cost id (:id %)]) :center? true :render currency}
                   {:label "Unit Cost w/Shipping" :key #(listen [:order/unit-cost-with-shipping id (:id %)]) :center? true :render currency}
                   {:label "Pieces" :key #(listen [:order/item-pieces id (:id %)]) :center? true}
                   {:label "Subtotal" :key #(listen [:order/item-subtotal id (:id %)]) :center? true :render currency}]]]]]]])))

(let [skus-cell (fn [sku]
                  (when sku
                    [text-input {:val (sub [:order/sku-qty :new (:id @sku)])
                                 :save #(dispatch [:order/change-sku-qty :new (:id @sku) %])}
                     {:class "form-control-sm qty-input"}]))]
  (defn new-order-page []
    (let [order (sub [:order/new])]
      [:div
       [navbar [breadcrumb [:a {:href (path :page/orders)} "Orders"] "New"]
        (list [navbar-button {:href no-href
                              :on-click #(dispatch [:clear :type/order])} "Clear"]
              [navbar-one-time-button
               {:href no-href
                :disabled (listen [:new/invalid? :type/order])
                :on-click #(dispatch [:order/place-order])}
               "Place Order"])]
       [:div.container-fluid
        [:div.row
         [:div.col-md
          [panel "Details"
           [:div.row
            [:div.col-lg-6
             [form-group "Vendor"
              [text-input {:val (:vendor @order)
                           :save #(dispatch [:order/change :new :vendor %])}]]]
            [:div.col-lg-6
             [form-group "Shop"
              [select {:val (:shop @order)
                       :save #(dispatch [:order/change :new :shop %])
                       :opts (listen [:select/shops])}]]]]
           [:div.row
            [:div.col-lg-5
             [form-group "Expected on"
              [datepicker {:val (:expected-on @order)
                           :save #(dispatch [:order/change :new :expected-on %])}]]]]
           [:div.row
            [:div.col-md-6.col-lg-4
             [form-group [:span "Shipping" (spaces 2) [help {:text "Shipping cost will be split across all items in the order."}]]
              [money-input {:val (:shipping @order)
                            :save #(dispatch [:order/change :new :shipping %])}]]]]
           [:strong "Pieces: " (listen [:order/pieces :new])]
           [:br]
           [:strong "Total: " (currency (listen [:order/total :new]))]]]
         [order-payment-and-notes :new order]]
        [:div.row
         [:div.col
          [panel "Items"
           [add-item-buttons [:order/add :new] [:order/add-sku :new]]
           [:br]
           [:br]
           [stock-management-items-table
            :entity order
            :cols [{:label "Skus" :center? true
                    :render (fn [item]
                              [matrix (:id item) skus-cell])} ; Can't use an anonymous inline cell fn: the matrix is redrawn when the page is redrawn and the inputs lose focus while you're typing.
                   {:label "Unit Cost" :key #(listen [:order/unit-cost :new (:id %)]) :span 2 :center? true
                    :render (fn [_ item]
                              [money-input {:val (sub [:order/unit-cost :new (:id item)])
                                            :save #(dispatch [:order/change-unit-cost :new (:id item) %])}])}
                   {:label "Unit Cost w/Shipping" :key #(listen [:order/unit-cost-with-shipping :new (:id %)]) :center? true :render currency}
                   {:label "Pieces" :key #(listen [:order/item-pieces :new (:id %)]) :center? true}
                   {:label "Subtotal" :key #(listen [:order/item-subtotal :new (:id %)]) :center? true :render currency}
                   {:center? true :render (fn [item]
                                            [:a {:href no-href
                                                 :on-click #(dispatch [:order/remove :new (:id item)])}
                                             [:i.fas.fa-times]])}]]]]]]])))

(defn transfers-page []
  [:div
   [navbar [breadcrumb "Inventory" "Transfers"]
    [navbar-new-button {:href (path :page/new-transfer)}
     "New Transfer"]]
   [lamina
    [table
     :source [:transfers]
     :filters (fn [tid]
                [:div.row
                 [:div.col-md-4.col-lg-3
                  [form-group "Sent"
                   [datepicker-range (filter-bindings tid q/date-within?)]]]
                 [:div.col-md-4.col-lg-3
                  [form-group "Received"
                   [datepicker-range (filter-bindings tid q/received-on-within?)]]]])
     :extra (fn [tid]
              [:div.row
               [:div.col-md-4.col-lg-3
                [form-group "From"
                 [select (merge (filter-bindings tid :from :wrap? true)
                                {:opts (listen [:select/shops :all? true])})]]]
               [:div.col-md-4.col-lg-3
                [form-group "To"
                 [select (merge (filter-bindings tid :to :wrap? true)
                                {:opts (listen [:select/shops :all? true])})]]]
               [:div.col-md-4.col-lg-3
                [form-group "Employee"
                 [select (merge (filter-bindings tid :employee :wrap? true)
                                {:opts (listen [:select/employees :all? true])})]]]])
     :columns [[:label "Status" :key :status :center? true :render (fn [status]
                                                                     [stamp :transfer status])]
               [:label "From" :key #(:name (listen [:shop (:from %)])) :center? true]
               [:label "To" :key #(:name (listen [:shop (:to %)])) :center? true]
               [:label "# Sent" :key #(listen [:transfer/pieces (:id %)]) :center? true]
               [:label "# Received" :key #(listen [:transfer/received-pieces (:id %)]) :center? true]
               [:label "Value" :key #(listen [:transfer/value (:id %)]) :render currency]
               :employee
               (merge (:date table-cols) {:label "Sent"})
               (merge (:date table-cols) {:label "Received" :key :received-on})]
     :row-route :page/transfer
     :bottom-child (fn [results]
                     [table-summary
                      [{:label "# Sent" :key :num-sent}
                       {:label "# Received" :key :num-received}
                       {:label "Value" :key :value :render currency}]
                      {:num-sent (apply + (map #(listen [:transfer/pieces (:id %)]) results))
                       :num-received (apply + (map #(listen [:transfer/received-pieces (:id %)]) results))
                       :value (apply + (map #(listen [:transfer/value (:id %)]) results))}])]]])

(defn transfer-page [id]
  (let [transfer (sub [:transfer id])
        receive-cell
        (fn [sku]
          (when (and sku (listen [:transfer/has-sku (:id @transfer) (:id @sku)]))
            (if (= (:status @transfer) :received)
              [:span (listen [:transfer/received-qty (:id @transfer) (:id @sku)])]
              [text-input {:val (sub [:transfer/received-qty (:id @transfer) (:id @sku)])
                           :save #(dispatch [:transfer/change-received-qty (:id @transfer) (:id @sku) %])}
               {:class "form-control-sm qty-input"}])))]
    (fn []
      [:div
       [navbar [breadcrumb [:a {:href (path :page/transfers)} "Transfers"] (date (:date @transfer))]
        (append-delete-button
         id [:navigate :page/transfers]
         (list (let [finished? (listen [:transfer/finished? id])]
                 [navbar-button {:href no-href
                                 :on-click (fn []
                                             (dispatch [:transfer/mark-finished id (not finished?)])
                                             (dispatch [:navigate :page/transfers]))}
                  (str (if finished?
                         "Un-mark"
                         "Mark")
                       " Received")])))]
       [:div.container-fluid
        [:div.row
         [:div.col-md
          [panel "Details"
           [:div [stamp :transfer (:status @transfer)]]
           [:br]
           [property "From" (:name (listen [:shop (:from @transfer)]))]
           [property "To" (:name (listen [:shop (:to @transfer)]))]
           [property "# Sent" (listen [:transfer/pieces id])]
           [property "# Received" (listen [:transfer/received-pieces id])]
           [property "Value" (currency (listen [:transfer/value id]))]
           [:br]
           [property "Employee" [employee-link (:employee @transfer)]]
           [property "Sent on" (date (:date @transfer))]
           (when (= :received (:status @transfer))
             [property "Received on" (date (:received-on @transfer))])]]
         [:div.col-md
          [panel "Notes"
           (:notes @transfer)]]]
        [:div.row
         [:div.col
          [panel "Items"
           (when-not (listen [:transfer/all-received? id])
             [:div
              [:button.btn.btn-primary {:on-click #(dispatch [:transfer/receive-all id])}
               "I Got Everything"]
              [:br]
              [:br]])
           [stock-management-items-table
            :entity transfer
            :cols [{:label "Sent" :center? true
                    :render (fn [item]
                              [matrix (:id item)
                               (fn [sku]
                                 (when sku
                                   [:a {:href no-href
                                        :on-click #(dispatch [:navigate-sku (:id @sku)])}
                                    (listen [:transfer/sku-qty id (:id @sku)])]))])}
                   {:label "Received" :center? true
                    :render (fn [item]
                              [matrix (:id item) receive-cell])}]]]]]]]))) ; Can't use an anonymous inline cell fn: the matrix is redrawn when the page is redrawn and the inputs lose focus while you're typing.

(let [send-cell
      (fn [sku]
        (when sku
          [text-input {:val (sub [:transfer/sku-qty :new (:id @sku)])
                       :save #(dispatch [:transfer/change-sku-qty :new (:id @sku) %])}
           {:class "form-control-sm qty-input"}]))]
  (defn new-transfer-page []
    (let [transfer (sub [:transfer/new])]
      [:div
       [navbar [breadcrumb [:a {:href (path :page/transfers)} "Transfers"] "New"]
        (list [navbar-button {:href no-href
                              :on-click #(dispatch [:clear :type/transfer])} "Clear"]
              [navbar-one-time-button {:href no-href
                                       :disabled (listen [:new/invalid? :type/transfer])
                                       :on-click #(dispatch [:transfer/initiate])}
               "Initiate Transfer"])]
       [:div.container-fluid
        [:div.row
         [:div.col-md
          [panel "Details"
           [:div.row
            [:div.col-lg-6
             [form-group "From"
              [select {:val (:from @transfer)
                       :save #(dispatch [:transfer/change :new :from %])
                       :opts (listen [:select/shops])}]]
             [form-group "To"
              [select {:val (:to @transfer)
                       :save #(dispatch [:transfer/change :new :to %])
                       :opts (listen [:select/shops])}]]]]]]
         [:div.col-md
          [panel "Notes"
           [textarea
            {:val (:notes @transfer)
             :save #(dispatch [:transfer/change :new :notes %])}
            {:rows 6}]]]]
        [:div.row
         [:div.col
          [panel "Items"
           [plus-button "Add Item"
            {:on-click (fn []
                         (dispatch [:modal/show
                            :item-search
                            (fn [item]
                              (dispatch [:transfer/add :new (:id item)])
                              (dispatch [:modal/close]))
                            (fn [sku]
                              (dispatch [:transfer/add-sku :new (:id sku)])
                              (dispatch [:modal/close]))]))}]
           [:br]
           [:br]
           [stock-management-items-table
            :entity transfer
            :cols [{:label "Available" :center? true
                    :render (fn [item]
                              [matrix (:id item)
                               (fn [sku]
                                 (when (and sku (:from @transfer))
                                   [:a {:href no-href
                                        :on-click #(dispatch [:navigate-sku (:id @sku)])}
                                    [:strong.text-stock (listen [:sku/stock (:id @sku) (:from @transfer)])]]))])}
                   {:label "Send" :center? true
                    :render (fn [item]
                              [matrix (:id item) send-cell])} ; Can't use an anonymous inline cell fn: the matrix is redrawn when the page is redrawn and the inputs lose focus while you're typing.
                   {:center? true
                    :render (fn [item]
                              [:a {:href no-href
                                   :on-click #(dispatch [:transfer/remove :new (:id item)])}
                               [:i.fas.fa-times]])}]]]]]]])))

(defn counts-page []
  [:div
   [navbar [breadcrumb "Inventory" "Inventory Counts"]
    [navbar-new-button {:href (path :page/new-count)}
     "New Inventory Count"]]
   [lamina
    [table
     :source [:counts]
     :search [:name]
     :extra (fn [tid]
              [:div
               [:div.row
                [:div.col-md-4.col-lg-3
                 [form-group "Reconciled"
                  [datepicker-range (filter-bindings tid q/date-within?)]]]
                [:div.col-md-4.col-lg-3
                 [form-group "Shop"
                  [select (merge (filter-bindings tid :shop :wrap? true)
                                 {:opts (listen [:select/shops :all? true])})]]]
                [:div.col-md-4.col-lg-3
                 [form-group "Category"
                  [select (merge (filter-bindings tid :category :wrap? true)
                                 {:opts (listen [:select/categories :all? true])})]]]]
               [:div.row
                [:div.col-md-4.col-lg-3
                 [form-group "Manufacturer"
                  [select (merge (filter-bindings tid :manufacturer :wrap? true)
                                 {:opts (listen [:select/manufacturers :all? true])})]]]
                [:div.col-md-4.col-lg-3
                 [form-group "Employee"
                  [select (merge (filter-bindings tid :employee :wrap? true)
                                 {:opts (listen [:select/employees :all? true])})]]]]])
     :columns [:name
               :shop
               :category
               :manufacturer
               {:label "Change" :key #(listen [:count/shrinkage-num (:id %)]) :center? true :render plus-minus}
               :employee
               (merge (:date table-cols) {:label "Reconciled"})]
     :row-route :page/count]]])

(defn counted-cell [class id sku]
  (when sku
    [text-input {:val (sub [:count/sku-qty id (:id @sku)])
                 :save #(dispatch [:count/change-sku-qty id (:id @sku) %])}
     {:class (str "form-control-sm qty-input " class)}]))

(defn count-summary-table [id]
  [table
   :id (str "count-summary-" id)
   :source [:count/against-items id]
   :sort [:name :asc]
   :search [:name]
   :columns (concat
             item-name-and-picture-cols
             [{:label [:span [:span.text-info "Expected"] " | Counted | Difference " [:span.text-success "+"] "/" [:span.text-danger "-"]]
               :center? true
               :render (fn [item]
                         [matrix (:id item)
                          (fn [sku]
                            (when (and sku (listen [:count/sku-in-count? id (:id @sku)]))
                              [:span
                               (spaces 1)
                               [:a {:href no-href
                                    :on-click #(dispatch [:navigate-sku (:id @sku)])}
                                [:strong.text-info (listen [:count/expected-qty id (:id @sku)])]]
                               (spaces 4)
                               (if (= id :new)
                                 [counted-cell "d-inline ml-2 mr-1" id sku]
                                 (or (listen [:count/sku-qty id (:id @sku)]) 0))
                               (spaces 4)
                               (let [n (listen [:count/difference id (:id @sku)])]
                                 (plus-minus n))
                               (spaces 1)]))])}])])

(defn count-totals [id]
  [:div
   [property "Stock change" (plus-minus (listen [:count/shrinkage-num id]))]
   [property "Amount change" (plus-minus (listen [:count/shrinkage-amount id]) currency)]])

(defn count-page [id]
  (let [count (sub [:count id])]
    [:div
     [navbar [breadcrumb [:a {:href (path :page/counts)} "Inventory Counts"] (:name @count)]
      (append-delete-button
       id [:navigate :page/counts])]
     [:div.container-fluid
      [:div.row
       [:div.col-md-6
        [panel "Details"
         [property "Name" (:name @count)]
         [property "Shop" (:name (listen [:shop (:shop @count)]))]
         (when-let [category (:category @count)]
           [property "Category" (listen [:category/path category])])
         (when-let [manufacturer (:manufacturer @count)]
           [property "Manufacturer" (:name (listen [:manufacturer manufacturer]))])
         [:br]
         [property "Employee" [employee-link (:employee @count)]]
         [property "Reconciled on" (date (:date @count))]]]
       [:div.col-md-6
        [panel "Totals"
         [count-totals id]]]]
      [:div.row
       [:div.col
        [panel "Summary"
         [count-summary-table id]]]]]]))

(let [counted-cell-new (partial counted-cell nil :new)]
  (defn new-count-page []
    (let [count (sub [:count/new])]
      [:div
       [navbar [breadcrumb [:a {:href (path :page/counts)} "Inventory Counts"] "New"]
        (list [navbar-button {:href no-href
                              :on-click #(dispatch [:clear :type/count])} "Clear"]
              [navbar-one-time-button {:href no-href
                                       :disabled (listen [:new/invalid? :type/count])
                                       :on-click #(dispatch [:count/reconcile])}
               "Reconcile"])]
       [:div.container-fluid
        [:div.row
         [:div.col-md-6.col-lg-4
          [panel "Details"
           [form-group "Name"
            [text-input {:val (:name @count)
                         :save #(dispatch [:count/change :new :name %])}]]
           [form-group "Shop"
            [select {:val (:shop @count)
                     :save #(dispatch [:count/set-shop :new %])
                     :opts (listen [:select/shops])}]]]]
         [:div.col-md-6.col-lg-4
          [panel [:span "Counting Against" (spaces 2) [help {:text "Do not select a category/manufacturer if you only want to adjust the stock of skus you specifically count."}]]
           [form-group "Category"
            [select {:val (:category @count)
                     :save #(dispatch [:count/set-category :new %])
                     :opts (listen [:select/categories :none? true])}]]
           [form-group "Manufacturer"
            [select {:val (:manufacturer @count)
                     :save #(dispatch [:count/set-manufacturer :new %])
                     :opts (listen [:select/manufacturers :none? true])}]]]]]
        [:div.row
         [:div.col
          [panel "Items"
           [pill-tabs
            :counted
            {:name :counted
             :tab-body "Counted"
             :tab-badge-body (sub [:count/num-counted :new])
             :pane [:div
                    [plus-button "Add Item"
                     {:on-click (fn []
                                  (dispatch [:modal/show
                                             :item-search
                                             (fn [item]
                                               (dispatch [:count/add :new (:id item)])
                                               (dispatch [:modal/close]))
                                             (fn [sku]
                                               (dispatch [:count/add-sku :new (:id sku)])
                                               (dispatch [:modal/close]))]))}]
                    [:br]
                    [:br]
                    [stock-management-items-table
                     :entity count
                     :cols [{:label "Counted" :center? true
                             :render (fn [item]
                                       [matrix (:id item) counted-cell-new])}
                            {:center? true
                             :render (fn [item]
                                       [:a {:href no-href
                                            :on-click #(dispatch [:count/remove :new (:id item)])}
                                        [:i.fas.fa-times]])}]]]}
            {:name :summary
             :tab-body "Summary"
             :tab-badge-body (sub [:count/num-expected :new])
             :pane [:div
                    [count-totals :new]
                    [:br]
                    [count-summary-table :new]]}]]]]]])))

(defn customers-page []
  [:div
   [navbar [breadcrumb "People" "Customers"]
    [navbar-new-button {:href no-href
                        :on-click #(dispatch [:modal/show :new-customer])}
     "New Customer"]]
   [lamina
    [customers-table #(dispatch [:navigate :page/customer %])]]])

(defn customer-nav [id]
  [navbar
   [breadcrumb
    [:a {:href (path :page/customers)} "People"]
    [:span
     (listen [:customer/name id]) (spaces 2) [tier-badge id {:style {:margin-top "-4px"}}]
     (when-let [credit-account-id (listen [:customer/credit-account id])]
       (let [balance (listen [:credit-account/balance credit-account-id])]
         (when (neg? balance)
           [:span
            (spaces 2)
            [:a {:href (path :page/credit-account id)}
             [:span.badge.badge-pill.bg-orange.badge-little "Owes " (currency (- balance))]]])))]]
   (list [navbar-link {:href (path :page/customer id)} "Info"]
         [navbar-link {:href (path :page/customer-sales id)} "Sales"]
         [navbar-link {:href (path :page/credit-account id)} "Credit Account"])
   nil])

(defn customer-page [id]
  (let [customer (listen [:customer id])]
    [:div
     [customer-nav id]
     [:div.container-fluid
      [:div.row
       [:div.col-md-6.col-lg-4
        [panel "Details"
         [property "Created on" (date (:date customer))]
         (when-let [employee (:employee customer)]
           [property "Created by" [employee-link employee]])
         [form-group "Type" [select {:val (:type customer)
                                     :save #(dispatch [:customer/change id :type %])
                                     :opts (listen [:select/customer-types :none? true])}]]
         [form-group [:span "Assigned Tier" (spaces 2) [help {:text "Manually assigned tier which will be used unless the frequency and quantity of the customer's purchases qualify him/her for a higher one."}]]
          [select {:val (:assigned-tier customer)
                   :save #(dispatch [:customer/change id :assigned-tier %])
                   :opts u/customer-tier-opts}]]]]]
      [:div.row
       [:div.col-md-6.col-lg-4
        [panel "Biographical"
         [fields id customer :type/customer
          [["First Name" :first-name]
           ["Last Name" :last-name]
           ["Title" :title]
           ["Company" :company]
           ["Birthday" :birthday]]]]]
       [:div.col-md-6.col-lg-4
        [address-fields id customer :type/customer]]
       [:div.col-md-6.col-lg-4 
        [phone-fields id customer :type/customer]
        [other-contact-fields id customer :type/customer]]]
      [:div.row
       [:div.col
        [panel "Notes"
         [textarea
          {:val (:notes customer)
           :save #(dispatch [:customer/change id :notes %])}
          {:rows 6}]]]]]]))

(defn customer-sales-page [id]
  [:div
   [customer-nav id]
   [:div.container-fluid
    [panel "Sales"
     [sales-table
      :source [:customer/sales id]]]]])

(defn credit-account-page [customer-id]
  [:div
   [customer-nav customer-id]
   [:div.container-fluid
    (if-let [credit-account-id (listen [:customer/credit-account customer-id])]
      (let [credit-account (listen [:credit-account credit-account-id])]
        [:div
         [:div.row
          [:div.col-md-6
           [panel "Details"
            [property "Customer" (let [customer-id (:customer credit-account)]
                                   [:a {:href (path :page/customer customer-id)}
                                    (listen [:customer/name customer-id])])]
            [property "Created on" (date (:date credit-account))]
            (when-let [employee (:employee credit-account)]
              [property "Created by" [employee-link employee]])
            [property "Balance" (currency (listen [:credit-account/balance credit-account-id]))]
            [property "Available" (currency (listen [:credit-account/available credit-account-id]))]
            [:div.row
             [:div.col-md-6.col-lg-4
              [form-group "Credit Limit" [money-input
                                          {:val (:limit credit-account)
                                           :save #(dispatch [:credit-account/change credit-account-id :limit %])}
                                          {:disabled (not (listen [:auth? :create-or-change-credit-account]))}]]]]]]]
         [:div.row
          [:div.col
           [panel "Activity"
            [account-activity-table [:credit-account/activity credit-account-id]]]]]])
      [:div
       "This customer does not yet have a credit account."
       [:br]
       [:div.tiny-br]
       [one-time-button
        {:class "btn-primary"
         :disabled (not (listen [:auth? :create-or-change-credit-account]))
         :on-click #(dispatch [:credit-account/create customer-id])}
        [:span [:i.fas.fa-plus] (spaces 2) "Create Account"]]])]])

(defn customer-types-page []
  [:div
   [navbar [breadcrumb "People" "Customer Types"]
    [navbar-new-button {:href no-href
                        :on-click #(dispatch [:modal/show :edit-customer-type :new])}
     "New Customer Type"]]
   [lamina
    [table
     :source [:customer-types]
     :search [:name]
     :columns [:name
               [:label "Discount" :key :discount :center? true :render (fn [discount]
                                                                         (when discount
                                                                           (str discount "%")))]
               :date]
     :row-on-click #(dispatch [:modal/show :edit-customer-type (:id %)])]]])

(defn credit-accounts-page []
  [:div
   [navbar [breadcrumb "People" "Credit Accounts"]]
   [lamina
    [table
     :source [:credit-accounts]
     :search [#(m/customer:name (fetch [:customer (:customer %)]))]
     :extra (fn [tid]
              [:div.row
               [:div.col-md-4.col-lg-3
                [form-group "Created"
                 [datepicker-range (filter-bindings tid q/date-within?)]]]])
     :columns [:customer
               {:label "Balance" :key #(q/credit-account:balance (:id %)) :center? true :render currency}
               (merge (:date table-cols) {:label "Created"})]
     :row-on-click #(dispatch [:navigate :page/credit-account (:customer %)])]]])

(defn gift-cards-page []
  [:div
   [navbar [breadcrumb "People" "Gift Cards"]]
   [lamina
    [table
     :source [:gift-cards]
     :search [:code
              #(m/customer:name (fetch [:customer (:customer %)]))]
     :extra (fn [tid]
              [:div.row
               [:div.col-md-4.col-lg-3
                [form-group "Activated"
                 [datepicker-range (filter-bindings tid q/date-within?)]]]])
     :columns [[:label "Code" :key :code]
               :customer
               [:label "Balance" :key #(q/gift-card:balance (:id %)) :center? true :render currency]
               (merge (:date table-cols) {:label "Activated"})]
     :row-route :page/gift-card]]])

(defn gift-card-page [id]
  (let [gift-card (sub [:gift-card id])]
    [:div
     [navbar [breadcrumb [:a {:href (path :page/gift-cards)} "Gift Cards"] (:code @gift-card)]]
     [:div.container-fluid
      [:div.row
       [:div.col-md
        [panel "Details"
         (when-let [customer (:customer @gift-card)]
           [property "Customer" [:a {:href (path :page/customer customer)}
                                 (listen [:customer/name customer])]])
         [property "Code" (:code @gift-card)]
         [property "Activated on" (date (:date @gift-card))]
         (when-let [employee (:employee @gift-card)]
           [property "Activated by" [employee-link employee]])
         [property "Balance" (currency (listen [:gift-card/balance id]))]]]
       [:div.col-md
        [panel "Notes"
         [textarea
          {:val (:notes @gift-card)
           :save #(dispatch [:change id :type/gift-card :notes %])}
          {:rows 4}]]]]
      [:div.row
       [:div.col
        [panel "Activity"
         [account-activity-table [:gift-card/activity id]]]]]]]))

(defn employees-page []
  [:div
   [navbar "Employees"
    [navbar-new-button {:href no-href
                        :on-click #(dispatch [:modal/show :new-employee])}
     "New Employee"]]
   [lamina
    [table
     :source [:employees/humans]
     :sort [m/employee:name :asc]
     :search [m/employee:name :email]
     :class "minimally-padded-table"
     :columns [{:key :locked?
                :render (fn [locked? employee]
                          ;; This should be a loading button but I'm lazy.
                          [:button.btn.btn-secondary {:class (when locked? "btn-danger")
                                                      :on-click (fn [e]
                                                                  (dispatch [:lock-unlock (if locked? :unlock :lock) (:id employee)])
                                                                  (.stopPropagation e))}
                           [:i.fas {:class (if locked? "fa-unlock" "fa-lock")}]])}
               {:label "Name" :key m/employee:name}
               {:label "Role" :key :role :render humanize}
               {:label "Lock" :key :locked? :render yes-no}
               {:key #(listen [:clock/in? (:id %)]) :center? true
                :render (fn [in? employee]
                          [loading-button
                           {:loading? (sub [:clock/working? (:id employee)])
                            :hide-children? true}
                           {:class (if in? "btn-success" "btn-warning")
                            :on-click (fn [e]
                                        (dispatch [:clock-in-out :employee (:id employee)])
                                        (.stopPropagation e))}
                           [:i.fas.fa-clock]])}
               {:label "Started" :key #(listen [:clock/in-time (:id %)]) :center? true :render #(date % :time)}
               {:label "Hours" :key #(listen [:clock/in-time (:id %)]) :center? true :render (fn [time]
                                                                                               [refresher #(elapsed time)])}]
     :row-route :page/employee
     :row-attrs (fn [i]
                  (when (:locked? i)
                    {:class "table-danger"}))]]])

(defn employee-nav [id]
  (let [employee (listen [:employee id])]
    [navbar [breadcrumb
             [:a {:href (path :page/employees)} "Employees"]
             (m/employee:name employee)]
     (list [navbar-link {:href (path :page/employee id)} "Details"]
           [navbar-link {:href (path :page/employee-timesheets id)} "Timesheets"]
           [navbar-link {:href (path :page/employee-sales id)} "Sales"])
     (concat
      (when (= (listen [:current-page]) :page/employee-timesheets)
        (list [navbar-new-button {:href no-href
                                  :on-click #(dispatch [:modal/show :edit-timesheet :new id])}
               "Add Timesheet"]
              navbar-divider))
      (list [navbar-button {:href no-href
                            :class "btn-danger"
                            :on-click #(dispatch [:employee/archive-and-lock id])}
             "Archive & Lock"]))]))

(def role-tooltip
  (let [roles-to-permissions
        [["Admin" ["Create and edit employees."
                   "Delete purchase orders, transfers, and inventory counts."
                   "Set the cost field for items and skus."
                   "Create and modify customer credit accounts."
                   "Change the payment type of sale payment lines."
                   "Change settings (registers, shops, payment types, etc)."
                   "All Manager and Associate permissions."]]
         ["Manager" ["Create a new purchase order."
                     "Create a new inventory count."
                     "Do register adjustments: adds and payouts."
                     "View reports."
                     "All Associate permissions."]]
         ["Associate" ["Everything else."]]]]
    [:div.text-left
     (interpose
      [:br]
      (for [[role permissions] roles-to-permissions]
        [:div
         [:strong role]
         [:ul.pl-3
          (for [permission permissions]
            [:li permission])]]))]))

(defn employee-page [id]
  (let [employee (listen [:employee id])]
    [:div
     [employee-nav id]
     [:div.container-fluid
      [:div.row
       [:div.col-md-6.col-lg-4
        [panel "Details"
         [property "Email" (:email employee)]
         [employee-name-input id employee]
         [form-group [:span "Role"
                      (spaces 2)
                      [help {:text (html role-tooltip)
                             :html? true
                             :placement :right}]]
          [select {:val (:role employee)
                   :save #(dispatch [:employee/change id :role %])
                   :opts u/role-opts}]]]]]]]))

(defn employee-timesheets-page [id]
  [:div
   [employee-nav id]
   [:div.container-fluid
    [panel "Timesheets"
     [table
      :id (str "timesheets-" id)
      :source [:employee/timesheets id]
      :sort [:in :desc]
      :filters (fn [tid]
                 [:div.row
                  [:div.col-md-4.col-lg-3
                   [form-group "Clock In"
                    [datepicker-range (filter-bindings tid q/in-within?)]]]])
      :columns [{:label "Clock In" :key :in :render #(date % :full)}
                {:label "Clock Out" :key :out :render #(date % :full)}
                {:label "Day" :key :in :render #(date % :day-of-week)}
                {:label "Hours" :key m/timesheet:time :render (fn [_ i]
                                                                (elapsed [(:in i) (:out i)]))}
                {:center? true :render (fn [timesheet]
                                         [:a {:href no-href
                                              :on-click (fn [e]
                                                          (dispatch [:store/delete (:id timesheet)])
                                                          (.stopPropagation e))}
                                          [:i.fas.fa-times]])}]
      :row-on-click #(dispatch [:modal/show :edit-timesheet (:id %)])
      :bottom-child (fn [results]
                      ;; HACK: `results` isn't live so do a real query to get the total. Not really a hack, in fact better than the original way I was doing it, but inconsistent with other tables.
                      ;; Posh won't let me use a custom query function and it complains about :with I think because it's rewriting the query.
                      (let [total-hours
                            (->> @(posh.reagent/q
                                   '[:find ?out ?in ; (sum ?time) .
                                     ;; :with ?e
                                     :in $ [?timesheet-id ...]
                                     :where
                                     [?e :e/id ?timesheet-id]
                                     [(get-else $ ?e :timesheet/in 0) ?in]
                                     [(get-else $ ?e :timesheet/out 0) ?out]
                                     ;; [(- ?out ?in) ?time]
                                     ]
                                   hyperdrive.db/conn (map :id results))
                                 (map #(- (first %) (second %)))
                                 (apply +))]
                        [table-summary
                         [{:label "Total Hours" :key :total-hours :render #(elapsed [0 %])}]
                         {:total-hours total-hours #_(apply + (map m/timesheet:time results))}]))]]]])

(defn employee-sales-page [id]
  [:div
   [employee-nav id]
   [:div.container-fluid
    [panel "Sales"
     [sales-table
      :source [:employee/sales id]
      :include #{:shop-and-register}]]]])

(defn sales-page []
  [:div
   [navbar [breadcrumb "Reports" "Sales"]]
   [lamina
    [sale-lines-table]]])

;; TODO only used in sale-lines-section at present, maybe inline it later
(def sale-page-lines-cols
  [{:label "Description"
    :key (fn [line]
           (case (:line-type line)
             :sku
             (listen [:sku/name (:sku line) :full? true])
             :credit-account
             "Credit Account Deposit"
             :gift-card
             (str "Gift Card Purchase: " (:code line))))
    :render (fn [description line]
              (case (:line-type line)
                :sku
                [:a {:href no-href
                     :on-click #(dispatch [:navigate-sku (:sku line)])}
                 description]
                :credit-account
                description
                :gift-card
                description))}
   {:label "Price" :key :price :render currency}])

(defn sale-lines-section [sale]
  (let [bulk? (r/atom (:bulk? @sale))]
    (fn [sale]
      [:div
       [checkbox (atom-bindings bulk?)
        "View as grid"]
       [:div.tiny-br]
       (if-not @bulk?
         [table
          :source (:lines @sale)
          :columns (conj sale-page-lines-cols
                         (update (:employee table-cols) :render (fn [default-render]
                                                                  (if (listen [:auth? :change-completed-sale])
                                                                    (fn [_ i]
                                                                      [select {:val (:employee i)
                                                                               :save #(dispatch [:sale/change-line-employee (:id @sale) (:id i) %])
                                                                               :opts (listen [:select/employees])}])
                                                                    default-render))))
          :row-attrs (fn [i]
                       (when (sale-line-refunded? i)
                         {:class "table-danger"}))]
         [bulk-sale-items-table
          :sale sale])])))

(defn sale-page []
  (let [print-fn (atom nil)]
    (fn [id]
      (let [sale (sub [:sale id])]
        [:div
         [navbar [breadcrumb [:a {:href (path :page/sales)} "Sales"] (:code @sale)]
          (list [navbar-button {:href no-href
                                :on-click #(@print-fn)} "Print Receipt"]
                [navbar-button {:href no-href
                                :on-click #(dispatch [:show-email-receipt-modal id])} "Email Receipt"]
                [navbar-button {:href no-href
                                :on-click #(dispatch-sync [:open-tab (print/sale @sale :gift-receipt? true)])} "Gift Receipt"]
                navbar-divider
                (if (:layaway? @sale)
                  [navbar-button {:href no-href
                                  :class "btn-warning"
                                  :on-click #(dispatch [:modal/show :payment id])}
                   "Layaway Payment"]
                  [navbar-button {:href no-href
                                  :class "btn-warning"
                                  :on-click #(dispatch [:start-refund id])}
                   "Refund Sale"]))]
         [:div.container-fluid
          [:div.row
           [:div.col-md
            [panel "Details"
             [property "Source" (humanize (:sale-type @sale))]
             (when (:layaway? @sale)
               [:div
                [:div
                 [stamp :sale :layaway?]]
                [:br]])
             [property "Code" (:code @sale)]
             (if (listen [:auth? :change-completed-sale])
               [:div.row
                [:div.col-md
                 [form-group "Shop"
                  [select {:val (:shop (listen [:register (:register @sale)]))
                           :save #(dispatch [:sale/change id :register (:id (first (listen [:shop/registers %])))])
                           :opts (listen [:select/shops])}]]]
                [:div.col-md
                 [form-group "Register"
                  [select {:val (:register @sale)
                           :save #(dispatch [:sale/change id :register %])
                           :opts (listen [:select/registers :shop (:shop (listen [:register (:register @sale)]))])}]]]]
               [:div
                [property "Shop" (:name (listen [:shop (:shop (listen [:register (:register @sale)]))]))]
                [property "Register" (:name (listen [:register (:register @sale)]))]
                [:br]])
             (when (:discount @sale)
               [property "Discount" [:span (:discount @sale) "%"]])
             (if (listen [:auth? :change-completed-sale])
               [:div.row
                [:div.col-md-6
                 [form-group "Employee"
                  [select {:val (:employee @sale)
                           :save #(dispatch [:sale/change id :employee %])
                           :opts (listen [:select/employees])}]]]]
               [property "Employee" [employee-link (:employee @sale)]])
             [property "Created on" (date (:date @sale) :full)]
             (when-let [shopify-order-id (:shopify-order-id @sale)] ; TODO or when (= (:sale-type @sale) :sale.type/ecom) but explicit types are probably going away
               [:div
                [:br]
                (when-let [store (listen [:shopify-store])]
                  [property "eCommerce Order"
                   [:a {:href (str "https://" store ".myshopify.com/admin/orders/" shopify-order-id)
                        :target "_blank"}
                    shopify-order-id]])
                [property "eCommerce Status" [stamp :ecom-status (:ecom-status @sale)]]])
             [:br]
             (if-let [customer (:customer @sale)]
               [property "Customer" [customer-with-remove-button customer [:sale/change id :customer nil]]]
               [add-customer-buttons [:sale/change id :customer]])]]
           [:div.col-md
            [panel "Totals"
             [property "Total" (currency (listen [:sale/total id]))]
             [property "Balance" (currency (listen [:sale/balance id]))]
             [:br]
             [property "Subtotal" (currency (listen [:sale/subtotal id]))]
             [property "Shipping" (currency (:shipping-cost @sale))]
             [property "Discount" (currency (listen [:sale/discount-amount id]))]
             [property "Tax" (currency (listen [:sale/tax id]))]]]]
          [:div.row
           [:div.col
            [panel "Lines"
             [sale-lines-section sale]]]]
          [:div.row
           [:div.col
            [panel "Payments"
             [table
              :source (map (fn [payment]
                             (u/kmap #(keyword (name %)) payment))
                           (:transactions @sale))
              :columns [{:label "Source" :key :line-type
                         :render (fn [line-type {:keys [id source]}]
                                   (case line-type
                                     :type/payment-type
                                     (if (listen [:auth? :change-completed-sale])
                                       [select {:val source
                                                :save #(dispatch [:sale/change-payment (:id @sale) id %])
                                                :opts (listen [:select/payment-types])}]
                                       (:name (listen [:payment-type source])))
                                     :type/credit-account
                                     "Credit Account"
                                     :type/gift-card
                                     (str "Gift Card: " (:code (listen [:gift-card source])))
                                     :type/exchange
                                     "Exchange"))}
                        {:label "Amount" :key :amount :render currency}
                        :employee
                        {:label "Date" :key :date :render #(date % :full)}]]]]]
          [panel "Receipt"
           [:div.row
            [:div.col-md-8.offset-md-2
             [print-preview {:doc @sale
                             :render print/sale
                             :print-fn print-fn}]]]]]]))))

(defn grouped-report [& {:keys [table-id-base options name columns source-sub-id summary-sub-id filters breakdown]}]
  (let [by (sub [:grouped-report/by table-id-base])]
    [:div
     (into
      [:div]
      (for [opt options]
        [radio {:val by
                :save #(dispatch [:grouped-report/set-by table-id-base %])
                :name "by"
                :option opt}
         (humanize opt)]))
     (when @by
       (let [table-id (str table-id-base @by)
             name-fn (fn [group]
                       (if-let [grouping (:grouping group)]
                         (name @by grouping)
                         "--none--"))
             columns (if (fn? columns)
                       (columns table-id)
                       columns)]
         [:div
          [:hr]
          [table
           :id table-id
           :source (fn [params]
                     [source-sub-id @by params])
           :sort [name-fn :asc]
           :search [name-fn]
           :filters filters
           :columns (cons {:label "Name" :key name-fn} columns)
           :row-on-click (when-let [breakdown-fn (and breakdown (breakdown @by))]
                           (fn [group]
                             (breakdown-fn (:grouping group) (fetch [:table/params table-id]))))
           :item-key #(or (:grouping %) ::nil-key)]
          [report-summary summary-sub-id @by table-id columns]]))]))

(defn grouped-sales-page []
  [:div
   [navbar [breadcrumb "Reports" "Grouped Sales"]]
   [lamina
    [grouped-report
     :table-id-base "grouped-sales"
     :options [:sku
               :category
               :manufacturer
               :customer
               :shop
               :employee]
     :name (fn [by grouping]
             (case by
               :sku (fetch [:sku/name grouping :full? true])
               :category (fetch [:category/path grouping])
               :manufacturer (:name (fetch [:manufacturer grouping]))
               :customer (fetch [:customer/name grouping])
               :shop (:name (fetch [:shop grouping]))
               :employee (:name (fetch [:employee grouping]))))
     :columns (fn [tid]
                (concat
                 (for [shop (when-let [rollup (listen [:table/param-value tid :rollup])]
                              (case rollup
                                :breakout (listen [:shops])
                                :pick-shops (listen [:shops (or (listen [:table/param-value tid :rollup-shops]) [])])))]
                   {:label (:name shop) :key #(get-in % [:shop-qty (:id shop)] 0)})
                 sale-lines-columns))
     :source-sub-id :reports/grouped-sales
     :summary-sub-id :reports/grouped-sales-summary
     :filters (fn [tid]
                [:div
                 [:div.row
                  [:div.col-md-4.col-lg-3
                   [form-group "Date"
                    [datepicker-range (param-bindings tid :date-range)]]]
                  [:div.col-md-4.col-lg-3
                   [form-group "Category"
                    [select (merge (param-bindings tid :category)
                                   {:opts (listen [:select/categories :all? true])})]]]
                  [:div.col-md-4.col-lg-3
                   [form-group "Manufacturer"
                    [select (merge (param-bindings tid :manufacturer)
                                   {:opts (listen [:select/manufacturers :all? true])})]]]]
                 [:div.row
                  [:div.col-md-4.col-lg-3
                   [form-group "Shop"
                    [select (merge (param-bindings tid :shop)
                                   {:opts (listen [:select/shops :all? true])})]]]
                  [:div.col-md-4.col-lg-3
                   [form-group "Employee"
                    [select (merge (param-bindings tid :employee)
                                   {:opts (listen [:select/employees :all? true])})]]]]
                 [:div.row
                  [:div.col-md-6.col-lg-4
                   (into
                    [:div]
                    (for [opt [{:text "Summarized" :option nil}
                               {:text "Breakout" :option :breakout}
                               {:text "Pick Shops" :option :pick-shops}]]
                      [radio (assoc (param-bindings tid :rollup)
                                    :name "rollup"
                                    :option (:option opt))
                       (:text opt)]))
                   (when (= :pick-shops (listen [:table/param-value tid :rollup]))
                     [:div ; Weird duplication bug when (multi)select not wrapped in a div.
                      [select (merge (param-bindings tid :rollup-shops)
                                     {:opts (listen [:select/shops])
                                      :multiple? true})]])]]])
     :breakdown (fn [by]
                  (when-not (= by :sku)
                    (fn [grouping params]
                      ;; Would be cleaner to write one event to do all of this. ; TODO maybe todo
                      (dispatch [:table/clear :table-id/sale-lines])
                      (doseq [[param-key param-value] (dissoc params :rollup :rollup-shops)]
                        (dispatch [:table/set-param :table-id/sale-lines param-key param-value]))
                      (dispatch [:table/set-param :table-id/sale-lines by grouping])
                      (dispatch [:navigate :page/sales]))))]]])

(defn grouped-assets-page []
  [:div
   [navbar [breadcrumb "Reports" "Grouped Assets"]]
   [lamina
    [grouped-report
     :table-id-base "grouped-assets"
     :options [:category
               :manufacturer
               :shop]
     :name (fn [by grouping]
             (case by
               :category (fetch [:category/path grouping])
               :manufacturer (:name (fetch [:manufacturer grouping]))
               :shop (:name (fetch [:shop grouping]))))
     :columns [{:label "Remaining" :key :remaining}
               {:label "Total Cost" :key :total-cost :render currency}
               {:label "Sale Value" :key :sale-value :render currency}]
     :source-sub-id :reports/grouped-assets
     :summary-sub-id :reports/grouped-assets-summary]]])

(defn employee-performance-page []
  (let [table-id :employee-performance
        columns [{:label "Hours Worked" :key :worked :render #(elapsed [0 %])}
                 {:label "Items Sold" :key :qty}
                 {:label "Items/Hr" :key :items-per-hour :render #(if % (.toFixed % 2) "n/a")}
                 {:label "Sales" :key :total :render currency}
                 {:label "Sales/Hr" :key :sales-per-hour :render #(if % (currency %) "n/a")}
                 {:label "Profit" :key :profit :render #(plus-minus % currency)}
                 {:label "Profit/Hr" :key :profit-per-hour :render #(if % (plus-minus % currency) "n/a")}
                 {:label "Commission" :key :commission :render currency}]]
    [:div
     [navbar [breadcrumb "Reports" "Employee Performance"]]
     [lamina
      [table
       :id table-id
       :source (fn [params]
                 [:reports/employee-performance params])
       :search [#(m/employee:name (fetch [:employee (:employee %)]))]
       :sort [#(m/employee:name (fetch [:employee (:employee %)])) :desc]
       :filters (fn [tid]
                  [:div
                   [:div.row
                    [:div.col-md-4.col-lg-3
                     [form-group "Date"
                      [datepicker-range (param-bindings tid :date-range)]]]]])
       :columns (cons (:employee table-cols) columns)
       :item-key :employee
       :bottom-child (fn [results]
                       [table-summary columns (into {} (for [key (map :key columns)]
                                                         [key (u/sum-key key results)]))])]]]))

(defn stock-history-page []
  [:div
   [navbar [breadcrumb "Reports" "Stock History"]]
   [lamina
    [table
     :id :stock-history-report
     :source (fn [params]
               [:reports/stock-history params])
     :search [#(fetch [:sku/name (:sku %) :full? true])]
     :filters (fn [tid]
                [:div.row
                 [:div.col-md-4.col-lg-3
                  [form-group "Date"
                   [datepicker-range (param-bindings tid :date-range)]]]])
     :extra (fn [tid]
              [:div.row
               [:div.col-md-4.col-lg-3
                [form-group "Event"
                 [select (merge (param-bindings tid :type)
                                {:opts u/botless-event-opts})]]]
               [:div.col-md-4.col-lg-3
                [form-group "Shop"
                 [select (merge (param-bindings tid :shop)
                                {:opts (listen [:select/shops :all? true])})]]]
               [:div.col-md-4.col-lg-3
                [form-group "Employee"
                 [select (merge (param-bindings tid :employee)
                                {:opts (listen [:select/employees :all? true])})]]]])
     :columns [{:label "Event" :key :type :center? true
                :render (fn [type]
                          [stamp :event type])}
               {:label "ID" :key :id :center? true}
               :sku
               {:label "Change" :key :adj :center? true :render plus-minus}
               :shop
               :employee
               {:label "Occurred" :key :date :center? true :render #(date % :full)}]
     ;; Could encounter a duplicate key error if a sale contains a line and refund-line with the same sku ("Encountered two children with the same key, `qqqqqqqqq2-aaaaaaaaa3-fffffffff1" with demo_data). After refactor I can use the sale line id or the transter/order/count piece id or something. Update: A refund is its own entity now ; TODO
     :item-key (fn [i]
                 (str/join "-" ((juxt :id :shop :sku) i)))
     :row-on-click (fn [i]
                     (dispatch [:navigate (case (:type i)
                                            :sale :page/sale
                                            :order :page/order
                                            :transfer :page/transfer
                                            :count :page/count) i]))]]])

(defn closing-counts-page []
  [:div
   [navbar [breadcrumb "Reports" "Closing Counts"]]
   [lamina
    [table
     :source [:reports/register-counts]
     :filters (fn [tid]
                [:div.row
                 [:div.col-md-4.col-lg-3
                  [form-group "Closed"
                   [datepicker-range (filter-bindings tid q/date-within?)]]]])
     :extra (fn [tid]
              [:div.row
               (shop-and-register-filters tid)])
     :columns [:shop*
               :register
               :employee
               [:label "Total" :key :total :render currency]
               [:label "Counted" :key :counted :render currency]
               [:label "Diff" :key :diff :render #(plus-minus % currency)]
               [:label "Cash Deposit" :key #(m/register-count:cash-deposit (listen [:register-count (:id %)])) :render #(plus-minus % currency)] ; TODO :key is hacky as balls, fix it.
               (merge (:date table-cols) {:label "Opened" :key :open-date})
               (merge (:date table-cols) {:label "Closed"})]
     :row-route :page/closing-count]]])

(defn closing-count-nav [id]
  [navbar [breadcrumb
           "Reports"
           [:a {:href (path :page/closing-counts)} "Closing Counts"]
           (date (:date (listen [:register-count id])))]
   (list
    [navbar-link {:href (path :page/closing-count id)} "Summary"]
    [navbar-link {:href (path :page/closing-count-sales id)} "Sales"]
    [navbar-link {:href (path :page/closing-count-adjustments id)} "Adjustments"])
   [navbar-button {:href no-href
                   :on-click #(dispatch-sync [:open-tab (print/closing-count (fetch [:register-count id]))])}
    "Print Receipt"]])

(defn closing-count-page [id]
  (let [reg-count (listen [:register-count id])]
    [:div
     [closing-count-nav id]
     [:div.container-fluid
      [:div.row
       [:div.col-md-5
        [panel "Details"
         [property "Shop" (:name (listen [:shop (:shop (listen [:register (:register reg-count)]))]))]
         [property "Register" (:name (listen [:register (:register reg-count)]))]]]
       [:div.col-md-7
        [panel "Opening"
         [property "Time" (date (:open-date reg-count) :full)]
         [property "Employee" [employee-link (:open-employee reg-count)]]]
        [panel "Closing"
         [property "Time" (date (:date reg-count) :full)]
         [property "Employee" [employee-link (:employee reg-count)]]
         [property "Notes" (:notes reg-count)]]]]
      [panel "Counts"
       [property "Left in Drawer" (currency (:left-in-drawer reg-count))]
       [property "Cash Deposit" (plus-minus (m/register-count:cash-deposit reg-count) currency)]
       [:br]
       [table
        :source [:reports/register-count id]
        :columns [:payment-type
                  {:label "Open" :key :open :render currency}
                  {:label "Payments" :key :payments :render currency} ;; TODO maybe change to "Sales" if those end up being the only kind of payment.
                  {:label "Adjustments" :key :adjustments :render currency}
                  {:label "Total" :key :total :render currency}
                  {:label "Counted" :key :counted :span 2 :render (fn [_ line]
                                                                    [money-input {:val (get-in reg-count [:amounts (:payment-type line)])
                                                                                  :save #(dispatch [:register-count/change-amount id (:payment-type line) %])}])}
                  {:label "Diff" :key :diff :render #(plus-minus % currency)}]
        :item-key :payment-type]]]]))

(defn closing-count-sales-page [id]
  (let [reg-count (listen [:register-count id])]
    [:div
     [closing-count-nav id]
     [:div.container-fluid
      [panel "Sales"
       [sales-table
        :source [:register-count/sales id]
        :include #{:employee}]]]]))

(defn closing-count-adjustments-page [id]
  (let [reg-count (listen [:register-count id])]
    [:div
     [closing-count-nav id]
     [:div.container-fluid
      [panel "Adjustments"
       [register-adjustments-table
        :source [:register-count/adjustments id]
        :filters (fn [tid]
                   [:div.row
                    [:div.col-md-4.col-lg-3
                     [form-group "Employee"
                      [select (merge (filter-bindings tid :employee :wrap? true)
                                     {:opts (listen [:select/employees :all? true])})]]]
                    [:div.col-md-4.col-lg-3
                     [form-group "Payment Type"
                      [select (merge (filter-bindings tid :payment-type :wrap? true)
                                     {:opts (listen [:select/payment-types :all? true])})]]]])]]]]))

(defn register-adjustments-page []
  [:div
   [navbar [breadcrumb "Reports" "Register Adjustments"]]
   [lamina
    [register-adjustments-table
     :filters (fn [tid]
                [:div.row
                 [:div.col-md-4.col-lg-3
                  [form-group "Date"
                   [datepicker-range (filter-bindings tid q/date-within?)]]]])
     :extra (fn [tid]
              [:div.row
               (shop-and-register-filters tid)
               [:div.col-md-4.col-lg-3
                [form-group "Employee"
                 [select (merge (filter-bindings tid :employee :wrap? true)
                                {:opts (listen [:select/employees :all? true])})]]]
               [:div.col-md-4.col-lg-3
                [form-group "Payment Type"
                 [select (merge (filter-bindings tid :payment-type :wrap? true)
                                {:opts (listen [:select/payment-types :all? true])})]]]])]]])

(defn shops-page []
  [:div
   [navbar [breadcrumb "Settings" "Shops"]
    [navbar-new-button {:href no-href
                        :on-click #(dispatch [:modal/show :new-shop])}
     "New Shop"]]
   [lamina
    [table
     :source [:shops]
     :sort [:name :asc] ;; TODO proper shop sorting with HQ on top
     :search [:name]
     :columns [:name]
     :row-route :page/shop]]])

(defn shop-page [id]
  (let [shop (listen [:shop id])]
    [:div
     [navbar [breadcrumb
              "Settings"
              [:a {:href (path :page/shops)} "Shops"]
              (:name shop)]
      [navbar-new-button {:href no-href
                          :on-click #(dispatch [:modal/show :edit-register :new (:id shop)])}
       "New Register"]]
     [:div.container-fluid
      [:div.row
       [:div.col-md-6.col-lg-4
        [form-group "Name" [text-input {:val (:name shop)
                                        :save #(dispatch [:shop/change id :name %])}]]]]
      [:br]
      [panel "Registers"
       [table
        :source [:shop/registers (:id shop)]
        :sort [:name :asc]
        :search [:name]
        :columns [:name]
        :row-on-click #(dispatch [:modal/show :edit-register (:id %)])]]]]))

(defn payment-types-page []
  [:div
   [navbar [breadcrumb "Settings" "Payment Types"]
    [navbar-new-button {:href no-href
                        :on-click #(dispatch [:modal/show :edit-payment-type :new])}
     "New Payment Type"]]
   [lamina
    [table
     :source [:payment-types]
     :sort [:name :asc]
     :search [:name]
     :columns [:name]
     :row-on-click #(dispatch [:modal/show :edit-payment-type (:id %)])]]])





;;; User pages

(defn chat []
  (some-> js/Notification .requestPermission) ; Notification permissions can't be requested from cross-domain iframe in Chrome and maybe other browsers. Ideally I'd figure out which ones and only do this on those browsers (currently you'll get two requests on e.g. Safari).
  (r/create-class
   {:component-did-mount
    (fn [this]
      (dispatch [:chat-login (dom/dom-node this)])) ; re-frame docs say this should ideally be done with an event/effect (also there are other js/setIntervals in the views).
    :reagent-render
    (fn []
      [:iframe.w-100.border-0 {:src (str chat-url "/home")}])})) ; Put "/home" on the end because of #2: https://stackoverflow.com/questions/49853671/rocket-chat-iframe-integration-browsers-navigation-issue/51243994#51243994

(defn messages-page []
  [:div.messages-page
   [navbar "Messages"]
   [:div.container-fluid
    [:div.row
     [:div.col
      [chat]]]]])

(defn commission-page []
  [:div
   [navbar "Commission"]
   [:div.container-fluid
    [:div.row
     [:div.col-md-5
      [panel "Leaderboard"
       "Top earners this month."
       [:br][:br]
       [table
        :source [:commission/leaderboard]
        :sort [:rank :asc]
        :columns [{:label "Rank" :key :rank :center? true :render (fn [rank]
                                                                    [:span
                                                                     (if-let [star (get {1 "text-warning" 2 "text-secondary" 3 "text-orange"} rank)]
                                                                       [:span [:i.fas.fa-star {:class star}] (spaces 2)])
                                                                     rank])}
                  :employee
                  {:label "Commission" :key :commission :center? true :render currency}]
        :item-key :employee]]]
     [:div.col-md-7
      [panel "My Commissions"
       [table
        :source [:commission/lines (:id (listen [:user]))]
        :search [#(fetch [:sku/name (:sku %) :full? true])]
        :columns [:sku
                  {:label "Percentage" :key :percentage :render #(str % "%")}
                  {:label "Commission" :key :commission :render #(plus-minus % currency)}
                  {:label "Date" :key :date :render date}]
        :row-on-click #(dispatch [:navigate :page/sale (:sale %)])]]]]]])





;;; Customer Display

(defn enter-passkey-page []
  (let [passkey (r/atom nil)]
    (fn []
      [:div.splash
       [:div.card.shadow.login-card
        [:div.card-body
         [:form {:on-submit (fn [e]
                              (dispatch [:customer-display/set-passkey @passkey])
                              (.preventDefault e))}
          [form-group
           [input-addon
            :append [:i.fas.fa-key]
            (atom-bindings passkey)
            {:placeholder "Customer Display Passkey"
             :auto-focus true}]]
          [:button.btn.btn-primary.btn-block {:type "submit"}
           "Enter Passkey"]]]]])))

(defn customer-display-sale [state]
  [:div.text-white.bg-primary.vh-100.px-0
   {:class (if (= (:phase state) :customer-display.phase/sale)
             "col-md" "col")}
   (case (:phase state)
     :customer-display.phase/sale
     [:div.vh-100.overflow-auto
      [:table.table.sale-items-table
       [:tbody
        (for [i (:items state)]
          [:tr.sale-item.font-weight-lighter.text-white.font-weight-bold.px-4.border-top-0
           [:td
            (when (:image i)
              [:img.rounded-circle {:src (thumbnail-url (:image i) 50)}])]
           [:td (:name i)]
           [:td.text-center (currency (:price i))]])]]
      [:div.sale-bottom.bg-secondary.font-weight-bold.text-center
       (currency (:total state))]]
     :customer-display.phase/thanks
     [:div.vh-100.d-flex.justify-content-center.align-items-center
      [:div.text-center
       [:h1                             ; .animated.fadeInDown
        "Thank you" (if-let [name (:customer-first-name state)]
                      (str " " name))
        "!"]
       [:br]
       [:p.font-weight-lighter.big-font "Total:" (spaces 3) (currency (:total state))]
       [:p.font-weight-lighter.big-font "Change:" (spaces 3) (currency (:change state))]
       (when-let [savings (:tier-savings state)]
         [:div
          [:br]
          [:p.font-weight-lighter.big-font "Your " (get-in state [:customer :tier-name]) " Tier status saved you " [:strong (currency savings)] " today!"]])]]
     nil)])

(defn customer-display-signup []
  (let [form (r/atom nil)]
    (fn [state]
      [:div.col-md.vh-100.d-flex.justify-content-center.align-items-center
       (if-let [customer (:customer state)]
         [:div.big-font.text-center
          [:div.font-weight-lighter "Checking out as " (:name customer)]
          [:br]
          [:img {:src (:tier-img customer)
                 :height "60px"
                 :width "60px"}]
          [:br]
          [:div (:tier-name customer) " Tier"]]
         [:div
          [:div.font-weight-lighter.big-font "Become a Krush customer!"]
          [:br]
          (into [:div]
                (for [[label field input] [["Email" :email email-input]
                                           ["First Name" :first-name]
                                           ["Last Name" :last-name]
                                           ["Phone" :phone phone-input]]]
                  [form-group label [(or input text-input)
                                     {:val (get @form field)
                                      :save #(swap! form assoc field %)}]]))
          [:div.tiny-br]
          [:div.text-center
           [:button.btn.btn-dark.btn-wide
            {:type "button"
             :on-click (fn []
                         (when @form ; Just make sure they typed something first
                           (dispatch [:customer-display/signup @form])))}
            "Sign up"]]])])))

(defn sale-and-signup-page []
  (let [state (listen [:customer-display/state])]
    [:div.container-fluid
     [:div.row.sale-and-signup-page
      [customer-display-sale state]
      (when (= (:phase state) :customer-display.phase/sale)
        [customer-display-signup state])]]))

(defn customer-display-main []
  (if-not (listen [:customer-display/passkey-set?])
    [enter-passkey-page]
    [sale-and-signup-page]))





;;; Special pages

(defn not-found-page []
  [:div.text-center
   [:br]
   [:h1 "Page not found"]
   [:br]
   [:p
    "Return to the "
    [:a {:href (path :page/home)}
     "home page"]
    "."]])

(defn unauthorized-page []
  [:div.text-center
   [:br]
   [:h1 "Unauthorized"]
   [:br]
   [:p "Your clearance isn't high enough to do this."]
   [:p
    "Go " [:a {:href no-href
               :on-click #(js/history.back)}
           "back"]
    " or return to the " [:a {:href (path :page/home)}
                          "home page"]
    "."]])

(defn screen-lock-page []
  (let [pin (r/atom nil)]
    (fn []
      [:div.splash
       [:div.card.shadow.login-card
        [:div.card-body
         [:form {:on-submit (fn [e]
                              (dispatch [:screen/unlock @pin])
                              (.preventDefault e))}
          [form-group
           [input-addon
            :append [:i.fas.fa-key]
            (atom-bindings pin)
            {:type "password"
             :placeholder "PIN"
             :auto-focus true}]]
          [loading-button
           {:loading? (sub [:unlock/working?])}
           {:class "btn-primary btn-block"
            :type "submit"
            :disabled (str/blank? @pin)}
           "Unlock"]
          [:div.mt-3.text-center
           [:a.small {:href no-href
                      :on-click #(dispatch [:logout])}
            "Logout"]]]]]])))

(defn loading-page []
  [:div.splash
   [:img {:src "/images/loading.svg"}]])





;;;; Main page

(defn sync-indicator []
  (let [synced? (listen [:comms/synced?])]
    [tooltip {:text (if synced?
                      "Your changes are saved."
                      "Saving...")
              :cmp (if synced?
                     [:i.fas.fa-fw.fa-cloud-upload-alt]
                     [:i.fas.fa-fw.fa-sync-alt.fa-spin])}]))

(defn sidebar-nav-item [text icon active? attrs collapse]
  [:li.nav-item {:class (when active? "active")}
   [:a.nav-link attrs
    [:i.fas.fa-fw {:class icon}] " " [:span text]]
   collapse])

(defn sidebar-item [text icon route]
  [sidebar-nav-item text icon
   (= (listen [:current-page]) route)
   {:href (path route)}])

(defn sidebar-item-multi [accordion-id text icon sub-items]
  (let [collapse-id (gen-id!)]
    (fn []
      [sidebar-nav-item text icon
       (some #{(listen [:current-page])} (->> sub-items flatten (filter keyword?)))
       {:href no-href
        :class "collapsed"
        :data-target (str "#" collapse-id)
        :data-toggle "collapse"}
       [:div.collapse {:id collapse-id
                       :data-parent (str "#" accordion-id)}
        (into
         [:div.bg-white.py-2.collapse-inner.rounded]
         (letfn [(sections [sub-items]
                   (mapcat
                    (fn [[header-or-item-text items-or-route]]
                      (if (sequential? items-or-route)
                        (cons [:h6.collapse-header header-or-item-text ":"]
                              (sections items-or-route))
                        [[:a.collapse-item {:href (path items-or-route)
                                            :on-click #(.collapse (jq (str "#" collapse-id)) "hide")
                                            :class (when (= (listen [:current-page]) items-or-route)
                                                     "active")}
                          header-or-item-text]]))
                    sub-items))]
           (sections sub-items)))]])))

(defn sidebar []
  (let [accordion-id (gen-id!)]
    (fn []
      [:ul.navbar-nav.bg-gradient-primary.sidebar.sidebar-dark.accordion {:id accordion-id
                                                                          :class (when (listen [:sidebar-toggled?]) "toggled")}
       [:a.sidebar-brand.d-flex.align-items-center.justify-content-center {:href (path :page/home)}
        [:img {:src "/images/rocket.svg"}]
        [:div.sidebar-brand-text.mx-3 "Hyperdrive"]]
       [:hr.sidebar-divider.my-0]
       [:li.nav-item
        (let [user (listen [:user])
              user-id (:id (listen [:user]))
              in? (listen [:clock/in? user-id])]
          [:a.nav-link.name-and-status.d-flex.align-items-center
           {:href no-href
            :on-click #(dispatch [:modal/show :time-clocks])}
           [:strong.text-white.mr-auto (m/employee:name user)]
           [:div.badge {:class (if in?
                                 "badge-clocked-in"
                                 "badge-clocked-out")}
            (spaces 1)
            (if in? "IN" "OUT")
            (spaces 1)]
           (when in?
             [:div
              (spaces 2)
              [:span.in-time.font-weight-bold
               [refresher #(elapsed (listen [:clock/in-time user-id]) :numbers)]]])])]
       [:div.sidebar-icons.px-3
        [:div
         [sync-indicator]]
        [:a {:href no-href
             :on-click #(dispatch [:modal/show :user-settings])}
         [:i.fas.fa-fw.fa-cog]]
        [:a {:href no-href
             :on-click #(dispatch [:screen/lock])}
         [:i.fas.fa-fw.fa-lock]]
        [:a {:href no-href
             :on-click #(dispatch [:logout])}
         [:i.fas.fa-fw.fa-sign-out-alt]]]
       (when (listen [:chat-available?])
         [sidebar-item "Messages" "fa-comment" :page/messages])
       [sidebar-item "Commission" "fa-money-bill-alt" :page/commission]
       ;; [:hr.sidebar-divider.my-0]
       ;; [sidebar-item "Dashboard" "fa-tachometer-alt" :page/home]
       [:hr.sidebar-divider]
       [:div.sidebar-heading "General"]
       [sidebar-item "Sales" "fa-shopping-cart" :page/sales-landing]
       [sidebar-item-multi accordion-id "Inventory" "fa-cube"
        [["Items" [["Items" :page/items]
                   ["Categories" :page/categories]
                   ["Manufacturers" :page/manufacturers]]]
         ["Stock Management" [["Orders" :page/orders]
                              ["Transfers" :page/transfers]
                              ["Inventory Counts" :page/counts]]]]]
       [sidebar-item-multi accordion-id "People" "fa-users"
        [["Customers" :page/customers]
         ["Customer Types" :page/customer-types]
         ["Credit Accounts" :page/credit-accounts]
         ["Gift Cards" :page/gift-cards]]]
       (when-let [store (listen [:shopify-store])]
         [sidebar-nav-item
          [:span "eCommerce"
           (let [num (listen [:ecom-sales/num-not-shipped])]
             (when (pos? num)
               [:span " " [:sup.badge.badge-danger num]]))]
          "fa-globe"
          false
          {:href (str "https://" store ".myshopify.com/admin/orders")
           :target "_blank"}])
       (let [auth-employees-page (listen [:auth? :page/employees])
             auth-any-reports (listen [:auth? :any-reports])
             auth-any-settings (listen [:auth? :any-settings])]
         (when (or auth-employees-page auth-any-reports auth-any-settings)
           [:div
            [:hr.sidebar-divider]
            [:div.sidebar-heading "Management"]
            (when auth-employees-page
              [sidebar-item "Employees" "fa-user-circle" :page/employees])
            (when auth-any-reports
              [sidebar-item-multi accordion-id "Reports" "fa-chart-bar"
               [["Lists" [["Sales" :page/sales]
                          ["Closing Counts" :page/closing-counts]
                          ["Adjustments" :page/register-adjustments]]]
                ["Aggregates" [["Grouped Sales" :page/grouped-sales]
                               ["Grouped Assets" :page/grouped-assets]
                               ["Employee Performance" :page/employee-performance]
                               ["Stock History" :page/stock-history]]]]])
            (when auth-any-settings
              [sidebar-item-multi accordion-id "Settings" "fa-cog"
               [["Shops" :page/shops]
                ["Payment Types" :page/payment-types]]])]))
       [:hr.sidebar-divider.d-none.d-md-block]
       [:div.text-center.d-none.d-md-inline
        [:button#sidebarToggle.rounded-circle.border-0 {:on-click #(dispatch [:toggle-sidebar-shown])}]]
       (when (listen [:installable?])
         [:div.text-center.px-3.mb-3
          [:button.btn.btn-light.btn-sm {:on-click #(dispatch-sync [:install-app])}
           [:i.fas.fa-fw.fa-download] " Install App"]])])))

(defn back-to-top-button []
  (r/create-class
   {:component-did-mount
    (fn [this]
      (let [$this (jq-node this)]
        (.on (jq js/document) "scroll"
             (throttle (fn [e]
                         (if (pos? js/window.scrollY)
                           (.fadeIn $this)
                           (.fadeOut $this)))
                       500))))
    :reagent-render
    (fn []
      [:a.scroll-to-top.rounded {:href no-href
                                 :on-click #(dispatch [:scroll-to-top])}
       [:i.fas.fa-angle-up]])}))

(defn main-page []
  (let [page (listen [:page/component])]
    (if (listen [:page/template?])
      [:div
       [:div#wrapper
        [sidebar]
        [:div#content-wrapper.d-flex.flex-column
         [:div#content
          page]]]
       [back-to-top-button]]
      page)))






(defn modal-root []
  (r/create-class
   {:component-did-mount
    (fn [this]
      (doto (jq-node this)
        (.on "hidden.bs.modal" #(dispatch [:modal/cleanup]))))
    :render
    (fn []
      [:div.modal {:tab-index -1} ; When I tried to put .fade here the .close button (with :data-dismiss "modal") and modal autoclose before hotload stopped working.
       (listen [:modal/component])])}))

(defn authenticator [cmp]
  [:> Authenticator
   {:hide [SignUp Greetings]
    :usernameAttributes "email"
    :onStateChange (fn [state]
                     (when (= state "signedIn")
                       (dispatch [:authenticate])))}
   (r/as-element cmp)])

(defn main-state []
  (when (listen [:logged-in?])
    [:div
     [modal-root]
     (cond
       (not (listen [:initialized?])) [loading-page]
       (listen [:screen/locked?]) [screen-lock-page]
       :else [main-page])]))

(defn main []
  [authenticator
   [main-state]])






(defn lookup-page [k]
  (case k
    :page/home home-page
    :page/sales-landing sales-landing-page
    :page/new-sale new-sale-page
    :page/choose-shop choose-shop-page
    :page/sale-complete sale-complete-page
    :page/register-count-complete register-count-complete-page
    :page/register-adjustment-complete register-adjustment-complete-page
    :page/items items-page
    :page/item item-page
    :page/item-history item-history-page
    :page/categories categories-page
    :page/categories-browse categories-browse-page
    :page/manufacturers manufacturers-page
    :page/orders orders-page
    :page/order order-page
    :page/new-order new-order-page
    :page/transfers transfers-page
    :page/transfer transfer-page
    :page/new-transfer new-transfer-page
    :page/counts counts-page
    :page/count count-page
    :page/new-count new-count-page
    :page/customers customers-page
    :page/customer customer-page
    :page/customer-sales customer-sales-page
    :page/credit-account credit-account-page
    :page/customer-types customer-types-page
    :page/credit-accounts credit-accounts-page
    :page/gift-cards gift-cards-page
    :page/gift-card gift-card-page
    :page/employees employees-page
    :page/employee employee-page
    :page/employee-timesheets employee-timesheets-page
    :page/employee-sales employee-sales-page
    :page/sales sales-page
    :page/sale sale-page
    :page/grouped-sales grouped-sales-page
    :page/grouped-assets grouped-assets-page
    :page/employee-performance employee-performance-page
    :page/stock-history stock-history-page
    :page/closing-counts closing-counts-page
    :page/closing-count closing-count-page
    :page/closing-count-sales closing-count-sales-page
    :page/closing-count-adjustments closing-count-adjustments-page
    :page/register-adjustments register-adjustments-page
    :page/shops shops-page
    :page/shop shop-page
    :page/payment-types payment-types-page
    :page/messages messages-page
    :page/commission commission-page
    :page/customer-display customer-display-main
    :page/not-found not-found-page
    :page/unauthorized unauthorized-page))

(defn lookup-modal [k]
  (case k
    :time-clocks time-clocks
    :user-settings user-settings
    :upcs upcs
    :print-labels print-labels
    :new-item new-item
    :edit-sku edit-sku
    :edit-category edit-category
    :edit-manufacturer edit-manufacturer
    :new-customer new-customer
    :edit-customer-type edit-customer-type
    :new-employee new-employee
    :edit-timesheet edit-timesheet
    :new-shop new-shop
    :edit-register edit-register
    :edit-payment-type edit-payment-type
    :item-search item-search
    :sku-picker sku-picker
    :customer-search customer-search
    :payment payment
    :use-gift-card-1 use-gift-card-1
    :use-gift-card-2 use-gift-card-2
    :activate-gift-card activate-gift-card
    :open-register open-register
    :close-register close-register
    :drawer-adjustment drawer-adjustment
    :email-receipt email-receipt
    :export-items export-items
    :showstopper showstopper))
