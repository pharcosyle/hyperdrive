(ns hyperdrive.singularity.style
  (:refer-clojure :exclude [+ - * / rem])
  (:require [clojure.string :as str]
            [garden.arithmetic :refer [+ - * /]]
            [garden.color :refer [rgb]]
            [garden.core :refer [css]]
            [garden.stylesheet :refer [at-media]]
            [garden.units :refer [rem px percent]]))


;;;; Util

(def none "none")
(def initial "initial")
(def auto "auto")
(def white "white")
(def inherit "inherit")



;;;; Bootstrap / SB Admin variables

(def font-size-base (rem 1))
(def spacer (rem 1))

(def md (px 768))
(def lg (px 992))

(def gray-200 "#eaecf4")
(def gray-500 "#b7b9cc")
(def gray-600 "#858796")
(def gray-700 "#6e707e")
(def gray-800 "#5a5c69")

(def primary "#4e73df")
(def secondary gray-600)
(def red "#e74a3b")
(def orange "#fd7e14")
(def yellow "#f6c23e")
(def green "#1cc88a")
(def cyan "#36b9cc")
(def purple "#6f42c1")
;; (def indigo "#6610f2")
;; (def teal "#20c9a6")
;; (def pink "#e83e8c")

(def body-color gray-600)
(def input-color gray-700)
(def input-border-color (rgb 209 211 226))
(def input-height "calc(1.5em + 0.75rem + 2px)")
(def border-radius (rem 0.35))
(def table-border (str "1px solid #e3e6f0"))
(def table-sm-padding (rem 0.3))



(defn font-size [n]
  (* n font-size-base))

(defn space [n]
  (* n spacer))

;;;; Variables

(def header-color gray-800)

(def link-hover {:opacity 0.8})
(def link-hover-rule [:&:hover link-hover])

(def table-header-font-size (font-size 0.875))

(def stock green)
(def clocked-in green)
(def clocked-out yellow)



(def rules

;;;; BS changes
  
  [[:.table {:border-bottom table-border}
    [:th :td {:padding-top (space 0.6)
              :padding-bottom (space 0.6)}]
    [:th {:font-size table-header-font-size
          :border-bottom-width (str "1px !important")}]
    [:th {:border-top none}]
    [:td {:vertical-align "middle"}]]
   ;; Default .table-hover color is kinda gross, let's chnage it.
   [:.table-hover
    [:tbody
     [:tr:hover {:background-color gray-200}]]]
   ;; Using e.g. :not(.table-bordered) in the .table :th definition above wasn't working (apprently it's not a well-supported form) so just do overrides.
   [:.table-bordered
    [:th {:border-top table-border}]]
   [:.table-sm
    [:th :td {:padding table-sm-padding}]]

   [:.badge {:color white}] ; Not all badges have white text in BS4, make it like BS3.
   [:.badge-light {:color body-color}]

   [:.modal-title {:color header-color}]
   [:.modal-header {:border-bottom initial}]
   [:.modal-footer {:border-top initial}]

   
   
;;;; SB Admin changes
   
   (at-media
    {:max-width lg}
    [:.topbar {:height initial}])

   [:.topbar
    [:h1
     [:a {:color inherit
          :text-decoration none}
      link-hover-rule]]
    [:a.nav-link {:color (str header-color " !important")}
     link-hover-rule]]


   
;;;; Other external CSS changes

   [:.bootstrap-tagsinput {:border-color input-border-color
                           :box-shadow initial
                           :min-height input-height}
    [:input {:color input-color}]]



;;;; Generic

   [:.tiny-br {:height (space 0.5)}] ; Prefer BS4 spacing utilities for new development to using this or regular :br for spacing.

   [:.cursor-pointer {:cursor "pointer"}]

   [:.minimally-padded-table
    [:td {:padding-top (space 0.4)
          :padding-bottom (space 0.4)}]]

   [:.code-input {:width (space 8.75)}] ; Accommodate 14 characters just to be safe.
   
   [:.badge-little {:font-size (font-size 1)
                    :vertical-align (percent 25)}]

   [:.btn-wide {:padding-right (space 2.5)
                :padding-left (space 2.5)}]

   [:.splash {:position "absolute"
              :top 0
              :bottom 0
              :left 0
              :right 0
              :display "flex"
              :justify-content "center"
              :align-items "center"}]

   [:.login-card {:width (space 22.5)}
    [:.card-body {:padding (space 1.875)}]]

   [:.text-orange {:color orange}]
   [:.bg-orange {:background-color orange}]

   [:.text-stock {:color stock}]
   [:.in-time {:color clocked-in}]
   [:.badge-stock {:background-color stock}]
   [:.badge-clocked-in {:background-color clocked-in}]
   [:.badge-clocked-out {:background-color clocked-out}]

   [:.red-stamp {:background-color red}]
   [:.orange-stamp {:background-color orange}]
   [:.yellow-stamp {:background-color yellow}]
   [:.green-stamp {:background-color green}]
   [:.blue-stamp {:background-color cyan}]
   [:.purple-stamp {:background-color purple}]


   
;;;; Components

   [:.tile {:border-left (str "0.25rem solid " yellow)
            :cursor "pointer"}
    [:&.operative {:border-left-color green}]]

   [:.matrix {:width "initial"}
    [:td {:font-size table-header-font-size}]
    [:td :th {:text-align "center"}]
    [:.row-header {:font-size table-header-font-size}]
    [:.qty-input {:width (space 2.5)}]]

   [:.hoverzoom
    [:.big-image {:display "none"
                  :position "fixed"
                  :top "50%"
                  :left "50%"
                  :transform "translate(-50%,-50%)" ; Glorious trick for centering horizontally and vertically. Update: there's proabably a better way to do this with flexbox but whatevs.
                  :max-width "75%"
                  :max-height "75%"
                  :z-index 1100 ; Over everything in bootstrap which only goes up to 1070.
                  :pointer-events "none" ; So hovering over the big image doesn't keep the hover state active and it doesn't hide.
                  :background-color "white" ; In case the image has a transparent background.
                  :border-radius border-radius
                  :border (str "2px solid " gray-500)}]
    [:&:hover
     [:.big-image {:display "block"}]]]

   [:.iframe-preview {:width "100%"
                      :height (space 40)
                      :border "none"}]

   

;;;; Pages

   [:.new-sale-page
    [:.totals-line {:display "flex"
                    :justify-content "space-between"
                    :align-items "center"
                    :margin-bottom (space 1)}]]

   [:.messages-page
    [:iframe {:height "calc(100vh - 86px - 8px)"}]] ; Hacky: full height of page minus the top nav minus a bit more to reach the bottom of the viewport (for some reason).


   
;;;; Layout
   
   [:.sidebar
    [:.sidebar-brand
     [:img {:height (space 2.5)}]]
    [:.sidebar-icons {:display "flex"
                      :justify-content "space-between"}
     [:i {:color white}
      ["&:not(:hover)" link-hover]]]
    (let [rules (list [:.sidebar-icons {:flex-direction "column"
                                        :align-items "center"}]
                      [:.name-and-status {:flex-direction "column"}])]
      (list (at-media {:max-width md} rules)
            [:&.toggled rules]))]



;;; Customer Display

   [:.sale-and-signup-page
    (let [bottom-height (space 6)]
      (list
       [:.big-font {:font-size (font-size 1.5)}]
       [:.sale-items-table {:margin-bottom bottom-height}
        [:.sale-item {:border-bottom (str "1px solid " white)
                      :height (space 5)}]]
       [:.sale-bottom {:position "fixed"
                       :width (percent 50)
                       :left 0
                       :right 0
                       :bottom 0
                       :height bottom-height
                       :line-height bottom-height
                       :font-size (font-size 2)}]))]])



(def styles (css rules))
