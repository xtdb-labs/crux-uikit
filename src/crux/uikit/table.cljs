(ns crux.uikit.table
  (:require
   [clojure.pprint :as pprint]
   [crux.uikit.utils :as utils]
   [reagent.core :as r]))

(defn pprint-str
  [x]
  (with-out-str (pprint/pprint x)))

(defn pprint-state
  [x]
  [:code
   {:style {:text-align "left"}}
   [:pre (pprint-str x)]])

(defn column-filter-select
  [table! column-key]
  [:div.column__filter
   [utils/component-hide-show
    (fn [active? ref-toggle ref-box]
      [:div.column__button-wrapper
       [:button.button.column__button
        {:ref ref-toggle}
        [:i.column__button-icon.fas.fa-filter]
        [:span "Select"]
        [:i.column__button-icon.fas.fa-chevron-down]]
       (into
        [:div.column__button-options
         {:class (when active? "column__button-options--show")
          :ref ref-box}]
        (mapv
         (fn [[id value]]
           ^{:key id}
           [:div.action__checkbox
            {:on-click #(utils/column-select-filter-on-change table! value column-key id)}
            [:input.checkbox__input
             {:type "checkbox"
              :checked (utils/column-select-filter-value @table! column-key id)
              :value value
              :on-change #()}]
            [:label.checkbox__custom
             value]])
         (utils/column-select-filter-options @table! column-key)))])]])

(defn column-filter-input
  [table! column-key]
  [:div.column__filter
   [:input.input.input--side-icons.input--no-borders
    {:value (utils/column-filter-value @table! column-key)
     :on-change #(utils/column-filter-on-change % table! column-key)}]
   [:span.input__icon.input__left-icon
    [:i.fas.fa-filter]]
   (when (not-empty (utils/column-filter-value @table! column-key))
     [:span.input__icon.input__right-icon.input__icon--clickable
      {:on-click #(utils/column-filter-reset table! column-key)}
      [:i.fas.fa-times]])])

(defn header-columns
  [table!]
  (let [columns (utils/table-columns @table!)]
    [:thead.table__head
     (into [:tr]
           (mapv
            (fn [[column-key column-name]]
              ^{:key column-key}
              [:th.table__cell.head__cell
               [:div.head__column-title
                [:span column-name]
                [:i.fas.fa-sort.column-title__sort-icon
                 ;; sort table by column inc or dec order
                 {:on-click #(utils/column-sort table! column-key)}]]
               (when (utils/column-filters? @table! column-key)
                 (if (utils/column-select-input? @table! column-key)
                   [column-filter-select table! column-key]
                   [column-filter-input table! column-key]))])
            columns))]))

(defn body-rows
  [table! rows]
  ;; TODO
  ;; - keep table! as arg to perform actions
  ;;   on row later on
  ;; - allow options in {:rows {:cloumn-key {:class ...}}}
  [:tbody.table__body
   (for [[id row {:keys [live-data?]}] rows]
     ^{:key id}
     [:tr.table__row.body__row
      {:class (when live-data? "body__row--live")}
      (for [[column-key value] row]
        ^{:key (str id column-key)}
        [:td.table__cell.body__cell value])])])

(defn search-all
  [table!]
  [:div.top__search-all
   [:input.input.input--side-icons.input--no-borders
    {:value (utils/search-all-value @table!)
     :on-change #(utils/search-all-on-change % table!)}]
   [:span.input__icon.input__left-icon
    [:i.fas.fa-search]]
   (when (not-empty (utils/search-all-value @table!))
     [:span.input__icon.input__right-icon.input__icon--clickable
      {:on-click #(utils/search-all-reset table!)}
      [:i.fas.fa-times]])])

(defn actions
  [table!]
  [:div.top__actions
   [utils/component-hide-show
    (fn [active? ref-toggle ref-box]
      [:div.action
       [:i.action__icon.fas.fa-th-large
        {:ref ref-toggle}]
       (into
        [:div.action__options
         {:class (when active? "action__options--show")
          :ref ref-box}
         [:div
          {:style {:color "black"}}
          "Show Columns"]]
        (mapv
         (fn [[column-key column-name]]
           ^{:key column-key}
           [:div.action__checkbox
            {:on-click #(utils/column-visibility-on-change table! column-key)}
            [:input.checkbox__input
             {:type "checkbox"
              :checked (utils/column-visible? @table! column-key)
              :on-change #()}]
            [:label.checkbox__custom
             column-name]])
         (-> @table! :columns :data)))])]])

(defn active-filters
  [table!]
  (let [active-filters (utils/block-filter-values @table!)]
    [:div.top__block-filters
     (when (seq active-filters)
       [:button.button--light.button.top__clear-filters
        {:on-click #(utils/column-filter-reset-all table!)}
        "RESET"])
     (for [[column-key value] active-filters]
       ^{:key column-key}
       [:button.button.button__active-filters
        {:on-click
         (if (vector? column-key)
           #(utils/column-select-filter-reset table! column-key)
           #(utils/column-filter-reset table! column-key))}
        [:span value]
        [:i.fas.fa-times-circle]])]))

(defn live-mode
  [table!]
  (let [live-mode? (utils/live-mode? @table!)
        notifications (utils/live-notifications @table!)]
    [:button.button.button__live
     {:class (if live-mode?
               "button__live--active"
               "button__live--inactive")
      :on-click #(utils/live-mode-on table!)}
     [:i.button__live-icon.fas.fa-circle
      {:class (when live-mode?
                "button__live-icon--active")}]
     [:span "Live"]
     (when (and (not live-mode?) notifications)
       [:span.live__notifications
        notifications])]))

(defn no-data-message
  [rows]
  (when (empty? rows)
    [:div.table__no-data
     [:div "Nothing to show"]]))

(defn table
  [table!]
  (let [[processed-rows paginated-rows] (utils/process-rows @table!)]
    [:div.table__wrapper
     #_[pprint-state (dissoc @table! :rows)]
     [:div.table__top
      [:div.top__first-group
       [search-all table!]
       [actions table!]]
      [active-filters table!]
      #_[live-mode table!]]
     [:div.table__main
      [no-data-message processed-rows]
      [:table.table
       [header-columns table!]
       [body-rows table! paginated-rows]]]
     [:table.table__foot
      [:tfoot
       [:tr
        [:td.foot__pagination
         [:div.select.pagination__select
          [:select
           {:value (utils/pagination-rows-per-page @table!)
            :on-change #(utils/pagination-rows-per-page-on-change % table!)}
           [:option {:value "5"} (str "5" " rows")]
           [:option {:value "15"} (str "15" " rows")]
           [:option {:value "100"} (str "100" " rows")]]]
         [:div.pagination__info (utils/pagination-current-and-total-pages @table!
                                                                          processed-rows)]
         [:div.pagination__arrow-group
          [:div.pagination__arrow-nav
           {:class (when (<= (utils/pagination-current-page @table!) 0)
                     "pagination__arrow-nav--disabled")
            :on-click #(utils/pagination-dec-page table!)}
           [:i.fas.fa-chevron-left]]
          [:div.pagination__arrow-nav
           {:class (when (utils/pagination-rows-exhausted? @table!
                                                           processed-rows)
                     "pagination__arrow-nav--disabled")
            :on-click #(utils/pagination-inc-page table!
                                                  processed-rows)}
           [:i.fas.fa-chevron-right]]]]]]]]))
