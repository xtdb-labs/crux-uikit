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
  [table-atom column-key]
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
        (map
         (fn [[id value]]
           ^{:key (str id value)}
           [:div.action__checkbox
            {:on-click #(utils/column-select-filter-on-change table-atom column-key value)}
            [:input.checkbox__input
             {:type "checkbox"
              :checked (utils/column-select-filter-value @table-atom column-key value)
              :value value
              :on-change #()}]
            [:label.checkbox__custom
             value]])
         (utils/column-select-filter-options @table-atom column-key)))])]])

(defn column-filter-input
  [table-atom column-key]
  [:div.column__filter
   [:input.input.input--side-icons.input--no-borders
    {:value (utils/column-filter-value @table-atom column-key)
     :on-change #(utils/column-filter-on-change % table-atom column-key)}]
   [:span.input__icon.input__left-icon
    [:i.fas.fa-filter]]
   (when (not-empty (utils/column-filter-value @table-atom column-key))
     [:span.input__icon.input__right-icon.input__icon--clickable
      {:on-click #(utils/column-filter-reset table-atom column-key)}
      [:i.fas.fa-times]])])

(defn header-columns
  [table-atom]
  (let [columns (utils/table-columns @table-atom)]
    [:thead.table__head
     (into [:tr]
           (map
            (fn [{:keys [column-key column-name]}]
              ^{:key column-key}
              [:th.table__cell.head__cell
               [:div.head__column-title
                {:on-click #(utils/column-sort table-atom column-key)}
                [:span column-name]
                [:i.fas.column-title__sort-icon
                 ;; sort table by column inc or dec order
                 {:class (utils/column-sort-icon @table-atom column-key)}]]
               (case (utils/column-filter-type @table-atom column-key)
                 :input [column-filter-input table-atom column-key]
                 :select [column-filter-select table-atom column-key]
                 [:div {:style
                        {:min-height "2.25em"}}])])
            columns))]))

(defn set-loading
  [table bool]
  (assoc-in table [:utils :loading?] bool))

(defn loading-table
  [{:keys [rows cols]}]
  [:table.table
   [:thead
    [:tr
     (for [col (range cols)]
       ^{:key col}
       [:th [:span]])]]
   [:tbody.table__body
    (for [row (range rows)]
      ^{:key row}
      [:tr.loading
       (for [col (range cols)]
         ^{:key col}
         [:td.td-loading-bar
          [:span]])])]])

(defn body-rows
  [table-atom rows]
  (let [columns (utils/table-columns @table-atom)]
    (if (seq rows)
      [:tbody.table__body
       (for [row rows]
         ^{:key (:id row)}
         [:tr.table__row.body__row
          (for [{:keys [column-key render-fn]
                 :or {render-fn identity}} columns]
            ^{:key (str (:id row) column-key)}
            [:td.table__cell.body__cell
             (render-fn (column-key row))])])]
      [:tbody.table__body.table__no-data
       [:tr [:td.td__no-data
             "No Match Found"]]])))

(defn filter-all
  [table-atom]
  [:div.top__filter-all
   [:input.input.input--side-icons.input--no-borders
    {:value (utils/filter-all-value @table-atom)
     :on-change #(utils/filter-all-on-change % table-atom)}]
   [:span.input__icon.input__left-icon
    [:i.fas.fa-search]]
   (when (not-empty (utils/filter-all-value @table-atom))
     [:span.input__icon.input__right-icon.input__icon--clickable
      {:on-click #(utils/filter-all-reset table-atom)}
      [:i.fas.fa-times]])])

(defn actions
  [table-atom]
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
        (map
         (fn [{:keys [column-key column-name]}]
           ^{:key column-key}
           [:div.action__checkbox
            {:on-click #(utils/column-visibility-on-change table-atom column-key)}
            [:input.checkbox__input
             {:type "checkbox"
              :checked (utils/column-visible? @table-atom column-key)
              :on-change #()}]
            [:label.checkbox__custom
             column-name]])
         (-> @table-atom :columns)))])]])

(defn active-filters
  [table-atom]
  [:div.top__block-filters
   (when-let [filters (utils/block-filter-values @table-atom)]
     [:<>
      [:button.button--light.button.top__clear-filters
       {:on-click #(utils/column-filter-reset-all table-atom)}
       "RESET"]
      (for [[column-key value select] filters]
        ^{:key (str column-key value)}
        [:button.button.button__active-filters
         {:on-click
          (if select
            #(utils/column-select-filter-reset table-atom column-key value)
            #(utils/column-filter-reset table-atom column-key))}
         [:span value]
         [:i.fas.fa-times-circle]])])])

(defn pagination
  [table-atom processed-rows]
  [:table.table__foot
   [:tfoot
    [:tr
     [:td.foot__pagination
      [:div.select.pagination__select
       [:select
        {:value (utils/pagination-rows-per-page @table-atom)
         :on-change #(utils/pagination-rows-per-page-on-change % table-atom)}
        [:option {:value "5"} (str "5" " rows")]
        [:option {:value "15"} (str "15" " rows")]
        [:option {:value "100"} (str "100" " rows")]]]
      [:div.pagination__info (utils/pagination-current-and-total-pages @table-atom
                                                                       processed-rows)]
      [:div.pagination__arrow-group
       [:div.pagination__arrow-nav
        {:class (when (<= (utils/pagination-current-page @table-atom) 0)
                  "pagination__arrow-nav--disabled")
         :on-click #(utils/pagination-dec-page table-atom)}
        [:i.fas.fa-chevron-left]]
       [:div.pagination__arrow-nav
        {:class (when (utils/pagination-rows-exhausted? @table-atom
                                                        processed-rows)
                  "pagination__arrow-nav--disabled")
         :on-click #(utils/pagination-inc-page table-atom
                                               processed-rows)}
        [:i.fas.fa-chevron-right]]]]]]])

(defn table
  [table-atom]
  (utils/table-atom-mount table-atom)
  (fn [table-atom]
    (let [[processed-rows paginated-rows] (utils/process-rows @table-atom)]
      [:div.table__wrapper
       #_[pprint-state (dissoc @table-atom :rows)]
       [:div.table__top
        [:div.top__first-group
         [filter-all table-atom]
         [actions table-atom]]
        [active-filters table-atom]]
       (if (utils/loading? @table-atom)
         [loading-table {:rows 7 :cols 4}]
         [:div.table__main
          [:table.table
           [header-columns table-atom]
           [body-rows table-atom paginated-rows]]])
       [pagination table-atom processed-rows]])))
