(ns crux.uikit.utils
  (:require
   [clojure.string :as s]
   [reagent.core :as r]))

(defn component-hide-show
  [component & [args]]
  (let [!ref-toggle (atom nil)
        !ref-box (atom nil)
        active? (r/atom false)
        handler (fn [e]
                  (let [^js node (.-target e)]
                    (cond
                      ;; don't close box if click happens on child-box
                      (.contains @!ref-box node) nil
                      ;; to toggle box - show/hide
                      (.contains @!ref-toggle node) (swap! active? not)
                      ;; always close child-box when clicking out
                      :else (reset! active? false))))
        ref-toggle (fn [el] (reset! !ref-toggle el))
        ref-box (fn [el] (reset! !ref-box el))]
    (r/create-class
     {:component-did-mount
      (fn []
        (js/document.addEventListener "mouseup" handler))
      :component-will-unmount
      (fn []
        (js/document.removeEventListener "mouseup" handler))
      :reagent-render
      (fn [component]
        [component @active? ref-toggle ref-box args])})))

(defn process-string
  [s]
  (some-> s
          s/trim
          not-empty
          s/lower-case
          (s/replace #"\s+" " ")))

(defn reset-pagination
  [table]
  (assoc-in table [:pagination :current-page] 0))

(defn column-sort
  [table! column-key]
  (swap! table!
         #(-> %
              reset-pagination
              (update-in [:columns :sort]
                         (fn [[curr-key order-bool]]
                           (if (= curr-key column-key)
                             [curr-key (not order-bool)]
                             [column-key true]))))))

(defn column-sort-value
  [table]
  (-> table :columns :sort))

(defn column-filter-value
  [table column-key]
  (-> table :columns :filter-input column-key))

(defn column-filter-values
  [table]
  (let [filter-input (->> (-> table :columns :filter-input)
                          (mapv (fn [[k v]]
                                  [k (process-string v)]))
                          (remove (comp nil? second))
                          (into {}))
        select-input (->> (-> table :columns :filter-select)
                          (mapv (fn [[[k _] v]]
                                  [k v]))
                          (reduce (fn [coll [k v]]
                                    (update coll k (fnil conj #{}) v)) nil))]
    (merge filter-input select-input)))

(defn column-filter-on-change
  [evt table! column-key]
  (swap! table!
         #(-> %
              reset-pagination
              (assoc-in [:columns :filter-input column-key]
                        (-> evt .-target .-value)))))

(defn column-filter-reset
  [table! column-key]
  (swap! table! update-in [:columns :filter-input] dissoc column-key))

(defn column-filters?
  [table column-key]
  (get-in table [:columns :column-filters? column-key]))

(defn column-select-input?
  [table column-key]
  (= :select (-> table :columns :column-filters column-key)))

(defn column-select-filter-options
  [table column-key]
  (->> (-> table :rows :data)
       (mapv (fn [[id row]]
               [id
                (get (into {} row) column-key)]))
       ;; filtering out equal vals but still
       ;; needing an id per value
       (group-by second)
       (mapv (fn [[value [[id _]]]]
               [id value]))
       (sort-by second)))

(defn column-select-filter-on-change
  [table! value column-key id]
  (swap! table!
         (fn [table]
           (let [curr-value (get-in table [:columns :filter-select
                                           [column-key id]])]
             (if curr-value
               (-> table
                   reset-pagination
                   (update-in [:columns :filter-select] dissoc [column-key id]))
               (-> table
                   reset-pagination
                   (assoc-in [:columns :filter-select [column-key id]] value)))))))

(defn column-select-filter-value
  [table column-key id]
  (get (-> table :columns :filter-select) [column-key id] false))

(defn column-select-filter-reset
  [table! column-key]
  (swap! table! update-in [:columns :filter-select] dissoc column-key))

(defn column-filter-reset-all
  [table!]
  (swap! table!
         (fn [table]
           (-> table
               reset-pagination
               (update :columns dissoc :filter-select)
               (update :columns dissoc :filter-input)))))

(defn search-all-value
  [table]
  (-> table :head :search-all))

(defn search-all-on-change
  [evt table!]
  (swap! table!
         #(-> %
              reset-pagination
              (assoc-in [:head :search-all]
                        (-> evt .-target .-value)))))

(defn search-all-reset
  [table!]
  (swap! table!
         #(-> %
              reset-pagination
              (update :head dissoc :search-all))))


(defn block-filter-values
  [table]
  (let [columns (-> table :columns)
        remove-empty #(into {}
                            (remove (comp empty? second) %))]
    (merge
     (remove-empty (:filter-input columns))
     (remove-empty (:filter-select columns)))))

(defn column-visible?
  [table column-key]
  (not (-> table :columns :hidden column-key)))

(defn column-visibility-on-change
  [table! column-key]
  (swap! table! #(-> %
                     reset-pagination
                     (update-in [:columns :filter-select]
                                (fn [filters-select]
                                  (into {}
                                        (remove (fn [[[k _] _]]
                                                  (= column-key k))
                                                filters-select))))
                     (update-in [:columns :filter-input] dissoc column-key)
                     (update-in [:columns :hidden column-key] not))))

(defn table-columns
  [table]
  (let [columns (-> table :columns :data)
        hidden (-> table :columns :hidden)]
    (remove #(get hidden (first %)) columns)))

(defn pagination-rows-per-page-on-change
  [evt table!]
  (swap! table!
         #(-> %
              (assoc-in [:pagination :rows-per-page]
                        (js/parseInt (-> evt .-target .-value)))
              (assoc-in [:pagination :current-page] 0))))

(defn pagination-rows-per-page
  [table]
  (or (-> table :pagination :rows-per-page) 15))

(defn pagination-current-page
  [table]
  (or (-> table :pagination :current-page) 0))

(defn pagination-current-and-total-pages
  [table processed-rows]
  (let [offset (pagination-current-page table)
        rows-per-page (pagination-rows-per-page table)
        nth-rows-at-page (+ rows-per-page (* offset rows-per-page))
        nth-rows (count processed-rows)]
    (str (inc (* offset rows-per-page))
         "-"
         (if (> nth-rows-at-page nth-rows)
           nth-rows
           nth-rows-at-page)
         " of "
         nth-rows)))

(defn pagination-rows-exhausted?
  [table processed-rows]
  (let [current-page (pagination-current-page table)
        rows-per-page (pagination-rows-per-page table)
        tot-rows (count processed-rows)
        left-rows (- tot-rows (* rows-per-page
                                 current-page)
                     rows-per-page)]
    (or (zero? left-rows) (neg? left-rows))))

(defn pagination-inc-page
  [table! processed-rows]
  (when-not (pagination-rows-exhausted? @table! processed-rows)
    (swap! table! update-in [:pagination :current-page]
           (fnil inc 0))))

(defn pagination-dec-page
  [table!]
  (when (> (pagination-current-page @table!) 0)
    (swap! table! update-in [:pagination :current-page]
           dec)))

(defn live-mode?
  [table]
  (-> table :live :live-mode))

(defn live-notifications
  [table]
  (let [n (-> table :live :notifications)]
    (if (> n 99)
      "99+"
      n)))

(defn live-mode-on
  [table!]
  (swap! table! #(-> %
                     reset-pagination
                     (update :live dissoc :notifications)
                     (update-in [:live :live-mode] not)
                     ;; clear all searches, filters
                     (update :columns dissoc :hidden)
                     (update :columns dissoc :sort)
                     (update :columns dissoc :filter-select)
                     (update :columns dissoc :filter-input)
                     (update :head dissoc :search-all))))

(defn resolve-sorting
  [table rows]
  (if-let [[column-key order] (column-sort-value table)]
    (sort
     (fn [row1 row2]
       (let [find-val #(get (into {} (second %))
                            column-key)
             val1 (find-val row1)
             val2 (find-val row2)]
         ;; TODO - this can do something smarter
         ;; for example with dates or other types
         ;; maybe allow user to provide custom fns in :columns
         (if order
           (compare val2 val1)
           (compare val1 val2))))
     rows)
    ;; with no sorting return rows input
    rows))

(defn resolve-column-filtering
  [table rows]
  (if-let [column-filters (column-filter-values table)]
    (filter
     (fn [[_ row-data]]
       (let [row-data-map (into {} row-data)]
         (every?
          (fn [[k v]]
            (let [row-v (s/lower-case (get row-data-map k))]
              (if (string? v)
                (s/includes? row-v v)
                ;; to filter when we have a select tag.
                ;; the v values are in a set
                (get v row-v))))
          column-filters))) rows)
    rows))

(defn resolve-search-all
  [table rows]
  (if-let [search-value (process-string (search-all-value table))]
    (filter
     (fn [[_ row-data]]
       (let [row-data-vals (map second row-data)]
         (some
          (fn [cell-data]
            (s/includes? (s/lower-case cell-data) search-value))
          row-data-vals))) rows)
    rows))

(defn resolve-hidden-columns
  [table rows]
  (if-let [hidden-columns (not-empty (->> (-> table :columns :hidden)
                                          (filter second)
                                          (map first)
                                          (into #{})))]
    (map
     (fn [[id row-data opts]]
       [id (remove
            #(get hidden-columns (first %))
            row-data) opts])
     rows)
    rows))

(defn resolve-live-data
  [table rows]
  (if-let [live-mode (-> table :live :live-mode)]
    (filter
     (fn [[_ _ {:keys [live-data?]}]]
       live-data?)
     rows)
    rows))

(defn resolve-pagination
  [table rows]
  (let [current-page (pagination-current-page table)
        rows-per-page (pagination-rows-per-page table)]
    [rows (when (seq rows)
            (nth (partition-all rows-per-page rows)
                 current-page))]))

(defn process-rows
  [table]
  ;; all data transformation is performed here, on READ!
  ;; swap! is not allowed in this function
  (let [rows (-> table :rows :data)]
    (->> rows
         (resolve-live-data table)
         (resolve-hidden-columns table)
         (resolve-sorting table)
         (resolve-column-filtering table)
         (resolve-search-all table)
         (resolve-pagination table))))
