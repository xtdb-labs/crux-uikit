(ns uikit.utils-test
  (:require
   [crux.uikit.utils :as utils]
   [cljs.test :refer-macros [deftest is testing run-tests]]))

(deftest column-sort
  (testing "from unset sorting"
    (let [state (atom nil)
          output {:sort {:status :asc}}]
      (is (= output (:utils (utils/column-sort state :status))))))
  (testing "from asc same column"
    (let [state (atom {:utils {:sort {:status :asc}}})
          output {:sort {:status :desc}}]
      (is (= output (:utils (utils/column-sort state :status))))))
  (testing "from desc same column"
    (let [state (atom {:utils {:sort {:status :desc}}})
          output {:sort {:status :asc}}]
      (is (= output (:utils (utils/column-sort state :status))))))
  (testing "from asc different column"
    (let [state (atom {:utils {:sort {:status :asc}}})
          output {:sort {:date :asc}}]
      (is (= output (:utils (utils/column-sort state :date))))))
  (testing "from desc different column"
    (let [state (atom {:utils {:sort {:status :desc}}})
          output {:sort {:date :asc}}]
      (is (= output (:utils (utils/column-sort state :date)))))))

(deftest process-row-value
  (let [k :my-key]
    (testing "render-fn is provided"
      (let [table {:columns [{:column-key k
                              :render-fn
                              (fn [x] 3)}]}]
        (is (= 3 (utils/process-row-value table k "row-value")))))
    (testing "render-fn is not provided"
      (let [table {:columns [{:column-key k}]}]
        (is (= "row-value" (utils/process-row-value table :k "row-value")))))))

(deftest column-select-filter-option
  (testing "remove duplicates for filter options"
    (let [table {:rows [{:a 3 :k 1} {:k 1}
                        {:k 2}
                        {:k 3} {:k 3 :a 0}
                        {:k 4} {:k 4} {:k 4}]}
          output (list [:k 1] [:k 2] [:k 3] [:k 4])]
      (is (= output (utils/column-select-filter-options table :k))))))

(deftest column-select-filter-on-change
  (testing "add filter first time"
    (let [table (atom nil)
          output {:filter-columns {:k #{"alert"}}}]
      (is (= output (:utils (utils/column-select-filter-on-change table :k "alert"))))))
  (testing "add filter with others already in place"
    (let [table (atom {:utils {:filter-columns {:k #{"filter"}}}})
          output {:filter-columns {:k #{"alert" "filter"}}}]
      (is (= output (:utils (utils/column-select-filter-on-change table :k "alert"))))))
  (testing "remove filter"
    (let [table (atom {:utils {:filter-columns {:k #{"alert"}}}})
          output {:filter-columns {:k #{}}}]
      (is (= output (:utils (utils/column-select-filter-on-change table :k "alert"))))))
  (testing "remove last filter with others in place"
    (let [table (atom {:utils {:filter-columns {:k #{"alert" "alert2"}}}})
          output {:filter-columns {:k #{"alert2"}}}]
      (is (= output (:utils (utils/column-select-filter-on-change table :k "alert")))))))

(deftest block-filter-values
  (testing "Transform :filter-columns into list of vectors"
    (let [table {:utils {:filter-columns {0 "0"
                                          1 "1"
                                          2 #{}
                                          3 #{"3" "5" "6"}
                                          4 ""}}}
          output (list [0 "0"] [1 "1"] [3 "3" :select]
                       [3 "5" :select] [3 "6" :select])]
      (is (= output (utils/block-filter-values table))))))

#_(run-tests)
