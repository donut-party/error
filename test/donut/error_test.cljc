(ns donut.error-test
  (:require
   [clojure.test :refer [deftest is]]
   [donut.error :as sut]))

(deftest test-validation
  (let [thrown? (atom false)]
    (try (sut/validate! int? "foo")
         (catch #?(:clj clojure.lang.ExceptionInfo
                   :cljs js/Object)
             e
           (is (= ":donut.error/schema-validation-error"
                  (ex-message e)))
           (reset! thrown? true)))
    (is @thrown?))

  (sut/validate! int? 1))
