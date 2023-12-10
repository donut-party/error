(ns donut.protocol-template-test
  (:require
   [clojure.test :refer [deftest is]]
   [donut.protocol-template :as dbpt]
   [donut.ns-protocol :as nsp]))


(defprotocol ExampleProtocol
  (foo [_]))

(dbpt/def-protocol-template
  ExampleProtocol
  :test
  (foo [_] "hi"))

(deftest implements-template
  (dbpt/extend-type-with-protocol-template
   java.util.Date
   ExampleProtocol
   :test)
  (is (= "hi" (foo (java.util.Date.)))))

(deftest implements-template-with-overrides
  (dbpt/extend-type-with-protocol-template
   java.util.Date
   ExampleProtocol
   :test
   {"hi" "hello"})
  (is (= "hello" (foo (java.util.Date.)))))


(deftest implements-template-with-ns-alias
  (dbpt/extend-type-with-protocol-template
   java.util.Date
   nsp/NSProtocol
   :test)
  (is (= #{:b :a :clojure.set/foo} (nsp/foo (java.util.Date.)))))
