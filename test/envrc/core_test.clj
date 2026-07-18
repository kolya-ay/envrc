(ns envrc.core-test
  (:require [clojure.test :refer [deftest is]]))

(deftest envrc-namespace-loads
  (is (fn? (deref (requiring-resolve 'envrc/-main)))))
