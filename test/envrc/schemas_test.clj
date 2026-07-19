(ns envrc.schemas-test
  (:require [clojure.test :refer [deftest is]]
            [envrc.schemas :as s]))

(deftest closest-match-is-nil-safe
  (is (nil? (s/closest-match nil ["branch" "rm" "list"])))
  (is (= "branch" (s/closest-match "branchh" ["branch" "rm" "list"]))))
