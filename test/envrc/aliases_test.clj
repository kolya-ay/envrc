(ns envrc.aliases-test
  (:require [clojure.test :refer [deftest is]]
            [envrc.aliases :as a]))

(deftest resolve-alias-to-task-keyword
  (is (= :test (a/resolve-task {:t :test} "t")))
  (is (= :test (a/resolve-task {:t :test} :t)))
  (is (nil? (a/resolve-task {:t :test} "missing"))))

(deftest aliases-cannot-shadow-core-verbs
  (is (thrown-with-msg? Exception #"cannot shadow core verb"
        (a/validate-aliases! {:status :x})))
  (is (thrown-with-msg? Exception #"cannot shadow core verb"
        (a/validate-aliases! {:config :x})))
  (is (nil? (a/validate-aliases! {:t :test})))
  (is (thrown-with-msg? Exception #"cannot shadow core verb"
        (a/validate-aliases! {:foo :test} #{:foo}))))

(deftest aliases-must-point-to-task-keywords
  (is (thrown-with-msg? Exception #"alias :t must point to a task keyword, got string"
        (a/validate-aliases! {:t "test"})))
  (is (thrown-with-msg? Exception #"alias :t must point to a task keyword, got vector"
        (a/validate-aliases! {:t ["test"]})))
  (is (thrown-with-msg? Exception #"alias :t must point to a task keyword, got map"
        (a/validate-aliases! {:t {:task :test}}))))
