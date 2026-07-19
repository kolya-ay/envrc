(ns envrc.find-by-capability-test
  (:require [clojure.test :refer [deftest is testing]]
            [envrc.plugin :as p]))

(deftest find-by-capability
  (testing "returns matching entry"
    (let [dispatch {"agents" {:plugin {:id "ag" :capability :agents} :verbs {:state identity}}
                   "pane"   {:plugin {:id "ko" :capability :pane}    :verbs {:spawn identity}}}]
      (is (= "ag" (get-in (p/find-by-capability dispatch :agents) [:plugin :id])))))
  (testing "returns nil when no plugin matches"
    (is (nil? (p/find-by-capability {} :agents)))
    (is (nil? (p/find-by-capability {"pane" {:plugin {:capability :pane}}} :agents)))))
