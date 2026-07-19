(ns envrc.capabilities-test
  (:require [clojure.test :refer [deftest is testing]]
            [envrc.capabilities :as caps]))

(deftest pane-contract-required-verbs
  (let [plugin {:id "tmux"
                :capability :pane
                :cli {:spawn (fn [_] nil)
                      :list  (fn [_] nil)
                      :focus (fn [_] nil)
                      ;; missing :kill and :send
                      }}]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"missing required verbs"
                          (caps/check-contract! plugin)))))

(deftest pane-contract-passes-with-all-required
  (let [plugin {:id "konsole"
                :capability :pane
                :cli {:spawn (fn [_] nil) :list (fn [_] nil) :focus (fn [_] nil)
                      :kill (fn [_] nil) :send (fn [_] nil)}}]
    (is (nil? (caps/check-contract! plugin)))))

(deftest no-capability-skips-contract
  (is (nil? (caps/check-contract! {:id "agent" :cli {}}))))

(deftest workspace-contract-required-verbs
  (let [plugin {:id "x" :capability :workspace
                :cli {:new (fn [_] nil)
                      ;; missing :rm and :go
                      }}]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"missing required verbs"
                          (caps/check-contract! plugin)))))

(deftest service-contract-required-verbs
  (let [plugin {:id "x" :capability :service
                :cli {:up (fn [_] nil) :down (fn [_] nil)
                      ;; missing :status, :logs, :attach
                      }}]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"missing required verbs"
                          (caps/check-contract! plugin)))))

(deftest service-contract-passes-with-required
  (testing ":list is optional; up + down + status + logs + attach sufficient"
    (let [plugin {:id "x" :capability :service
                  :cli {:up (fn [_] nil) :down (fn [_] nil) :status (fn [_] nil)
                        :logs (fn [_] nil) :attach (fn [_] nil)}}]
      (is (nil? (caps/check-contract! plugin))))))
