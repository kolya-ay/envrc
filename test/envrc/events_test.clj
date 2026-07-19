(ns envrc.events-test
  (:require [clojure.test :refer [deftest is testing]]
            [envrc.events :as ev]))

(deftest fire-runs-subscribers-in-order
  (let [calls (atom [])]
    (ev/with-bus {:shell [(fn [_] (swap! calls conj :a))
                          (fn [_] (swap! calls conj :b))]}
      (ev/fire! {:plugins {}} :shell))
    (is (= [:a :b] @calls))))

(deftest abort-default
  (testing "subsequent subscribers do not run after one throws"
    (let [calls (atom [])]
      (is (thrown? Exception
            (ev/with-bus {:shell [(fn [_] (swap! calls conj :a))
                                  (fn [_] (throw (ex-info "boom" {})))
                                  (fn [_] (swap! calls conj :c))]}
              (ev/fire! {:plugins {}} :shell))))
      (is (= [:a] @calls)))))

(deftest tolerant-subscriber-does-not-abort
  (let [calls (atom [])
        sub-b (with-meta (fn [_] (throw (ex-info "boom" {})))
                {:tolerant true})]
    (ev/with-bus {:shell [(fn [_] (swap! calls conj :a))
                          sub-b
                          (fn [_] (swap! calls conj :c))]}
      (ev/fire! {:plugins {}} :shell))
    (is (= [:a :c] @calls))))

(deftest fire-throws-on-undeclared-event
  (let [plugins {"x" {:id "x" :events #{:declared}}}
        cfg {:plugins plugins :tasks {}}]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo
                          #"undeclared event"
                          (ev/fire! cfg :undeclared)))))

(deftest fire-allows-declared-event
  (let [plugins {"x" {:id "x" :events #{:declared}}}
        cfg {:plugins plugins :tasks {}}]
    (is (nil? (ev/fire! cfg :declared)))))

(deftest fire-allows-core-events
  (let [cfg {:plugins {} :tasks {}}]
    (is (nil? (ev/fire! cfg :shell)))
    (is (nil? (ev/fire! cfg :gen)))))

(deftest payload-passed-to-subscriber
  (let [captured (atom nil)]
    (ev/with-bus {:shell [(fn [payload] (reset! captured payload))]}
      (ev/fire! {:plugins {}} :shell {:hello "world"}))
    (is (= {:hello "world"} @captured))))

(deftest fire-without-payload-passes-nil
  (let [captured (atom :untouched)]
    (ev/with-bus {:shell [(fn [payload] (reset! captured payload))]}
      (ev/fire! {:plugins {}} :shell))
    (is (nil? @captured))))
