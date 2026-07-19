(ns envrc.runner-test
  (:require [clojure.test :refer [deftest is testing]]
            [envrc.runner :as runner]))

(deftest subscribers-built-from-on-fields
  (let [cfg {:tasks {:a {:run ["echo a"] :on :shell}
                     :b {:run ["echo b"] :on [:shell :gen]}
                     :c {:run ["echo c"]}}}
        subs (runner/event-subscribers cfg)]
    (is (= 2 (count (:shell subs))) ":shell has 2 subscribers")
    (is (= 1 (count (:gen   subs))) ":gen   has 1 subscriber")))

(deftest subscriber-metadata-carries-task-name-and-tolerance
  (let [cfg {:tasks {:a {:run ["echo a"] :on :shell :tolerant true}}}
        sub (first (get (runner/event-subscribers cfg) :shell))]
    (is (= :a (:task (meta sub))))
    (is (true? (:tolerant (meta sub))))))

(deftest run-string-still-shells-out
  (testing "string :run still goes through p/shell"
    (with-redefs [babashka.process/shell (fn [opts cmd] {:cmd cmd :opts opts})]
      (let [task {:run "echo hi"}
            result (runner/run-task :echo task {} {:root "."})]
        (is (= "echo hi" (:cmd result)))))))

(deftest run-function-returning-nil-succeeds
  (is (= {:exit 0} (runner/run-task :noop {:run (fn [_] nil)} {} {:root "."}))))

(deftest run-function-returning-command-executes
  (let [captured (atom nil)]
    (with-redefs [babashka.process/process (fn [_opts & argv]
                                             (reset! captured (vec argv))
                                             {:exit 0})]
      (runner/run-task :dyn {:run (fn [_] ["echo" "hi"])} {} {:root "."})
      (is (= ["echo" "hi"] @captured)))))

(deftest event-subscriber-passes-event-to-function-run
  (let [seen (atom nil)
        cfg {:tasks {:a {:on :shell
                         :run (fn [{:keys [event]}]
                                (reset! seen event)
                                nil)}}}
        sub (first (:shell (runner/event-subscribers cfg)))]
    (sub nil)
    (is (= :shell @seen))))

(deftest event-subscriber-receives-payload
  (let [received (atom nil)
        cfg {:tasks {:capture {:on :foo :run (fn [{:keys [payload]}] (reset! received payload) nil)}}
             :plugins {}}
        subs (runner/event-subscribers cfg)
        sub  (first (get subs :foo))]
    (sub {:hello "world"})
    (is (= {:hello "world"} @received))))
