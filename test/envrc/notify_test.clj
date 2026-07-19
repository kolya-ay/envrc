(ns envrc.notify-test
  (:require [clojure.test :refer [deftest is testing]]
            [envrc.notify :as notify]
            [babashka.process :as p]))

(defn- capture-argv [f]
  (let [calls (atom [])]
    (with-redefs [p/shell (fn [_opts & argv] (swap! calls conj (vec argv)) {:exit 0})]
      (f)
      (first @calls))))

(deftest send-basic-argv
  (let [argv (capture-argv #(notify/send! {:summary "hi" :body "there"}))]
    (is (= "notify-send" (first argv)))
    (is (some #{"--urgency=normal"}             argv))
    (is (some #{"--icon=dialog-information"}   argv))
    (is (= "hi"    (nth argv (- (count argv) 2))))
    (is (= "there" (last argv)))))

(deftest send-omits-body-when-nil
  (let [argv (capture-argv #(notify/send! {:summary "hi"}))]
    (is (= "hi" (last argv)))))

(deftest send-emits-sync-hint
  (let [argv (capture-argv #(notify/send! {:summary "x" :hint "key-1"}))]
    (is (some #{"--hint=string:x-canonical-private-synchronous:key-1"} argv))))

(deftest send-respects-urgency-and-icon
  (let [argv (capture-argv #(notify/send! {:summary "x" :urgency "low" :icon "folder"}))]
    (is (some #{"--urgency=low"}  argv))
    (is (some #{"--icon=folder"} argv))))

(deftest send-tolerates-missing-binary
  (with-redefs [p/shell (fn [& _] (throw (java.io.IOException. "ENOENT")))]
    (is (nil? (notify/send! {:summary "x"})))))
