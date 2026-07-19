(ns envrc.api-test
  (:require [clojure.test :refer [deftest is testing]]
            [envrc.api :as api]
            [envrc.events :as events]))

(deftest data-accessors
  (testing "cfg/root/state stored in dynamic context"
    (api/with-context {:cfg {:env {:K "v"}} :root "/tmp/p" :state "/tmp/s"}
      (is (= "v" (api/env "K")))
      (is (= "/tmp/p" (api/root)))
      (is (= "/tmp/s" (api/state)))
      (is (= {:K "v"} (:env (api/cfg)))))))

(deftest plugin-of
  (testing "returns plugin id for active capability"
    (api/with-context {:cfg {} :capabilities {:pane "konsole"}}
      (is (= "konsole" (api/plugin-of :pane)))
      (is (nil? (api/plugin-of :workspace))))))

(deftest api-fire-bridges-to-events
  (let [calls (atom 0)]
    (events/with-bus {:shell [(fn [_] (swap! calls inc))]}
      (api/fire! :shell))
    (is (= 1 @calls))))
