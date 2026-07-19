(ns envrc.plugin-test
  (:require [clojure.test :refer [deftest is testing]]
            [envrc.plugin :as plugin]
            [babashka.fs :as fs]))

(def fixtures "test/fixtures/plugins")

(deftest discovery
  (testing "scans both global and project dirs"
    (let [plugins (plugin/discover {:global (str fixtures "/global")
                                    :project (str fixtures "/project")})]
      (is (contains? plugins "test_a"))
      (is (contains? plugins "test_b"))
      (is (= :pane (get-in plugins ["test_a" :capability]))))))

(deftest project-wins-on-id-collision
  (let [plugins (plugin/discover {:global  (str fixtures "/collide-global")
                                  :project (str fixtures "/collide-project")})]
    (is (= "project" (get-in plugins ["test_a" :source])))))

(deftest capability-conflict-errors
  (is (thrown-with-msg? Exception #"capability :pane fulfilled by both"
        (plugin/discover {:global  (str fixtures "/double-pane-a")
                          :project (str fixtures "/double-pane-b")}))))

(deftest prefix-derivation
  (testing "capability plugin gets capability prefix"
    (is (= "pane" (plugin/prefix-for {:capability :pane :id "konsole"}))))
  (testing "non-capability plugin gets its id"
    (is (= "deploy" (plugin/prefix-for {:id "deploy"})))))

(deftest verb-collision-detection
  (testing "non-capability plugin colliding with capability prefix → error"
    (is (thrown-with-msg? Exception #"collides with capability prefix"
          (plugin/build-dispatch
            {"pane-impostor" {:id "pane" :cli {:run identity} :file "p.clj"}
             "konsole"          {:id "konsole" :capability :pane :cli {} :file "k.clj"}}
            plugin/capability->prefix)))))

(deftest build-dispatch-keyed-by-prefix
  (let [d (plugin/build-dispatch
            {"konsole"   {:id "konsole"   :capability :pane      :cli {:spawn identity} :file "k.clj"}
             "wt"     {:id "wt"     :capability :workspace :cli {:new identity}   :file "w.clj"}
             "deploy" {:id "deploy"                        :cli {:run identity}   :file "d.clj"}}
            plugin/capability->prefix)]
    (is (= "konsole"   (get-in d ["pane"     :plugin :id])))
    (is (= "wt"     (get-in d ["ws"       :plugin :id])))
    (is (= "deploy" (get-in d ["deploy"   :plugin :id])))
    (is (contains? (get-in d ["pane"   :verbs]) :spawn))
    (is (contains? (get-in d ["ws"     :verbs]) :new))
    (is (contains? (get-in d ["deploy" :verbs]) :run))))
