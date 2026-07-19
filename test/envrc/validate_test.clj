(ns envrc.validate-test
  (:require [clojure.test :refer [deftest is testing]]
            [envrc.validate :as v]
            [malli.core]))

(deftest validate-passes-on-valid-shape
  (is (nil? (v/validate! [:map [:x :int]] {:x 1}))))

(deftest validate-throws-on-shape-mismatch
  (is (thrown? Exception
        (v/validate! [:map [:x :int]] {:x "not an int"}))))

(deftest validate-throws-with-reason-for-disallowed-key
  (testing "closed map rejects unknown keys; ex-info carries caller's :reason"
    (let [err (try (v/validate! [:map {:closed true} [:tasks :any]] {:commands {}}
                                :reason :unknown-key)
                   (catch Exception e (ex-data e)))]
      (is (= :unknown-key (:reason err))))))

(deftest validate-with-aliases-injects-suggestion
  (testing "synonym 'cmds' suggests 'tasks' via envrc.schemas/aliases"
    (let [err (try (v/validate! [:map {:closed true}
                                  [:tasks :any] [:files :any]]
                                {:cmds {}}
                                :reason :unknown-key
                                :allowed-keys [:tasks :files])
                   (catch Exception e (ex-data e)))]
      (is (= "tasks" (:suggestion err))))))

(deftest validate-with-levenshtein-suggestion
  (testing "typo 'taks' suggests 'tasks'"
    (let [err (try (v/validate! [:map {:closed true}
                                  [:tasks :any] [:files :any]]
                                {:taks {}}
                                :reason :unknown-key
                                :allowed-keys [:tasks :files])
                   (catch Exception e (ex-data e)))]
      (is (= "tasks" (:suggestion err))))))

(deftest effective-events-include-plugin-events
  (let [plugins {"konsole" {:id "konsole"
                         :extends {:events #{:open :pre-spawn}}}}
        {:keys [Event]} (v/build-effective-schemas plugins)]
    (is (nil? (malli.core/explain Event :shell)))
    (is (nil? (malli.core/explain Event :open)))
    (is (nil? (malli.core/explain Event :pre-spawn)))
    (is (some? (malli.core/explain Event :bogus)))))

(deftest task-accepts-plugin-subnamespace
  (testing "plugin :extends.tasks schema folded under :<plugin-id> sub-map"
    (let [plugins {"process-compose"
                   {:id "process-compose"
                    :extends {:tasks [:map
                                      [:availability {:optional true} :any]]}}}
          {:keys [Task]} (v/build-effective-schemas plugins)
          task {:run "x" :process-compose {:availability {:restart "always"}}}]
      (is (nil? (malli.core/explain Task task))))))

(deftest task-rejects-plugin-fields-at-top-level
  (testing "supervisor fields nested only — flat top-level rejected"
    (let [plugins {"process-compose"
                   {:id "process-compose"
                    :extends {:tasks [:map
                                      [:availability {:optional true} :any]]}}}
          {:keys [Task]} (v/build-effective-schemas plugins)
          task {:run "x" :availability {:restart "always"}}]
      (is (some? (malli.core/explain Task task))))))

(deftest effective-task-schema-still-rejects-unknown-keys
  (let [plugins {"process-compose"
                 {:id "process-compose"
                  :extends {:tasks [:map [:availability {:optional true} :any]]}}}
        {:keys [Task]} (v/build-effective-schemas plugins)]
    (testing "base :run still accepted alongside plugin-contributed sub-map"
      (is (nil? (malli.core/explain Task {:run "echo x"})))
      (is (nil? (malli.core/explain Task {:run ["echo" "x"]}))))
    (testing "unknown task keys still rejected by closed map"
      (is (some? (malli.core/explain Task {:made-up-key "x"}))))))

(deftest validate-reports-all-errors
  (let [bad-cfg {:tasks {:my-task {:run 123}}}    ; :run must be string/vec/fn
        plugins {}
        Edn (-> (v/build-effective-schemas plugins) :EnvrcEdn)]
    (try
      (v/validate! Edn bad-cfg :reason :envrc-edn-error)
      (is false "should have thrown")
      (catch clojure.lang.ExceptionInfo e
        (let [errors (:errors (ex-data e))
              msg    (.getMessage e)]
          (is (>= (count errors) 1)
              "expected at least one error")
          (is (re-find #"envrc: \d+ error" msg)
              "message should carry count header")
          (is (clojure.string/includes? msg "tasks/my-task/run")
              "message should list :tasks error path"))))))
