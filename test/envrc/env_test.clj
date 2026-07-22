(ns envrc.env-test
  (:require [clojure.test :refer [deftest is testing]]
            [envrc.env :as env]))

(deftest port-env-builds-port-map-with-shared-offset-precedence
  (let [cfg {:project {:workspace "main"}
             :use {:ports {:base 4000 :stride 10 :vars [:A :B] :offset 5}}}]
    (with-redefs [envrc.env/env-port-offset (constantly 7)
                  envrc.ports/offset       (constantly 42)]
      (is (= {:A "4070" :B "4071"}
             (env/port-env cfg)))))
  (testing "disabled ports return empty map"
    (is (= {} (env/port-env {:project {:workspace "main"}}))))
  (testing "config offset overrides workspace hash"
    (let [cfg {:project {:workspace "main"}
               :use {:ports {:base 4000 :stride 10 :vars [:A] :offset 5}}}]
      (with-redefs [envrc.env/env-port-offset (constantly nil)
                    envrc.ports/offset       (constantly 42)]
        (is (= {:A "4050"}
               (env/port-env cfg))))))
  (testing "workspace hash used when no overrides exist"
    (let [cfg {:project {:workspace "main"}
               :use {:ports {:base 4000 :stride 10 :vars [:A]}}}]
      (with-redefs [envrc.env/env-port-offset (constantly nil)
                    envrc.ports/offset       (constantly 42)]
        (is (= {:A "4420"}
               (env/port-env cfg)))))))

(deftest global-env-resolves-forward-port-and-numeric-references
  (let [cfg {:project {:workspace "main"}
             :use {:ports {:base 4000 :stride 10 :vars [:APP_PORT] :offset 7}}
             :env {:APP_URL "http://127.0.0.1:${APP_PORT}"
                   :GREETING "hello ${TARGET}"
                   :TARGET "world"
                   :COUNT 12
                   :RATIO 1.5
                   :LABEL "n=${COUNT}"
                   :RATIO_LABEL "r=${RATIO}"
                   :OMIT nil}}]
    (with-redefs [envrc.env/env-port-offset (constantly nil)
                  envrc.ports/offset       (constantly 42)]
      (is (= {:APP_URL "http://127.0.0.1:4070"
              :GREETING "hello world"
              :TARGET "world"
              :COUNT "12"
              :RATIO "1.5"
              :LABEL "n=12"
              :RATIO_LABEL "r=1.5"}
             (env/global-env cfg))))))

(deftest task-env-resolves-overrides-shadows-unsets-and-numbers
  (let [cfg {:project {:workspace "main"}
             :use {:ports {:base 4000 :stride 10 :vars [:APP_PORT :ADMIN_PORT] :offset 7}}
             :env {:APP_URL "http://127.0.0.1:${APP_PORT}"
                   :GREETING "hello"
                   :GLOBAL_ONLY "global"}
             :tasks {:serve {:env {:APP_PORT 9000
                                   :GREETING "hi ${APP_URL}"
                                   :APP_URL "http://127.0.0.1:${APP_PORT}"
                                   :GLOBAL_ONLY nil
                                   :ADMIN_PORT nil
                                   :TASK_ONLY 5
                                   :DECIMAL 1.5
                                   :DECIMAL_LABEL "v=${DECIMAL}"
                                   :ZZZ nil
                                   :AAA nil}}}}]
    (with-redefs [envrc.env/env-port-offset (constantly nil)
                  envrc.ports/offset       (constantly 42)]
      (is (= {:set {:APP_PORT "9000"
                    :APP_URL "http://127.0.0.1:9000"
                    :GREETING "hi http://127.0.0.1:9000"
                    :TASK_ONLY "5"
                    :DECIMAL "1.5"
                    :DECIMAL_LABEL "v=1.5"}
              :unset [:ADMIN_PORT :GLOBAL_ONLY]}
             (env/task-env cfg (get-in cfg [:tasks :serve])))))))

(deftest global-env-rejects-unknown-reference
  (let [cfg {:env {:A "${MISSING}"}}]
    (try
      (env/global-env cfg)
      (is false "expected exception")
      (catch clojure.lang.ExceptionInfo e
        (is (= :A (:var (ex-data e))))
        (is (= [:A] (:stack (ex-data e))))
        (is (= "${MISSING}" (:template (ex-data e))))
        (is (re-find #"unknown" (ex-message e)))))))

(deftest global-env-rejects-reference-to-unset-variable
  (let [cfg {:env {:A "${B}" :B nil}}]
    (try
      (env/global-env cfg)
      (is false "expected exception")
      (catch clojure.lang.ExceptionInfo e
        (is (= :A (:var (ex-data e))))
        (is (= [:A] (:stack (ex-data e))))
        (is (= "${B}" (:template (ex-data e))))
        (is (re-find #"nil" (ex-message e)))))))

(deftest global-env-rejects-cycles
  (let [cfg {:env {:A "${B}" :B "${A}"}}]
    (try
      (env/global-env cfg)
      (is false "expected exception")
      (catch clojure.lang.ExceptionInfo e
        (is (= :A (:var (ex-data e))))
        (is (= [:A :B :A] (:stack (ex-data e))))
        (is (= "${B}" (:template (ex-data e))))
        (is (re-find #"cycle" (ex-message e)))))))

(deftest env-rejects-unsupported-value-types
  (doseq [[label f cfg] [[:global env/global-env {:env {:A true}}]
                         [:task   #(env/task-env % (get-in % [:tasks :serve]))
                          {:tasks {:serve {:env {:A [:bad]}}}}]]]
    (try
      (f cfg)
      (is false (str "expected exception for " label))
      (catch clojure.lang.ExceptionInfo e
        (is (= :A (:var (ex-data e))))
        (is (= label (:scope (ex-data e))))
        (is (re-find #"must be a string, number, or nil" (ex-message e)))))))

(deftest task-env-rejects-unknown-reference
  (let [cfg {:tasks {:serve {:env {:A "${MISSING}"}}}}
        task (get-in cfg [:tasks :serve])]
    (try
      (env/task-env cfg task)
      (is false "expected exception")
      (catch clojure.lang.ExceptionInfo e
        (is (= :A (:var (ex-data e))))
        (is (= [:A] (:stack (ex-data e))))
        (is (= "${MISSING}" (:template (ex-data e))))
        (is (re-find #"unknown" (ex-message e)))))))

(deftest task-env-rejects-reference-to-task-nil
  (let [cfg {:env {:B "global"}
             :tasks {:serve {:env {:A "${B}" :B nil}}}}
        task (get-in cfg [:tasks :serve])]
    (try
      (env/task-env cfg task)
      (is false "expected exception")
      (catch clojure.lang.ExceptionInfo e
        (is (= :A (:var (ex-data e))))
        (is (= [:A] (:stack (ex-data e))))
        (is (= "${B}" (:template (ex-data e))))
        (is (re-find #"nil" (ex-message e)))))))

(deftest task-env-rejects-cycles
  (let [cfg {:tasks {:serve {:env {:A "${B}" :B "${A}"}}}}
        task (get-in cfg [:tasks :serve])]
    (try
      (env/task-env cfg task)
      (is false "expected exception")
      (catch clojure.lang.ExceptionInfo e
        (is (= :A (:var (ex-data e))))
        (is (= [:A :B :A] (:stack (ex-data e))))
        (is (= "${B}" (:template (ex-data e))))
        (is (re-find #"cycle" (ex-message e)))))))
