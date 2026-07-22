(ns envrc.run-test
  (:require [clojure.test :refer [deftest is testing]]
            [envrc.run :as run]))

(def ^:private wrap-unset-shell-command
  (resolve 'envrc.run/wrap-unset-shell-command))

(deftest evaluate-string-run
  (is (= "echo hi"
         (run/evaluate {:tasks {}} :hello {:run "echo hi"} {:root "."}))))

(deftest evaluate-argv-run
  (is (= ["echo" "hi"]
         (run/evaluate {} :hello {:run ["echo" "hi"]} {:root "."}))))

(deftest evaluate-function-returning-nil
  (is (nil? (run/evaluate {} :noop {:run (fn [_] nil)} {:root "."}))))

(deftest evaluate-function-returning-string
  (is (= "echo dyn"
         (run/evaluate {}
                       :dyn
                       {:run (fn [{:keys [task-name]}]
                               (str "echo " (name task-name)))}
                       {:root "."}))))

(deftest evaluate-function-returning-argv
  (is (= ["echo" "from-fn"]
         (run/evaluate {} :dyn {:run (fn [_] ["echo" "from-fn"])} {:root "."}))))

(deftest evaluate-function-gets-resolved-env-context
  (let [seen (atom nil)
        task {:run (fn [ctx] (reset! seen ctx) nil)
              :env {:PORT 9000
                    :SECRET nil
                    :URL "http://127.0.0.1:${PORT}"}}
        cfg {:env {:FOO "bar" :SECRET "secret"}
             :tasks {:x task}}]
    (run/evaluate cfg :x task {:root "/repo" :event :enter})
    (is (= :x (:task-name @seen)))
    (is (= task (:task @seen)))
    (is (= "/repo" (:root @seen)))
    (is (= :enter (:event @seen)))
    (is (= {"FOO" "bar" "PORT" "9000" "URL" "http://127.0.0.1:9000"}
           (:env @seen)))))

(deftest evaluate-function-context-includes-nil-event
  (let [seen (atom nil)
        task {:run (fn [ctx] (reset! seen ctx) nil)}]
    (run/evaluate {:tasks {:x task}} :x task {:root "/repo"})
    (is (contains? @seen :event))
    (is (nil? (:event @seen)))))

(deftest evaluate-invalid-return-errors
  (is (thrown-with-msg? Exception #"envrc: task :bad returned invalid :run value"
        (run/evaluate {} :bad {:run (fn [_] {:exit 0})} {:root "."}))))

(deftest evaluate-missing-run-errors
  (is (thrown-with-msg? Exception #"envrc: task :bad has no :run"
        (run/evaluate {} :bad {} {:root "."}))))

(deftest evaluate-invalid-run-errors
  (testing "empty argv is invalid"
    (is (thrown-with-msg? Exception #"envrc: task :bad has invalid :run"
          (run/evaluate {} :bad {:run []} {:root "."})))))

(deftest execute-string-uses-process-with-unset-wrapper-and-derefs-result
  (let [captured (atom nil)
        derefed? (atom false)
        completed {:exit 0 :out "" :err ""}
        fake-proc (reify clojure.lang.IDeref
                    (deref [_]
                      (reset! derefed? true)
                      completed))
        task {:run "echo hi"
              :env {:PORT 9000 :SECRET nil :TOKEN nil :URL "http://127.0.0.1:${PORT}"}}
        cfg {:env {:FOO "bar" :SECRET "secret" :TOKEN "token"}
             :tasks {:hello task}}]
    (with-redefs [babashka.process/process (fn [opts & argv]
                                             (reset! captured {:opts opts :argv (vec argv)})
                                             fake-proc)]
      (is (= completed
             (run/execute cfg :hello task {:root "."})))
      (is @derefed?)
      (is (= ["env" "-u" "SECRET" "-u" "TOKEN" "--" "sh" "-c" "echo hi"]
             (:argv @captured)))
      (is (= {"FOO" "bar" "PORT" "9000" "URL" "http://127.0.0.1:9000"}
             (get-in @captured [:opts :extra-env]))))))


(deftest execute-argv-uses-resolved-task-env
  (let [captured (atom nil)
        task {:run ["echo" "hi"]
              :env {:PORT 9000 :SECRET nil :URL "http://127.0.0.1:${PORT}"}}
        cfg {:env {:FOO "bar" :SECRET "secret"}
             :tasks {:hello task}}]
    (with-redefs [babashka.process/process (fn [opts & argv]
                                             (reset! captured {:opts opts :argv (vec argv)})
                                             {:exit 0})]
      (is (= {:exit 0}
             (run/execute cfg :hello task {:root "."})))
      (is (= ["env" "-u" "SECRET" "--" "echo" "hi"] (:argv @captured)))
      (is (= {"FOO" "bar" "PORT" "9000" "URL" "http://127.0.0.1:9000"}
             (get-in @captured [:opts :extra-env]))))))

(deftest execute-nil-uses-resolved-task-env-for-function-context
  (let [seen (atom nil)
        task {:run (fn [ctx] (reset! seen ctx) nil)
              :env {:PORT 9000 :SECRET nil :URL "http://127.0.0.1:${PORT}"}}
        cfg {:env {:FOO "bar" :SECRET "secret"}
             :tasks {:noop task}}]
    (is (= {:exit 0} (run/execute cfg :noop task {:root "."})))
    (is (= {"FOO" "bar" "PORT" "9000" "URL" "http://127.0.0.1:9000"}
           (:env @seen)))))

(deftest execute-nil-is-success
  (is (= {:exit 0} (run/execute {} :noop {:run (fn [_] nil)} {:root "."}))))

(deftest shell-body-coerces-string-and-argv-and-omits-nil
  (is (= "echo hi" (run/shell-body {} :a {:run "echo hi"} {:root "."})))
  (is (= "'echo' 'hi there'" (run/shell-body {} :a {:run ["echo" "hi there"]} {:root "."})))
  (is (nil? (run/shell-body {} :a {:run (fn [_] nil)} {:root "."}))))

(deftest service-shell-body-rejects-nil
  (is (thrown-with-msg? Exception #"envrc: service task :svc must produce a command"
        (run/shell-body {}
                        :svc
                        {:run (fn [_] nil)}
                        {:root "."}
                        {:require-command true :surface :service}))))

(deftest editor-entry-uses-resolved-task-env-and-unsets
  (let [task {:run ["npm" "test"]
              :env {:PORT 9000 :SECRET nil :URL "http://127.0.0.1:${PORT}"}}
        cfg {:env {:FOO "bar" :SECRET "secret"}
             :tasks {:test task}}
        expected-env {"FOO" "bar" "PORT" "9000" "URL" "http://127.0.0.1:9000" "SECRET" nil}]
    (is (= {:label "t"
            :type "shell"
            :command "env -u SECRET -- sh -c 'npm test'"
            :options {:env expected-env}}
           (run/editor-entry cfg :t :test (assoc task :run "npm test") {:root "."} :vscode)))
    (is (= {:label "t"
            :type "process"
            :command "npm"
            :args ["test"]
            :options {:env expected-env}}
           (run/editor-entry cfg :t :test task {:root "."} :vscode)))
    (is (= {:label "t"
            :command "npm"
            :args ["test"]
            :env expected-env}
           (run/editor-entry cfg :t :test task {:root "."} :zed)))
    (is (= {:label "t"
            :command "env -u SECRET -- sh -c 'npm test'"
            :shell "system"
            :env expected-env}
           (run/editor-entry cfg :t :test (assoc task :run "npm test") {:root "."} :zed)))))

(deftest editor-entry-omits-nil
  (is (nil? (run/editor-entry {} :noop :noop {:run (fn [_] nil)} {:root "."} :vscode)))
  (is (nil? (run/editor-entry {} :noop :noop {:run (fn [_] nil)} {:root "."} :zed))))

