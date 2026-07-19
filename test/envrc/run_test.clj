(ns envrc.run-test
  (:require [clojure.test :refer [deftest is testing]]
            [envrc.run :as run]))

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

(deftest evaluate-function-gets-context
  (let [seen (atom nil)
        task {:run (fn [ctx] (reset! seen ctx) nil)
              :env {:FOO "bar"}}]
    (run/evaluate {:tasks {:x task}} :x task {:root "/repo" :event :enter})
    (is (= :x (:task-name @seen)))
    (is (= task (:task @seen)))
    (is (= "/repo" (:root @seen)))
    (is (= :enter (:event @seen)))
    (is (= {"FOO" "bar"} (:env @seen)))))

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

(deftest execute-string-uses-shell
  (let [captured (atom nil)]
    (with-redefs [babashka.process/shell (fn [opts cmd]
                                           (reset! captured {:opts opts :cmd cmd})
                                           {:exit 0})]
      (is (= {:exit 0}
             (run/execute {} :hello {:run "echo hi" :env {:FOO "bar"}} {:root "."})))
      (is (= "echo hi" (:cmd @captured)))
      (is (= {"FOO" "bar"} (get-in @captured [:opts :extra-env]))))))

(deftest execute-argv-uses-process
  (let [captured (atom nil)]
    (with-redefs [babashka.process/process (fn [opts & argv]
                                             (reset! captured {:opts opts :argv (vec argv)})
                                             {:exit 0})]
      (is (= {:exit 0}
             (run/execute {} :hello {:run ["echo" "hi"]} {:root "."})))
      (is (= ["echo" "hi"] (:argv @captured))))))

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

(deftest editor-entry-coerces-string-and-argv
  (is (= {:label "t" :type "shell" :command "npm test" :options {:env {}}}
         (run/editor-entry {} :t :test {:run "npm test"} {:root "."} :vscode)))
  (is (= {:label "t" :type "process" :command "npm" :args ["test"] :options {:env {}}}
         (run/editor-entry {} :t :test {:run ["npm" "test"]} {:root "."} :vscode)))
  (is (= {:label "t" :command "npm" :args ["test"] :env {}}
         (run/editor-entry {} :t :test {:run ["npm" "test"]} {:root "."} :zed))))

(deftest editor-entry-omits-nil
  (is (nil? (run/editor-entry {} :noop :noop {:run (fn [_] nil)} {:root "."} :vscode)))
  (is (nil? (run/editor-entry {} :noop :noop {:run (fn [_] nil)} {:root "."} :zed))))
