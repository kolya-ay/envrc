(ns envrc.services-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [babashka.fs :as fs]))

(require '[envrc.services :as svc])

(deftest up-pc-writes-no-env-file
  (let [state (str (fs/create-temp-dir))
        sock  (str (fs/create-temp-dir) "/pc.sock")
        cfg   {:tasks {:db {:run ["true"] :service true :env {:PGHOST "/tmp"}}}}]
    (with-redefs [babashka.process/shell (fn [& _] {:exit 0})]
      (svc/up-pc cfg {:state-dir state :sock-path sock :extra-args [] :attach? false}))
    (is (fs/exists? (str state "/process-compose.yml")))
    (is (fs/exists? (str state "/running")))
    (is (not (fs/exists? (str state "/.env"))))))

(deftest transpile-minimal-service
  (let [out (svc/transpile {:tasks {:db {:run ["pg_ctl start"] :service true}}})]
    (is (str/includes? out "db:"))
    (is (str/includes? out "command:"))
    (is (str/includes? out "pg_ctl start"))))

(deftest transpile-resolves-service-env-per-service
  (let [yaml (svc/transpile
              {:env {:DB_URL "postgres://global" :SECRET "global-secret"}
               :tasks {:api {:run ["echo api"] :service true :env {:PORT "3000"}}
                       :worker {:run ["echo worker"] :service true
                                :env {:DB_URL "postgres://worker" :SECRET nil}}}})]
    (is (str/includes? yaml "api:"))
    (is (str/includes? yaml "worker:"))
    (is (str/includes? yaml "- DB_URL=postgres://global"))
    (is (str/includes? yaml "- SECRET=global-secret"))
    (is (str/includes? yaml "- PORT=3000"))
    (is (str/includes? yaml "- DB_URL=postgres://worker"))
    (is (not (str/includes? yaml "- SECRET=nil")))
    (is (not (str/includes? yaml "worker:\n    command: env -u SECRET -- 'echo worker'\n    environment:\n    - DB_URL=postgres://worker\n    - SECRET=")))
    (is (str/includes? yaml "env -u SECRET --"))
    (is (not (str/includes? yaml "env -u SECRET -- 'echo api'")))
    (is (str/includes? yaml "command: '''echo api'''"))
    (is (str/includes? yaml "command: env -u SECRET -- sh -c ''\\''echo worker'\\'''"))))

(deftest transpile-kebab-to-snake-on-service-fields
  (let [out (svc/transpile
              {:tasks
               {:db {:run ["x"] :service true
                     :process-compose
                     {:readiness-probe {:exec {:command "pg_isready"}}
                      :depends-on {:other {:condition "process_healthy"}}
                      :shutdown {:command "stop" :timeout-sec 10}}}}})]
    (is (str/includes? out "readiness_probe:"))
    (is (str/includes? out "depends_on:"))
    (is (str/includes? out "timeout_seconds: 10"))))

(deftest transpile-script-vector-becomes-multiline-command
  (let [out (svc/transpile
              {:tasks {:setup {:run ["echo step1" "echo step2"] :service true}}})]
    (is (str/includes? out "echo step1"))
    (is (str/includes? out "echo step2"))))

(deftest transpile-passthrough-unknown-fields
  (let [out (svc/transpile
              {:tasks {:db {:run ["x"] :service true
                            :process-compose {:is-daemon true
                                              :log-location "/tmp/db.log"}}}})]
    (is (str/includes? out "is_daemon: true"))
    (is (str/includes? out "log_location: /tmp/db.log"))))

(deftest transpile-later-service-env-wins-on-collision
  (let [out (svc/transpile
              {:tasks {:a {:run ["x"] :service true :env {:PORT "1"}}
                       :b {:run ["y"] :service true :env {:PORT "2"}}}})]
    (is (str/includes? out "- PORT=1"))
    (is (str/includes? out "- PORT=2"))))

(deftest transpile-depends-on-preserves-service-name-keys
  (let [out (svc/transpile
              {:tasks {:web-app {:run ["x"] :service true
                                 :process-compose
                                 {:depends-on {:web-app {:condition "process_started"}}}}}})]
    (is (str/includes? out "web-app:"))
    (is (not (str/includes? out "web_app:")))))

(deftest transpile-env-becomes-list-of-strings-in-yaml
  (let [out (svc/transpile
              {:tasks {:db {:run ["x"] :service true :env {:PGHOST "/tmp" :PGPORT "5432"}}}})]
    (is (str/includes? out "- PGHOST=/tmp"))
    (is (str/includes? out "- PGPORT=5432"))))

(deftest transpile-ignores-non-service-tasks
  (let [out (svc/transpile
              {:tasks {:db {:run ["x"] :service true}
                       :test {:run ["bun test"]}}})]
    (is (str/includes? out "db:"))
    (is (not (str/includes? out "test:")))))

(deftest transpile-service-function-returning-command
  (let [out (svc/transpile {:tasks {:db {:run (fn [_] ["echo" "start"]) :service true}}})]
    (is (str/includes? out "start"))))

(deftest transpile-service-function-returning-nil-errors
  (is (thrown-with-msg? Exception #"service task :db must produce a command"
        (svc/transpile {:tasks {:db {:run (fn [_] nil) :service true}}}))))


(deftest transpile-string-service-wraps-full-shell-command-for-unset
  (let [yaml (svc/transpile {:env {:SECRET "secret"}
                             :tasks {:db {:run "cd /tmp && echo \"$SECRET\""
                                          :service true
                                          :env {:SECRET nil}}}})]
    (is (str/includes? yaml "command: env -u SECRET -- sh -c 'cd /tmp && echo \"$SECRET\"'"))))

(deftest up-pc-attach-execs-process-compose-foreground
  (let [tmp      (str (babashka.fs/create-temp-dir))
        sock     (str (babashka.fs/create-temp-dir) "/pc.sock")
        captured (atom nil)
        captured-env (atom nil)]
    (with-redefs [babashka.process/shell (fn [opts & cmd]
                                           (reset! captured (vec cmd))
                                           (reset! captured-env (:extra-env opts))
                                           {:exit 0})]
      (svc/up-pc
        {:tasks {:db {:run ["echo hi"] :service true :env {:PGHOST "/tmp"}}}}
        {:state-dir tmp :sock-path sock :extra-args [] :attach? true}))
    (is (clojure.string/includes?
          (slurp (str tmp "/process-compose.yml")) "db:"))
    (is (= ["process-compose" "-f" (str tmp "/process-compose.yml")
            "-U" "-u" sock "up"]
           @captured))
    (is (= sock (get @captured-env "PC_SOCKET_PATH")))
    (is (= "process-compose"
           (clojure.string/trim (slurp (str tmp "/running"))))))
)

(deftest up-pc-detached-sets-pc-socket-path
  (let [tmp          (str (babashka.fs/create-temp-dir))
        sock         (str (babashka.fs/create-temp-dir) "/pc.sock")
        captured-args (atom nil)
        captured-env  (atom nil)]
    (with-redefs [babashka.process/shell (fn [opts & cmd]
                                           (reset! captured-args (vec cmd))
                                           (reset! captured-env (:extra-env opts))
                                           {:exit 0})]
      (svc/up-pc
        {:tasks {:db {:run ["x"] :service true}}}
        {:state-dir tmp :sock-path sock :extra-args [] :attach? false}))
    (is (= ["process-compose" "-f" (str tmp "/process-compose.yml")
            "-U" "-u" sock "up" "-D"]
           @captured-args))
    (is (= sock (get @captured-env "PC_SOCKET_PATH")))))

(deftest restart-wrap-always
  (is (= "while true; do echo x; sleep 1; done"
         (#'svc/wrap-for-restart "echo x" "always"))))

(deftest restart-wrap-on-failure
  (is (= "until echo x; do sleep 1; done"
         (#'svc/wrap-for-restart "echo x" "on_failure"))))

(deftest restart-wrap-no-or-default
  (is (= "echo x" (#'svc/wrap-for-restart "echo x" "no")))
  (is (= "echo x" (#'svc/wrap-for-restart "echo x" nil))))

(deftest topological-order-respects-depends-on
  (let [services {:app {:process-compose {:depends-on {:db {:condition "process_started"}}}}
                  :db  {}
                  :worker {:process-compose {:depends-on {:app {:condition "process_started"}}}}}]
    (is (= [:db :app :worker] (#'svc/topo-order services)))))

(deftest topological-order-cycle-throws
  (let [services {:a {:process-compose {:depends-on {:b {:condition "process_started"}}}}
                  :b {:process-compose {:depends-on {:a {:condition "process_started"}}}}}]
    (is (thrown-with-msg? Exception #"cycle"
          (#'svc/topo-order services)))))

(deftest service-tasks-extracted-from-unified
  (let [cfg {:tasks {:db {:service true} :build {:service false} :lint {}}}]
    (is (= {:db {:service true}}
           (svc/service-tasks cfg)))))

(deftest parse-process-list-extracts-fields
  (let [procs (#'svc/parse-process-list "[{\"name\":\"api\",\"status\":\"Running\",\"pid\":12,\"is_ready\":\"Ready\",\"restarts\":0,\"age\":\"1s\"}]")]
    (is (= "api"     (:name (first procs))))
    (is (= "Running" (:status (first procs))))
    (is (= "Ready"   (:is-ready (first procs))))))

(deftest readiness-verdict-running-standard
  (is (true?  (svc/all-ready? [{:name "a" :status "Running" :is-ready "-"}] :running))))

(deftest readiness-verdict-ready-standard-honors-probe
  (let [with-probe-ok {:name "a" :status "Running" :is-ready "Ready"}
        no-probe      {:name "b" :status "Running" :is-ready "-"}]
    (is (true?  (svc/all-ready? [with-probe-ok no-probe] :ready)))))

(deftest service-line-ok-and-failed
  (is (str/includes? (#'svc/service-line {:name "a" :status "Running" :is-ready "Ready"} :ready) "✓"))
  (is (str/includes? (#'svc/service-line {:name "a" :status "Exited" :is-ready "-"} false)
                     "✗")))
