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
    (is (str/includes? (:yaml out) "db:"))
    (is (str/includes? (:yaml out) "command:"))
    (is (str/includes? (:yaml out) "pg_ctl start"))
    (is (= {} (:env out)))))

(deftest transpile-merges-service-env
  (let [out (svc/transpile
              {:tasks {:db  {:run ["x"] :service true :env {:PGHOST "/tmp" :PGPORT "5432"}}
                       :app {:run ["y"] :service true :env {:DATABASE_URL "postgres://"}}}})]
    (is (= {:PGHOST "/tmp" :PGPORT "5432" :DATABASE_URL "postgres://"}
           (:env out)))))

(deftest transpile-kebab-to-snake-on-service-fields
  (let [out (svc/transpile
              {:tasks
               {:db {:run ["x"] :service true
                     :process-compose
                     {:readiness-probe {:exec {:command "pg_isready"}}
                      :depends-on {:other {:condition "process_healthy"}}
                      :shutdown {:command "stop" :timeout-sec 10}}}}})]
    (is (str/includes? (:yaml out) "readiness_probe:"))
    (is (str/includes? (:yaml out) "depends_on:"))
    (is (str/includes? (:yaml out) "timeout_seconds: 10"))))

(deftest transpile-script-vector-becomes-multiline-command
  (let [out (svc/transpile
              {:tasks {:setup {:run ["echo step1" "echo step2"] :service true}}})]
    ;; clj-yaml emits block scalar (|-) so both lines appear in output
    (is (str/includes? (:yaml out) "echo step1"))
    (is (str/includes? (:yaml out) "echo step2"))))

(deftest transpile-passthrough-unknown-fields
  (let [out (svc/transpile
              {:tasks {:db {:run ["x"] :service true
                            :process-compose {:is-daemon true
                                              :log-location "/tmp/db.log"}}}})]
    (is (str/includes? (:yaml out) "is_daemon: true"))
    (is (str/includes? (:yaml out) "log_location: /tmp/db.log"))))

(deftest transpile-later-service-env-wins-on-collision
  (let [out (svc/transpile
              {:tasks {:a {:run ["x"] :service true :env {:PORT "1"}}
                       :b {:run ["y"] :service true :env {:PORT "2"}}}})]
    ;; sorted iteration: :a then :b; :b wins
    (is (= "2" (-> out :env :PORT)))))

(deftest transpile-depends-on-preserves-service-name-keys
  (let [out (svc/transpile
              {:tasks {:web-app {:run ["x"] :service true
                                 :process-compose
                                 {:depends-on {:web-app {:condition "process_started"}}}}}})]
    ;; both the process key AND the depends_on key should be `web-app`
    (is (str/includes? (:yaml out) "web-app:"))
    (is (not (str/includes? (:yaml out) "web_app:")))))

(deftest transpile-env-becomes-list-of-strings-in-yaml
  (let [out (svc/transpile
              {:tasks {:db {:run ["x"] :service true :env {:PGHOST "/tmp" :PGPORT "5432"}}}})]
    ;; process-compose requires `environment:` as a list, not map
    (is (str/includes? (:yaml out) "- PGHOST=/tmp"))
    (is (str/includes? (:yaml out) "- PGPORT=5432"))))

(deftest transpile-ignores-non-service-tasks
  (let [out (svc/transpile
              {:tasks {:db   {:run ["pg"] :service true}
                       :test {:run ["bun test"]}}})]
    (is (str/includes? (:yaml out) "db:"))
    (is (not (str/includes? (:yaml out) "test:")))))

(deftest transpile-service-function-returning-command
  (let [out (svc/transpile {:tasks {:db {:run (fn [_] ["pg_ctl" "start"])
                                        :service true}}})]
    (is (str/includes? (:yaml out) "pg_ctl"))
    (is (str/includes? (:yaml out) "start"))))

(deftest transpile-service-function-returning-nil-errors
  (is (thrown-with-msg? Exception #"service task :db must produce a command"
        (svc/transpile {:tasks {:db {:run (fn [_] nil) :service true}}}))))

(deftest up-pc-attach-execs-process-compose-foreground
  (let [tmp      (str (babashka.fs/create-temp-dir))
        sock     (str (babashka.fs/create-temp-dir) "/pc.sock")
        captured (atom nil)
        captured-env (atom nil)]
    (with-redefs [babashka.process/shell
                  (fn [opts & cmd]
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
           (clojure.string/trim (slurp (str tmp "/running")))))))

(deftest up-pc-detached-sets-pc-socket-path
  (let [tmp           (str (babashka.fs/create-temp-dir))
        sock          (str (babashka.fs/create-temp-dir) "/pc.sock")
        captured-args (atom nil)
        captured-env  (atom nil)]
    (with-redefs [babashka.process/shell
                  (fn [opts & cmd]
                    (reset! captured-args (vec cmd))
                    (reset! captured-env (:extra-env opts))
                    {:exit 0})]
      (svc/up-pc
        {:tasks {:db {:run ["x"] :service true}}}
        {:state-dir tmp :sock-path sock :extra-args [] :attach? false}))
    (is (some #(= % "-D") @captured-args))
    (is (some #(= % "-U") @captured-args))
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
  (let [cfg {:tasks {:db   {:run ["pg"] :service true :env {:K "v"}}
                     :test {:run ["bun test"]}}}]
    (is (= {:db {:run ["pg"] :service true :env {:K "v"}}}
           (svc/service-tasks cfg)))))

(deftest parse-process-list-extracts-fields
  (let [payload "[{\"name\":\"start\",\"status\":\"Running\",\"pid\":12345,\"is_ready\":\"Ready\",\"restarts\":0,\"age\":723000000000}]"
        procs   (svc/parse-process-list payload)]
    (is (= 1 (count procs)))
    (is (= "start"   (:name (first procs))))
    (is (= "Running" (:status (first procs))))
    (is (= 12345     (:pid (first procs))))
    (is (= "Ready"   (:is-ready (first procs))))))

(deftest readiness-verdict-running-standard
  (let [procs [{:name "a" :status "Running" :is-ready "Ready"}
               {:name "b" :status "Launching" :is-ready "-"}]]
    (is (false? (svc/all-ready? procs :running)))
    (is (true?  (svc/all-ready? [{:name "a" :status "Running" :is-ready "-"}] :running)))))

(deftest readiness-verdict-ready-standard-honors-probe
  (let [with-probe    {:name "a" :status "Running" :is-ready "Not Ready" :has-probe true}
        with-probe-ok {:name "a" :status "Running" :is-ready "Ready" :has-probe true}
        no-probe      {:name "b" :status "Running" :is-ready "-" :has-probe false}]
    (is (false? (svc/all-ready? [with-probe no-probe] :ready)))
    (is (true?  (svc/all-ready? [with-probe-ok no-probe] :ready)))))

(deftest service-line-ok-and-failed
  (is (str/includes? (svc/service-line {:name "start" :status "Running" :pid 12345
                                        :url "http://127.0.0.1:2001"} true)
                     "✓"))
  (is (str/includes? (svc/service-line {:name "start" :status "Launching"} false)
                     "✗")))
