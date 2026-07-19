(ns envrc.plugin.process-compose-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [babashka.fs :as fs]
            [envrc]
            [envrc.api]
            [envrc.capabilities]
            [envrc.dirs]))

(load-file "plugins/default/process-compose.clj")

(deftest plugin-provides-services-init-on-enter
  (let [plugin @(resolve 'envrc.plugin.process-compose/plugin)
        provides (:provides plugin)
        provided (if (fn? provides) (provides nil) provides)
        task    (get-in provided [:tasks :services-init])]
    (is (some? task) ":services-init task is provided")
    (is (= :enter (:on task)))
    (is (true? (:tolerant task)))))

(deftest services-init-body-has-no-dotenv-or-path-exports
  (let [body ((resolve 'envrc.plugin.process-compose/services-init-body) {})]
    (is (not (re-find #"dotenv " body)))
    (is (not (re-find #"\.env" body)))
    (is (not (re-find #"export ENVRC_SERVICES_STATE_DIR=" body)))
    (is (re-find #"watch_file .*running" body))
    (is (re-find #"ENVRC_SERVICES_BACKEND" body))))

(deftest services-env-emitter-emits-concrete-exports
  (let [plugin    @(resolve 'envrc.plugin.process-compose/plugin)
        transform (get-in plugin [:extends :emitters :services-env :transform])
        cfg       {:project {:scope :ego :slug "foo" :workspace "main"}}
        out       (binding [envrc.dirs/*env-overrides*
                            {"XDG_CACHE_HOME" "/c" "XDG_RUNTIME_DIR" "/run/u/1000" "PWD" "/home/u/proj"}]
                    (transform cfg))]
    (is (re-find #"export ENVRC_SERVICES_STATE_DIR=\"/c/direnv/layouts/" out))
    (is (re-find #"export ENVRC_SERVICES_SOCK=\"/run/u/1000/envrc/ego/foo/main/process-compose\.sock\"" out))
    (is (re-find #"mkdir -p " out))))

(deftest up-impl-backgrounds-and-reports-running
  (let [calls (atom [])
        sock-seq (atom 0)]
    (with-redefs
      [envrc.services/up-pc (fn [_ opts] (swap! calls conj opts) {:exit 0})
       envrc.plugin.process-compose/already-running? (fn [& _] false)
       envrc.plugin.process-compose/socket-alive?
       (fn [_] (let [n @sock-seq] (swap! sock-seq inc) (pos? n)))
       envrc.plugin.process-compose/query-processes
       (fn [_ _] [{:name "start" :status "Running" :pid 12345 :is-ready "-"}])
       envrc.api/root         (constantly "/tmp/proj")
       envrc.api/fire!        (fn [& _] nil)
       envrc/state-dir-for    (constantly "/tmp/services")
       envrc/sock-path-for    (constantly "/tmp/socket")]
      (let [out (with-out-str
                  (let [code ((resolve 'envrc.plugin.process-compose/up-impl)
                              {:tasks {:start {:run ["x"] :service true}}
                               :project {:scope :ego :slug "pi-route" :workspace "default"}}
                              {:args [] :opts {}})]
                    (is (or (nil? code) (zero? code)))))]
        (is (str/includes? out "Starting services for pi-route"))
        (is (str/includes? out "✓ start"))
        (is (str/includes? out "Running in background"))
        (is (false? (:attach? (first @calls))))))))

(deftest up-impl-idempotent-when-already-running
  (with-redefs
    [envrc.plugin.process-compose/already-running? (fn [& _] true)
     envrc.services/up-pc (fn [& _] (throw (ex-info "should not relaunch" {})))
     envrc.api/root       (constantly "/tmp/proj")
     envrc/state-dir-for  (constantly "/tmp/services")
     envrc/sock-path-for  (constantly "/tmp/socket")]
    (let [out (with-out-str
                ((resolve 'envrc.plugin.process-compose/up-impl)
                 {:tasks {:start {:run ["x"] :service true}}
                  :project {:scope :ego :slug "pi-route" :workspace "default"}}
                 {:args [] :opts {}}))]
      (is (str/includes? out "already running")))))

(deftest up-impl-attach-runs-foreground
  (let [calls (atom [])]
    (with-redefs [envrc.services/up-pc (fn [_ opts] (swap! calls conj opts) {:exit 0})
                  envrc.plugin.process-compose/already-running? (fn [& _] false)
                  envrc.api/root  (constantly "/tmp/proj")
                  envrc.api/fire! (fn [& _] nil)
                  envrc/state-dir-for (constantly "/tmp/services")
                  envrc/sock-path-for (constantly "/tmp/socket")]
      ((resolve 'envrc.plugin.process-compose/up-impl)
       {:tasks {:start {:run ["x"] :service true}}
        :project {:scope :ego :slug "pi-route" :workspace "default"}}
       {:args [] :opts {:attach true}})
      (is (true? (:attach? (first @calls)))))))

(deftest down-impl-reports-nothing-to-stop-when-marker-absent
  (let [sd (str (fs/create-temp-dir))]            ; no running file
    (with-redefs [envrc/state-dir-for (constantly sd)
                  envrc.plugin.process-compose/query-processes (fn [& _] nil)
                  envrc/delete-marker! (fn [_] nil)
                  envrc/pc-shell       (fn [& _] {:exit 0})
                  envrc.api/fire!      (fn [& _] nil)
                  envrc.api/root       (constantly "/tmp/proj")]
      (let [out (with-out-str
                  ((resolve 'envrc.plugin.process-compose/down-impl)
                   {:project {:scope :ego :slug "pi-route" :workspace "default"}} {:args []}))]
        (is (str/includes? out "Nothing to stop"))))))

(deftest plugin-has-no-list-or-env-verbs
  (let [plugin @(resolve 'envrc.plugin.process-compose/plugin)]
    (is (nil? (get-in plugin [:cli :list])))
    (is (nil? (get-in plugin [:cli :env])))
    (is (= #{:services} (:handles plugin)))
    (is (fn? (get-in plugin [:cli :status])))))

(deftest plugin-satisfies-service-contract
  (let [plugin @(resolve 'envrc.plugin.process-compose/plugin)]
    (is (nil? (envrc.capabilities/check-contract! plugin)))))

(deftest status-impl-brief-is-cheap-one-line
  (with-redefs [envrc.plugin.process-compose/socket-alive? (constantly true)
                envrc/sock-path-for (constantly "/tmp/pc.sock")
                envrc.api/root (constantly "/tmp/proj")]
    (let [out (with-out-str
                ((resolve 'envrc.plugin.process-compose/status-impl)
                 {:tasks {:start {:run ["x"] :service true}}
                  :project {:scope :ego :slug "pi-route" :workspace "default"}}
                 {:label :services :brief? true}))]
      (is (str/includes? out "services"))
      (is (str/includes? out "running"))
      (is (not (str/includes? out "NAME"))))))

(deftest status-impl-full-renders-table
  (with-redefs [envrc.plugin.process-compose/socket-alive? (constantly true)
                envrc.plugin.process-compose/query-processes
                (fn [_ _] [{:name "start" :status "Running" :pid 12345 :is-ready "-"}])
                envrc/sock-path-for (constantly "/tmp/pc.sock")
                envrc.api/root (constantly "/tmp/proj")]
    (let [out (with-out-str
                ((resolve 'envrc.plugin.process-compose/status-impl)
                 {:tasks {:start {:run ["x"] :service true}}
                  :project {:scope :ego :slug "pi-route" :workspace "default"}}
                 {:label :services :brief? false}))]
      (is (str/includes? out "Services for pi-route"))
      (is (str/includes? out "NAME"))
      (is (str/includes? out "start")))))

(deftest down-impl-frames-running-service-when-marker-present
  (let [sd (str (fs/create-temp-dir))]
    (spit (str sd "/running") "process-compose\n")
    (with-redefs [envrc/state-dir-for (constantly sd)
                  envrc.plugin.process-compose/query-processes
                  (fn [& _] [{:name "start" :status "Running" :age 723000000000}])
                  envrc/delete-marker! (fn [_] nil)
                  envrc/pc-shell       (fn [& _] {:exit 0})
                  envrc.api/fire!      (fn [& _] nil)
                  envrc.api/root       (constantly "/tmp/proj")]
      (let [out (with-out-str
                  ((resolve 'envrc.plugin.process-compose/down-impl)
                   {:project {:scope :ego :slug "pi-route" :workspace "default"}} {:args []}))]
        (is (str/includes? out "Stopping services for pi-route"))
        (is (str/includes? out "start"))
        (is (str/includes? out "Stopped"))))))

(deftest down-impl-cleans-stale-marker-when-socket-dead
  (let [sd (str (fs/create-temp-dir)) deleted (atom false)]
    (spit (str sd "/running") "process-compose\n")
    (with-redefs [envrc/state-dir-for (constantly sd)
                  envrc.plugin.process-compose/query-processes (fn [& _] nil)   ; socket dead
                  envrc/delete-marker! (fn [_] (reset! deleted true))
                  envrc/pc-shell       (fn [& _] {:exit 0})
                  envrc.api/fire!      (fn [& _] nil)
                  envrc.api/root       (constantly "/tmp/proj")]
      (let [out (with-out-str
                  ((resolve 'envrc.plugin.process-compose/down-impl)
                   {:project {:scope :ego :slug "pi-route" :workspace "default"}} {:args []}))]
        (is (str/includes? out "Stopping services"))   ; NOT "Nothing to stop"
        (is (true? @deleted))                            ; stale marker cleaned
        (is (str/includes? out "Stopped"))))))

(deftest up-impl-no-services-is-noop-success
  (with-redefs [envrc.plugin.process-compose/already-running? (fn [& _] false)
                envrc.services/up-pc (fn [& _] (throw (ex-info "should not launch" {})))
                envrc/state-dir-for  (constantly "/tmp/sd")
                envrc/sock-path-for  (constantly "/tmp/pc.sock")
                envrc.api/root       (constantly "/tmp/proj")]
    (let [code (atom nil)
          out  (with-out-str
                 (reset! code ((resolve 'envrc.plugin.process-compose/up-impl)
                               {:tasks {} :project {:scope :ego :slug "p" :workspace "default"}}
                               {:args [] :opts {}})))]
      (is (= 0 @code))
      (is (str/includes? out "No services defined")))))

(deftest up-impl-reports-failure-and-exits-nonzero
  (with-redefs [envrc.services/up-pc (fn [& _] {:exit 0})
                envrc.plugin.process-compose/already-running? (fn [& _] false)
                envrc.plugin.process-compose/socket-alive? (constantly true)
                envrc.plugin.process-compose/query-processes
                (fn [_ _] [{:name "start" :status "Launching" :is-ready "-"}])
                envrc/state-dir-for (constantly "/tmp/sd")
                envrc/sock-path-for (constantly "/tmp/pc.sock")
                envrc.api/root      (constantly "/tmp/proj")
                envrc.api/fire!     (fn [& _] nil)]
    (let [code (atom nil)
          out  (with-out-str
                 (reset! code ((resolve 'envrc.plugin.process-compose/up-impl)
                               {:tasks {:start {:run ["x"] :service true}}
                                :project {:scope :ego :slug "p" :workspace "default"}
                                :use {:process-compose {:await-timeout-sec 0}}}
                               {:args [] :opts {}})))]
      (is (= 1 @code))
      (is (str/includes? out "Started 0/1")))))
