(ns envrc.plugin.process-compose
  "process-compose service supervisor backend.
   Delegates to in-tree envrc.services for transpile/up logic; this file owns the
   plugin manifest and CLI handlers."
  (:require [envrc.api :as e]
            [envrc.services :as svc]
            [envrc :as envrc-cli]   ;; for state-dir-for (→ cache-dir), pc-shell, delete-marker!
            [envrc.dirs :as dirs]
            [cheshire.core :as cheshire]
            [clojure.string :as str]
            [babashka.fs :as fs]
            [babashka.process :as p]))

(defn- toplevel [] (System/getProperty "user.dir"))  ;; cwd from envrc-core invocation

(defn socket-alive?
  "True if the pc unix socket accepts a connection. Public: a with-redefs seam."
  [sock-path]
  (try
    (let [chan (java.nio.channels.SocketChannel/open
                 (java.net.UnixDomainSocketAddress/of sock-path))]
      (.close chan) true)
    (catch Throwable _ false)))

(defn query-processes
  "Run `process list -o json` over the socket; normalized records or nil.
   Annotates :has-probe from cfg readiness-probes. Public: a with-redefs seam."
  [tl cfg]
  (let [{:keys [exit out]} (envrc-cli/pc-shell-out tl "process" "list" "-o" "json")]
    (when (zero? exit)
      (let [probed (->> (svc/service-tasks cfg)
                        (filter (fn [[_ s]] (get-in s [:process-compose :readiness-probe])))
                        (map (comp name key)) set)]
        (mapv #(assoc % :has-probe (contains? probed (:name %)))
              (svc/parse-process-list out))))))

(defn already-running?
  "Idempotency precheck: socket alive AND every service meets the standard.
   Public: a with-redefs seam."
  [tl cfg sock standard]
  (and (socket-alive? sock)
       (boolean (when-let [ps (query-processes tl cfg)]
                  (svc/all-ready? ps standard)))))

(defn- await-standard [cfg]
  (keyword (or (get-in cfg [:use :process-compose :await]) :running)))

(defn- await-timeout-ms [cfg]
  (* 1000 (or (get-in cfg [:use :process-compose :await-timeout-sec]) 10)))

(defn- url-for [cfg svc-name]
  (when-let [hg (get-in cfg [:tasks (keyword svc-name) :process-compose :readiness-probe :http-get])]
    (str "http://" (or (:host hg) "127.0.0.1") ":" (:port hg) (or (:path hg) ""))))

(defn- count-services [cfg] (count (svc/service-tasks cfg)))

(defn- poll-until-ready [tl cfg standard deadline]
  (loop []
    (let [procs (query-processes tl cfg)]
      (cond
        (and procs (svc/all-ready? procs standard)) procs
        (> (System/currentTimeMillis) deadline)     procs
        :else (do (Thread/sleep 300) (recur))))))

(defn- up-impl [cfg {:keys [args opts]}]
  (let [tl       (or (e/root) (toplevel))
        slug     (get-in cfg [:project :slug] "project")
        ws       (get-in cfg [:project :workspace] "default")
        sd       (envrc-cli/state-dir-for tl)
        sock     (envrc-cli/sock-path-for tl)
        standard (await-standard cfg)
        attach?  (boolean (or (:attach opts) (:a opts)))]
    (cond
      (zero? (count-services cfg))
      (do (println "No services defined.") 0)

      (already-running? tl cfg sock standard)
      (do (println (str "Services already running for " slug "."))
          (println "  Status:  envrc status services")
          0)

      attach?
      (do (svc/up-pc cfg {:state-dir sd :sock-path sock :extra-args (vec args) :attach? true})
          (e/fire! :service-up)
          0)

      :else
      (do
        (println (str "Starting services for " slug " (workspace: " ws ")…"))
        (svc/up-pc cfg {:state-dir sd :sock-path sock :extra-args (vec args) :attach? false})
        (let [deadline (+ (System/currentTimeMillis) (await-timeout-ms cfg))]
          (while (and (not (socket-alive? sock))
                      (< (System/currentTimeMillis) deadline))
            (Thread/sleep 200))
          (let [procs (poll-until-ready tl cfg standard deadline)
                ok?   (svc/all-ready? procs standard)]
            (doseq [p (or procs [])]
              (println (svc/service-line (assoc p :url (url-for cfg (:name p)))
                                         (svc/proc-ready? p standard))))
            (if ok?
              (do (e/fire! :service-up)
                  (println)
                  (println "Running in background.")
                  (println "  Logs:    envrc services logs <name> -- -f")
                  (println "  Status:  envrc status services")
                  (println "  Attach:  envrc services attach")
                  (println "  Stop:    envrc services down")
                  0)
              (let [n (count procs) r (count (filter #(svc/proc-ready? % standard) procs))]
                (println (str "Started " r "/" n " within "
                              (quot (await-timeout-ms cfg) 1000) "s. "
                              "Tail with: envrc services logs <name> -n 50"))
                1))))))))

(defn- fmt-age [age-ns]
  (when (and age-ns (number? age-ns))
    (let [secs (long (/ age-ns 1e9)) m (quot secs 60) s (mod secs 60)]
      (format "%dm %02ds" m s))))

(defn- down-impl [cfg {:keys [args]}]
  (let [tl     (or (e/root) (toplevel))
        slug   (get-in cfg [:project :slug] "project")
        marker (str (envrc-cli/state-dir-for tl) "/running")]
    (if-not (fs/exists? marker)
      (println "Nothing to stop.")
      (let [procs (query-processes tl cfg)]
        (println (str "Stopping services for " slug "…"))
        (doseq [{:keys [name status age]} procs]
          (println (str "  ✓ " name "  (was: " status
                        (when-let [a (fmt-age age)] (str ", ran " a)) ")")))
        (envrc-cli/delete-marker! tl)
        (apply envrc-cli/pc-shell tl "down" args)
        (e/fire! :service-down)
        (println "Stopped.")))))

(defn- restart-impl [cfg {:keys [args]}]
  (let [name (first args)]
    (print (str "Restarting " name "…  "))
    (flush)
    (apply envrc-cli/pc-shell (toplevel) "process" "restart" args)
    (println "done.")))

(defn- attach-impl [cfg _opts]
  (let [slug (get-in cfg [:project :slug] "project")]
    (println (str "Attaching to " slug " services. Press 'q' or Ctrl-c to detach (services keep running)."))
    (envrc-cli/pc-shell (toplevel) "attach")))

(defn- logs-impl [_cfg {:keys [args opts]}]
  (apply envrc-cli/pc-shell (toplevel) "process" "logs"
         (cond-> (vec args) (:n opts) (into ["-n" (str (:n opts))]))))

(defn- status-impl [cfg {:keys [brief?] :as opts}]
  (let [tl   (or (e/root) (toplevel))
        slug (get-in cfg [:project :slug] "project")
        ws   (get-in cfg [:project :workspace] "default")
        sock (envrc-cli/sock-path-for tl)
        n    (count-services cfg)]
    (if brief?
      (println (str "  services — " (if (socket-alive? sock) "running" "stopped")
                    " (" n " defined)"))
      (let [procs (when (socket-alive? sock) (query-processes tl cfg))]
        (if (:json opts)
          (println (cheshire/generate-string
                     {:project slug :workspace ws :services (or procs [])}))
          (if (empty? procs)
            (println (str "Services for " slug ": stopped (" n " defined)."))
            (do
              (println (str "Services for " slug " (workspace: " ws "):"))
              (println "  NAME    STATUS    PID     UPTIME    READY")
              (doseq [{:keys [name status pid is-ready] :as p} procs]
                (println (format "  %-7s %-9s %-7s %-9s %s"
                                 name status (str pid)
                                 (or (fmt-age (:age p)) "-")
                                 (let [u (url-for cfg name)]
                                   (cond u (str "✓ " u)
                                         (= "Ready" is-ready) "✓"
                                         :else (or is-ready "-")))))))))))))

(defn services-env-shell
  "Emitter transform: compute the concrete state-dir + socket from the dir
   registry and emit shell exports. Single source of truth for service paths."
  [cfg]
  (binding [e/*context* {:cfg cfg}]
    (let [ctx       (:project cfg)
          state-dir (dirs/services-dir)
          sock      (dirs/resolve-dir :socket ctx nil)]
      (str/join "\n"
        [(str "export ENVRC_SERVICES_STATE_DIR=\"" state-dir "\"")
         (str "export ENVRC_SERVICES_SOCK=\"" sock "\"")
         (str "mkdir -p \"" state-dir "\" \"$(dirname \"" sock "\")\"")]))))

(defn services-init-body [_cfg]
  (clojure.string/join "\n"
    ["watch_file \"$ENVRC_SERVICES_STATE_DIR/running\""
     "if [[ -f \"$ENVRC_SERVICES_STATE_DIR/running\" ]]; then"
     "  export ENVRC_SERVICES_BACKEND=\"$(cat \"$ENVRC_SERVICES_STATE_DIR/running\")\""
     "fi"]))

(def plugin
  {:id "process-compose"
   :description "process-compose service supervisor"
   :requires ["process-compose"]
   :capability :service
   :events #{:service-up :service-down}
   :handles #{:services}
   :cli {:up      up-impl
         :down    down-impl
         :restart restart-impl
         :attach  attach-impl
         :logs    logs-impl
         :status  status-impl}
   ;; Supervisor task fields nest under :process-compose in user tasks
   ;; (see envrc.validate/build-task-schema). :tolerant lives on base Task —
   ;; it's capability-generic (event subscriber tolerance), not pc-specific.
   :extends
   {:tasks [:map
            [:availability    {:optional true} :any]
            [:readiness-probe {:optional true} :any]
            [:shutdown        {:optional true} :any]
            [:depends-on      {:optional true} :any]
            [:before          {:optional true} [:sequential string?]]
            [:after           {:optional true} [:sequential string?]]
            [:on-failure      {:optional true} [:sequential string?]]
            [:supervisor      {:optional true} string?]]
    :emitters {:services-env {:input :cfg :encode :raw :transform services-env-shell}}
    :use [:map
          [:process-compose {:optional true}
           [:map {:closed false}
            [:await             {:optional true} [:enum :running :ready]]
            [:await-timeout-sec {:optional true} pos-int?]]]]}
   :provides
   {:tasks
    {:services-init
     {:label "Initialize services state dir"
      :group "internal"
      :on :enter
      :tolerant true
      :run (services-init-body nil)}}}})
