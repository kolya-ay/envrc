(ns envrc.services
  (:require [clojure.string :as str]
            [cheshire.core :as json]
            [clj-yaml.core :as yaml]
            [envrc.run :as run]
            [babashka.fs :as fs]
            [babashka.process :as p]))

(defn run->body
  [cfg svc-name spec]
  (run/shell-body cfg svc-name spec {:root (System/getProperty "user.dir")}
                  {:require-command true :surface :service}))

(defn service-tasks
  "Extract service-flagged tasks from the unified :tasks map."
  [cfg]
  (->> (:tasks cfg)
       (filter (fn [[_ t]] (true? (:service t))))
       (into {})))

(defn- kebab->snake [k]
  (when k (str/replace (name k) #"-" "_")))

(defn- transform-key [k]
  (-> (case k :timeout-sec :timeout_seconds k)
      kebab->snake
      keyword))

(defn- transform-keys [m]
  (cond
    (map? m)    (into {} (map (fn [[k v]]
                                (let [k' (transform-key k)
                                      v' (if (= k' :depends_on)
                                           ;; preserve inner service-name keys verbatim
                                           v
                                           (transform-keys v))]
                                  [k' v']))) m)
    (vector? m) (mapv transform-keys m)
    :else       m))

(defn- env-map->list
  "process-compose's `environment` is a list of KEY=VALUE strings, not a map."
  [env]
  (->> env
       (sort-by key)
       (mapv (fn [[k v]] (str (name k) "=" v)))))

(defn- wrap-unset-command [body unset]
  (run/wrap-unset-shell-command body unset))

(defn- service->pc-process [cfg svc-name spec]
  (let [body  (run->body cfg svc-name spec)
        ;; Supervisor / passthrough fields nest under :process-compose;
        ;; process-compose passes through any unknown YAML keys, so ad-hoc
        ;; fields under :process-compose flow straight to the YAML output.
        pc    (:process-compose spec)
        {:keys [set unset]} (run/task-env-resolution cfg spec)
        base  {:command (wrap-unset-command body unset)}]
    (cond-> (merge base (transform-keys pc))
      (seq set) (assoc :environment (env-map->list set)))))

(defn transpile
  "Pure: cfg -> {:yaml string}. Reads :service-flagged entries
   from cfg's unified :tasks map."
  [cfg]
  (let [services (service-tasks cfg)
        processes (into (sorted-map)
                        (map (fn [[k v]] [k (service->pc-process cfg k v)]))
                        services)]
    {:yaml (yaml/generate-string {:processes processes} {:dumper-options {:flow-style :block}})}))

(defn up-pc
  "Side-effecting: write YAML into state-dir, then launch process-compose.
   opts = {:state-dir str :sock-path str :extra-args vec :attach? bool}.
   :attach? true  → foreground (blocks; Ctrl-c flows through).
   :attach? false → detached `-D` launch (returns once the supervisor forks).
   No .env is written — service env is inline in the YAML."
  [cfg {:keys [state-dir sock-path extra-args attach?]}]
  (fs/create-dirs state-dir)
  (fs/create-dirs (fs/parent sock-path))
  (let [{:keys [yaml]} (transpile cfg)
        yaml-path (str state-dir "/process-compose.yml")
        marker    (str state-dir "/running")]
    (spit yaml-path yaml)
    (spit marker "process-compose\n")
    (let [base ["process-compose" "-f" yaml-path "-U" "-u" sock-path "up"]
          args (cond-> base
                 (not attach?) (conj "-D")
                 true          (into (or extra-args [])))
          opts {:inherit true :continue true
                :extra-env {"PC_SOCKET_PATH" sock-path}}]
      (apply p/shell opts args))))

(defn wrap-for-restart [body restart]
  (case restart
    "always"     (str "while true; do " body "; sleep 1; done")
    "on_failure" (str "until " body "; do sleep 1; done")
    body))

(defn topo-order
  "Kahn's algorithm over :process-compose.depends-on graph. Throws on cycle."
  [services]
  (let [deps (into {} (map (fn [[k v]]
                             [k (set (keys (get-in v [:process-compose :depends-on])))])
                           services))
        all  (set (keys services))
        step (fn step [resolved remaining]
               (if (empty? remaining)
                 resolved
                 (let [ready (filter (fn [k]
                                       (every? (set resolved) (deps k)))
                                     remaining)]
                   (if (empty? ready)
                     (throw (ex-info "envrc services: dependency cycle" {:remaining remaining}))
                     (recur (into resolved (sort-by name ready))
                            (remove (set ready) remaining))))))]
    (vec (step [] (sort-by name all)))))

(defn poll-readiness! [{:keys [readiness-probe]}]
  (when-let [exec-cmd (get-in readiness-probe [:exec :command])]
    (let [deadline (+ (System/currentTimeMillis)
                      (* 1000 (or (:timeout-sec readiness-probe) 60)))
          interval (* 1000 (or (:period-sec readiness-probe) 2))]
      (loop []
        (let [{:keys [exit]} (p/shell {:continue true :out :string :err :string}
                                      "bash" "-c" exec-cmd)]
          (cond
            (zero? exit) :ready
            (> (System/currentTimeMillis) deadline) (throw (ex-info "readiness timeout" {}))
            :else (do (Thread/sleep interval) (recur))))))))


(defn parse-process-list
  "Parse `process-compose process list -o json` into normalized records.
   Field names verified against process-compose's JSON: name/status/pid/
   is_ready/restarts/age."
  [json-str]
  (->> (json/parse-string json-str true)
       (mapv (fn [p]
               {:name     (:name p)
                :status   (:status p)
                :pid      (:pid p)
                :is-ready (:is_ready p)
                :restarts (:restarts p)
                :age      (:age p)}))))

(defn proc-ready?
  "A single process meets the standard. :running → status Running.
   :ready → if it declares a probe, require is-ready Ready; else just Running.
   Public: the plugin's up-impl calls it per service for the ✓/✗ verdict."
  [{:keys [status is-ready has-probe]} standard]
  (and (= "Running" status)
       (or (= :running standard)
           (not has-probe)
           (= "Ready" is-ready))))

(defn all-ready?
  "All processes meet the readiness standard (:running | :ready)."
  [procs standard]
  (boolean (and (seq procs) (every? #(proc-ready? % standard) procs))))

(defn service-line
  "One framed status line for a service. ok? selects ✓/✗."
  [{:keys [name status pid url]} ok?]
  (str "  " (if ok? "✓" "✗") " " name
       (when url (str "  → " url))
       (when (and ok? pid) (str "  (pid " pid ")"))
       (when-not ok? (str "  (status: " status ")"))))
