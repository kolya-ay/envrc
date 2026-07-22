(ns envrc.plugin.ports
  "Deterministic per-worktree port assignment.
   Opt-in via :use {:ports {…}}; emits `export` lines that direnv sources via
   `eval \"$(envrc gen ports --stdout)\"` in stdlib.sh. Pure logic lives in
   envrc.ports."
  (:require [clojure.string :as str]
            [envrc.ports :as ports]))

(def PortsConfig
  "Schema for the :use :ports slot value. Matches the convention used by
   the ref, konsole, notifier, worktree plugins: :extends :use describes the
   plugin's own slot, not a wrapping {:plugin-id …} map."
  [:map {:closed true}
   [:base   pos-int?]
   [:stride {:optional true} pos-int?]
   [:vars   [:vector {:min 1} keyword?]]
   [:offset {:optional true} [:int {:min 0 :max 99}]]])

(defn env-offset
  "Parse and range-check ENVRC_PORT_OFFSET. nil/blank ⇒ nil; bad value raises.
   Public so plugin tests can with-redefs the env-read seam."
  []
  (let [raw (System/getenv "ENVRC_PORT_OFFSET")]
    (when-not (str/blank? raw)
      (let [n (try (Integer/parseInt (str/trim raw))
                   (catch Exception _
                     (throw (ex-info (str "envrc.ports: ENVRC_PORT_OFFSET is not an integer: "
                                          (pr-str raw))
                                     {:value raw}))))]
        (when-not (<= 0 n 99)
          (throw (ex-info (str "envrc.ports: ENVRC_PORT_OFFSET out of range 0..99: " n)
                          {:value n})))
        n))))

(defn- offset-for
  "Precedence: ENVRC_PORT_OFFSET env > config :offset > md5 hash of workspace."
  [cfg]
  (let [env-off (env-offset)
        cfg-off (get-in cfg [:use :ports :offset])
        handle  (get-in cfg [:project :workspace] "main")]
    (cond
      (some? env-off) env-off
      (some? cfg-off) cfg-off
      :else           (ports/offset handle))))

(defn resolved-ports
  "Resolved deterministic ports for status and shell emitters."
  [cfg]
  (when-let [p (get-in cfg [:use :ports])]
    (let [off (offset-for cfg)]
      {:offset off
       :pairs  (ports/build-exports p off)})))

(defn ports-shell
  "Emitter transform. Returns `\"\"` when the project hasn't opted in."
  [cfg]
  (if-let [{:keys [pairs]} (resolved-ports cfg)]
    (ports/shell-lines pairs)
    ""))

(defn status-impl [cfg {:keys [brief?]}]
  (if-let [{:keys [offset pairs]} (resolved-ports cfg)]
    (if brief?
      (println (str "  ports — offset=" offset "; "
                    (str/join "; " (map (fn [[k v]] (str (name k) "=" v)) pairs))))
      (do
        (println (str "Ports for " (get-in cfg [:project :workspace] "main")
                      " (offset " offset "):"))
        (doseq [[k v] pairs]
          (println (format "  %-20s %s" (name k) v)))))
    (when-not brief?
      (println "Ports: not configured."))))

(def plugin
  {:id          "ports"
   :description "Deterministic per-worktree port assignment via workspace-name hash"
   :handles     #{:ports}
   :cli         {:status status-impl}
   :extends     {:use      PortsConfig
                 :emitters {:ports {:input :cfg
                                    :encode :raw
                                    :transform ports-shell}}}})
