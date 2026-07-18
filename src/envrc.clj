(ns envrc
  (:require [core :as c]
            [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :as p]
            [cheshire.core :as json]
            [clojure.string :as str]
            [envrc.aliases :as envrc.aliases]
            [envrc.api :as envrc.api]
            [envrc.config :as econf]
            [envrc.data :as data]
            [envrc.dirs :as envrc.dirs]
            [envrc.events :as envrc.events]
            [envrc.git :as envrc.git]
            [envrc.gen :as envrc.gen]
            [envrc.plugin :as envrc.plugin]
            [envrc.run :as envrc.run]
            [envrc.runner :as envrc.runner]
            [envrc.schemas :as envrc.schemas]
            [envrc.services :as envrc.services]
            [envrc.status :as envrc.status]))

(declare dispatch-table)

(def ^:private verb-aliases
  "Verb-name aliases resolved centrally before per-plugin lookup.
   Plugins declare canonical names only."
  {:ls          :list
   :ls-sessions :list-sessions})

(defn sha1-hex [s]
  (let [md (java.security.MessageDigest/getInstance "SHA-1")
        bs (.digest md (.getBytes (str s) "UTF-8"))]
    (apply str (map #(format "%02x" %) bs))))

(defn- project-ctx [tl]
  (:project (try (data/load-config tl) (catch Exception _ {}))))

(defn- config-json [cfg pretty?]
  (let [payload (or (:config cfg) {})]
    (if pretty?
      (json/generate-string payload {:pretty true})
      (json/generate-string payload))))

(defn- do-config [{:keys [opts]}]
  (let [cfg (data/load-config (or (:dir opts) "."))]
    (println (config-json cfg (boolean (:pretty opts))))))

(defn- toplevel []
  (let [git-out (:out (p/shell {:out :string :err :string :continue true}
                               "git" "rev-parse" "--show-toplevel"))]
    (if (str/blank? git-out) (str (fs/canonicalize ".")) (str/trim git-out))))

(defn dispatch-apply
  "Built-in `envrc apply <label> [args]`. Dispatches to every active plugin
   whose :handles set contains the label keyword."
  [cfg [label-arg & rest-args]]
  (when-not label-arg
    (throw (ex-info "envrc apply: missing <label>" {})))
  (let [label-kw (keyword label-arg)
        handlers (for [[_ plugin] (:plugins cfg)
                       :when (contains? (:handles plugin) label-kw)
                       :let  [handler (get-in plugin [:cli :apply])]
                       :when handler]
                   handler)
        handlers (vec handlers)]
    (when (empty? handlers)
      (let [available (->> (:plugins cfg) vals
                           (mapcat :handles) distinct sort)]
        (throw (ex-info (str "envrc: unknown label `" (name label-kw)
                             "`; known: " (str/join ", " (map name available)))
                        {:label label-kw :available available}))))
    (envrc.api/with-context (envrc.api/build-context cfg (toplevel))
      (doseq [h handlers]
        (h cfg {:label label-kw :args (vec rest-args)})))))

(defn watch-paths [dir]
  (let [dir           (str (fs/canonicalize (or dir ".")))
        [layer fns]   (econf/load-layer dir)
        project-files (->> (vals fns)
                           (map #(str (fs/path dir %)))
                           (filter fs/exists?))
        cfg           (econf/collapse-layer layer)
        cfg-pointer   (when (string? (:config cfg))
                        (str (fs/path dir (:config cfg))))]
    (->> (concat project-files (when (and cfg-pointer (fs/exists? cfg-pointer)) [cfg-pointer]))
         sort
         distinct
         (str/join "\n"))))

(defn- project-name [dir]
  (let [out (:out (p/shell {:dir (or dir ".") :out :string :err :string :continue true}
                           "git" "rev-parse" "--show-toplevel"))]
    (if (str/blank? out)
      (last (str/split (str (fs/canonicalize (or dir "."))) #"/"))
      (last (str/split (str/trim out) #"/")))))


(defn- build-invocation [cfg cmd-name {:keys [toplevel]} extra-args]
  (let [aliases   (get-in cfg [:use :aliases] {})
        task-name (envrc.aliases/resolve-task aliases cmd-name)
        available (concat (map name (keys aliases))
                          (map (comp first :cmds) dispatch-table)
                          (keys (or (:dispatch cfg) {})))]
    (when-not task-name
      (let [suggestion (envrc.schemas/closest-match cmd-name available)]
        (throw (ex-info (str "envrc: unknown command `" cmd-name "`"
                             (when suggestion (str "; did you mean `" suggestion "`?")))
                        {:name cmd-name
                         :available (sort (distinct available))
                         :suggestion suggestion}))))
    (let [task   (get-in cfg [:tasks task-name])
          result (envrc.run/evaluate cfg task-name task {:root toplevel})
          env    (into {} (map (fn [[k v]] [(name k) v])) (:env task))
          with-env (fn [argv]
                     (if (seq env)
                       (into ["direnv" "exec" toplevel "env"]
                             (concat (map (fn [[k v]] (str k "=" v)) env) argv))
                       (into ["direnv" "exec" toplevel] argv)))]
      (cond
        (nil? result)
        ["true"]

        (string? result)
        (with-env (concat ["bash" "-c" result "--"] extra-args))

        :else
        (with-env (concat result extra-args))))))

(defn- do-status [{:keys [args opts]}]
  (let [cfg (data/load-config (or (:dir opts) (toplevel)))
        view (first args)]
    (envrc.status/dispatch cfg view opts)))

(defn- do-name [{:keys [args]}]
  (let [tl         (toplevel)
        cfg        (data/load-config tl)
        cmd-name   (first args)
        extras     (vec (rest args))
        invocation (build-invocation cfg cmd-name {:toplevel tl} extras)]
    (System/exit (:exit (apply p/shell {:inherit true :continue true} invocation)))))


(defn- dispatch-capability [cfg cap-prefix verb-name opts args]
  (let [dispatch (:dispatch cfg)
        entry    (get dispatch cap-prefix)]
    (when-not entry
      (throw (ex-info (str "envrc: no plugin fulfills `" cap-prefix "` capability")
                      {:prefix cap-prefix})))
    (let [verb-kw     (keyword verb-name)
          resolved-kw (get verb-aliases verb-kw verb-kw)
          handler     (or (get-in entry [:verbs resolved-kw])
                          (get-in entry [:verbs verb-kw]))]
      (when-not handler
        (let [candidates (map name (keys (:verbs entry)))
              suggestion (envrc.schemas/closest-match verb-name candidates)]
          (throw (ex-info (str "envrc " cap-prefix ": unknown verb `" verb-name "`"
                               (when suggestion (str "; did you mean `" suggestion "`?")))
                          {:prefix cap-prefix :verb verb-name
                           :available (sort candidates)
                           :suggestion suggestion}))))
      (envrc.api/with-context (envrc.api/build-context cfg (toplevel))
        (handler cfg {:args (vec args) :opts (or opts {})})))))

(defn- dispatch-plugin-prefix [cfg prefix verb args]
  (let [entry (get (:dispatch cfg) prefix)]
    (if (and verb (get-in entry [:verbs (keyword verb)]))
      (dispatch-capability cfg prefix verb {} (vec args))
      (if-let [default-handler (get-in entry [:verbs :default])]
        (envrc.api/with-context (envrc.api/build-context cfg (toplevel))
          (default-handler cfg {:args (vec (cond-> args verb (cons verb)))
                                :opts {}}))
        (dispatch-capability cfg prefix verb {} (vec args))))))


(defn- do-pane [{:keys [args opts]}]
  (let [[verb & extra] args
        cfg (data/load-config (or (:dir opts) (toplevel)))]
    (dispatch-capability cfg "pane" verb opts (vec extra))))

(defn- do-ws [{:keys [args opts]}]
  (let [[verb & extra] args
        cfg (data/load-config (or (:dir opts) (toplevel)))]
    (dispatch-capability cfg "ws" verb opts (vec extra))))

(defn state-dir-for [tl]
  (or (System/getenv "ENVRC_SERVICES_STATE_DIR")
      (envrc.api/with-context {:cfg (try (data/load-config tl) (catch Exception _ {}))}
        (envrc.dirs/services-dir))))

(defn sock-path-for [tl]
  (or (System/getenv "ENVRC_SERVICES_SOCK")
      (envrc.api/with-context {:cfg (try (data/load-config tl) (catch Exception _ {}))}
        (envrc.dirs/resolve-dir :socket (project-ctx tl) nil))))

(defn pc-shell [tl & args]
  (let [sock (sock-path-for tl)]
    (apply p/shell {:inherit true :continue true
                    :extra-env {"PC_SOCKET_PATH" sock}}
           "process-compose" "-U" "-u" sock args)))

(defn pc-shell-out [tl & args]
  (let [sock (sock-path-for tl)]
    (apply p/shell {:out :string :err :string :continue true
                    :extra-env {"PC_SOCKET_PATH" sock}}
           "process-compose" "-U" "-u" sock args)))

(defn delete-marker! [tl]
  (let [marker (str (state-dir-for tl) "/running")]
    (when (.exists (clojure.java.io/file marker))
      (.delete (clojure.java.io/file marker)))))

(defn- do-gen [{:keys [args opts]}]
  (let [fmt       (or (:fmt opts) (first args))
        fmt-kw    (when fmt (keyword fmt))
        cfg       (data/load-config (or (:dir opts) (toplevel)))]
    (when-not fmt-kw
      (throw (ex-info "envrc gen: format arg required" {})))
    (envrc.gen/gen! cfg fmt-kw opts)))

(defn- do-services [{:keys [args opts]}]
  (let [[verb & extra] args
        cfg (data/load-config (or (:dir opts) (toplevel)))
        ;; If the first remaining arg is a task name with :process-compose :supervisor set, route via that plugin
        first-task (some-> (first extra) keyword)
        override   (get-in cfg [:tasks first-task :process-compose :supervisor])
        effective-prefix
        (if override
          (let [plugin-manifest (get-in cfg [:plugins override])]
            (envrc.plugin/prefix-for plugin-manifest))
          "services")]
    (dispatch-capability cfg effective-prefix verb opts (vec extra))))

(def ^:private dispatch-table
  [{:cmds ["status"]   :fn do-status
    :spec {:json {:coerce :boolean}
           :v    {:coerce :boolean}
           :vv   {:coerce :boolean}
           :show-secrets {:coerce :boolean :desc "Reveal masked env values"}
           :dir  {:desc "Project dir (default cwd)"}}}
   {:cmds ["config"]   :fn do-config
    :spec {:pretty {:coerce :boolean :desc "Pretty-print JSON"}
           :dir    {:desc "Project dir (default cwd)"}}}
   {:cmds ["pane"] :fn do-pane}
   {:cmds ["ws"] :fn do-ws}
   {:cmds ["services"] :fn do-services
    :spec {:attach {:coerce :boolean :alias :a :desc "Run services in the foreground"}}}
   {:cmds ["gen"] :fn do-gen
    :spec {:stdout {:coerce :boolean :desc "Print to stdout instead of writing file"}
           :output {:desc "Override emitter's :path destination"}
           :dir    {:desc "Project dir (default cwd)"}}
    :args->opts [:fmt]}
   {:cmds ["apply"] :fn (fn [{:keys [opts args]}]
                          (let [cfg (data/load-config (or (:dir opts) "."))]
                            (dispatch-apply cfg args)))}])

(def ^:private builtin-subcommands
  (into #{} (map (comp first :cmds) dispatch-table)))

(defn -main [& _]
  (let [argv    (vec *command-line-args*)
        cfg     (try (data/load-config (toplevel)) (catch Exception _ {}))
        aliases (or (get-in cfg [:use :aliases]) {})
        _       (envrc.aliases/validate-aliases! aliases)
        first-arg (first argv)
        dispatch-map (or (:dispatch cfg) {})
        bus     (envrc.runner/event-subscribers cfg)]
    (envrc.events/with-bus bus
      (cond
        (nil? first-arg)             (do-status {:args [] :opts {}})
        (contains? builtin-subcommands first-arg) (cli/dispatch dispatch-table argv
                                                                 {:error-fn c/dispatch-error})
        (contains? dispatch-map first-arg)
        (let [[prefix verb & rest-args] argv]
          (dispatch-plugin-prefix cfg prefix verb rest-args))
        :else                        (do-name {:args argv})))))
