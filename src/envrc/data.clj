(ns envrc.data
  "Canonical envrc config loader. Lifted from konsole.config in V2 sub-project #1."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [envrc.aliases :as envrc.aliases]
            [envrc.config :as econf]
            [envrc.dirs :as envrc.dirs]
            [envrc.git :as envrc.git]
            [envrc.plugin :as envrc.plugin]
            [envrc.provides :as envrc.provides]
            [envrc.schemas :as envrc.schemas]
            [envrc.validate :as envrc.validate]))

(def defaults {})

(defn merge-files
  "Concat per label across global + project; dedup per label preserving order."
  [global project]
  (let [ks (into #{} (concat (keys global) (keys project)))]
    (into {}
          (for [k ks]
            [k (vec (distinct (concat (get global k []) (get project k []))))]))))


(defn- validate-single-file!
  "Errors if any non-:config top-level key appears in more than one format.
   `filenames` is a {:fmt actual-filename} map returned by load-layer."
  [layer filenames layer-label]
  (let [key->fmts (->> layer
                       (mapcat (fn [[fmt cfg]]
                                 (for [k (keys cfg) :when (not= k :config)]
                                   [k fmt])))
                       (reduce (fn [m [k fmt]] (update m k (fnil conj #{}) fmt)) {}))]
    (doseq [[k fmts] key->fmts :when (> (count fmts) 1)]
      (let [sorted-fmts (sort-by {:json 0 :yaml 1 :edn 2} fmts)
            names (->> sorted-fmts (map filenames) (str/join " and "))]
        (throw (ex-info (str layer-label " envrc: " k " appears in both " names " — pick one")
                        {:key k :files (mapv filenames sorted-fmts)}))))))

(defn- deep-merge
  "Recursive merge for maps. Non-maps follow `merge` semantics (rhs wins).
   Nil values are treated as empty maps."
  [& maps]
  (let [maps (remove nil? maps)]
    (if (every? map? maps)
      (apply merge-with deep-merge maps)
      (last maps))))


(defn- as-keyword [v]
  (if (string? v)
    (keyword (if (str/starts-with? v ":") (subs v 1) v))
    v))

(defn- ->ref [v]
  (if (and (string? v) (str/starts-with? v ":"))
    (keyword (subs v 1))
    v))

(defn normalize-files-label-refs
  "Inside :files values, convert string elements starting with `:` to keywords.
   JSON/YAML/TOML carry keywords as strings; EDN passes through.
   A scalar value (keyword or string) is sugared to a one-element vector, so
   `:files {:local :taskmd}` and `:files {:dot-local \"*.local.*\"}` both work.
   No-op when :files is absent."
  [cfg]
  (if-let [files-dict (:files cfg)]
    (assoc cfg :files
           (into {} (for [[k v] files-dict]
                      [k (mapv ->ref (if (sequential? v) v [v]))])))
    cfg))

(defn- normalize-tokens [cfg]
  (cond-> cfg
    (:tasks cfg)
    (update :tasks
            (fn [cmds]
              (into {}
                    (map (fn [[k cmd]]
                           [k (cond-> cmd
                                (get-in cmd [:show :pane])
                                (update-in [:show :pane] as-keyword))])
                         cmds))))

    (vector? (get-in cfg [:use :worktree :dirty]))
    (update-in [:use :worktree :dirty]
               (fn [v] (into #{} (map as-keyword) v)))

    true normalize-files-label-refs))



(defn- warn-global-collisions! [layer filenames]
  "Emit a warning to stderr if any key appears in multiple formats in the global layer.
   Format precedence: json > yaml > edn (first-present-wins after collapse)."
  (let [key->fmts (->> layer
                       (mapcat (fn [[fmt cfg]]
                                 (for [k (keys cfg) :when (not= k :config)]
                                   [k fmt])))
                       (reduce (fn [m [k fmt]] (update m k (fnil conj #{}) fmt)) {}))]
    (doseq [[k fmts] key->fmts :when (> (count fmts) 1)]
      (let [names (->> fmts sort (map filenames) (str/join " and "))
            winner (first (sort-by {:json 0 :yaml 1 :edn 2} fmts))]
        (binding [*out* *err*]
          (println (str "envrc warning: global key " k " in " names
                        " — using " (filenames winner) " (precedence wins)")))))))

(defn load-global []
  (let [dir         (econf/xdg-config-dir)
        [layer fns] (econf/load-layer dir)]
    ;; Skip validate-single-file! on the global layer: a user migrating from
    ;; ~/.config/envrc.edn to the Nix-generated ~/.config/envrc.json can have
    ;; the same key (e.g. :packages) in both files during the transition.
    ;; But warn about duplicates so users know they have conflicting config.
    (warn-global-collisions! layer fns)
    [dir (normalize-tokens (econf/collapse-layer layer))]))

(defn check-use-plugin-ids!
  "Errors when :use references an unknown plugin id, suggesting the closest match.
   Skipped entries:
   - false / nil values (disabling / removing shorthand)
   - the built-in `:aliases` key (consumed by the runner, not a plugin)"
  [cfg plugins]
  (let [known (set (map (comp name key) plugins))]
    (doseq [[k v] (:use cfg)
            :let [id (name k)]
            :when (and (not (false? v))
                       (not (nil? v))
                       (not (contains? known id))
                       (not= id "aliases"))]
      (let [suggestion (envrc.schemas/closest-match id known)]
        (throw (ex-info (str "envrc: unknown plugin `" id "`"
                             (when suggestion (str "; did you mean `" suggestion "`?")))
                        {:reason :unknown-plugin :key k :suggestion suggestion}))))))

(defn load-project [project-dir]
  (let [project-dir       (str (fs/canonicalize project-dir))
        [layer filenames] (econf/load-layer project-dir)]
    (validate-single-file! layer filenames (str project-dir "/"))
    [project-dir (normalize-tokens (econf/collapse-layer layer))]))

(defn- validate-alias-targets [cfg]
  (let [aliases (get-in cfg [:use :aliases] {})
        tasks (:tasks cfg {})
        reserved-verbs (into envrc.aliases/core-flat-verbs
                             (map keyword (keys (:dispatch cfg))))]
    (envrc.aliases/validate-aliases! aliases reserved-verbs)
    (doseq [[alias task-name] aliases]
      (let [task (get tasks task-name)]
        (when-not task
          (throw (ex-info (str "envrc: alias " alias " points to unknown task " task-name)
                          {:alias alias :task task-name})))
        (when-not (contains? task :run)
          (throw (ex-info (str "envrc: alias " alias " points to task " task-name " without :run")
                          {:alias alias :task task-name})))))
    cfg))

(defn load-config [project-dir]
  "Load and merge global + project envrc config.

  Merge semantics for top-level keys:
  - :packages — concatenated: global items then project items
  - :env — deep-merged; project keys win on collision. To suppress a global env var,
    set its value to nil in the project's :env. There is no wholesale replace mode.
  - :config — deep-merged across formats (edn > yaml > json per format,
    then global < project precedence)
  - Other keys (:tasks) — project layer replaces global wholesale using
    merge semantics (rhs wins)"
  (let [[global-dir  global-cfg]  (load-global)
        [project-d   project-cfg] (load-project project-dir)
        ;; Deep merge :use across layers; project leaves win on collision.
        ;; A nil value for a plugin key in the project removes it from the active set.
        ;; Computed early because plugin discovery needs it to drop disabled plugins
        ;; before capability-conflict detection.
        use-cfg (let [g  (:use global-cfg)
                      p  (:use project-cfg)
                      ks (into #{} (concat (keys g) (keys p)))
                      m  (into {}
                               (map (fn [k]
                                      (if (and (contains? p k) (nil? (get p k)))
                                        [k nil]
                                        [k (deep-merge (get g k) (get p k))])))
                               ks)]
                  (into {} (remove (fn [[_ v]] (nil? v))) m))
        ;; Discover plugins first — their manifests feed the effective schema.
        roots    (envrc.plugin/default-roots project-d)
        plugins  (envrc.plugin/discover roots use-cfg)
        ;; Validate every plugin manifest against the base PluginManifest schema
        ;; (catches malformed :extends / :provides shapes early).
        _ (doseq [[_id m] plugins]
            (envrc.validate/validate! envrc.schemas/PluginManifest m
                                      :reason :invalid-plugin-manifest))
        ;; Closest-match check on :use keys against the active plugin set —
        ;; typos like `:procees-compose` get caught here with a suggestion.
        ;; Only project-level keys checked: the global layer (`~/.config/envrc.*`)
        ;; may carry transitional / cross-tool keys that lack a local plugin.
        _ (check-use-plugin-ids! {:use (:use project-cfg)} plugins)
        ;; Build effective schemas with plugin vocabulary folded in.
        effective (envrc.validate/build-effective-schemas plugins)
        ;; Validate the user-authored project config against the effective
        ;; EnvrcEdn schema. The global layer is left alone — it's user-personal
        ;; and may carry transitional keys.
        _ (envrc.validate/validate!
            (:EnvrcEdn effective)
            project-cfg
            :reason :unknown-key
            :allowed-keys
            (fn [in]
              (cond
                ;; Bad key sits at the top level: [:foo]
                (= 1 (count in))
                (vec (envrc.schemas/reserved-top-level-keys))
                ;; Bad task field: [:tasks <task-name> <field>]
                (and (= 3 (count in)) (= :tasks (first in)))
                (vec (envrc.schemas/reserved-task-fields))
                :else
                (vec (envrc.schemas/reserved-top-level-keys)))))
        ;; Per-plugin :extends.use slot validation — only when the plugin
        ;; declares a schema for its slot AND the user provided a non-shorthand
        ;; (i.e. a map) value. Shorthand values (true/false/string) bypass.
        _ (doseq [[id m] plugins
                  :let [schema   (get-in m [:extends :use])
                        user-cfg (get-in project-cfg [:use (keyword id)])]
                  :when (and schema
                             user-cfg
                             (not (true? user-cfg))
                             (not (false? user-cfg))
                             (not (string? user-cfg)))]
            (envrc.validate/validate! schema user-cfg
                                      :reason :invalid-use-slot))
        ;; Re-load raw layers locally so we can see string pointers
        ;; that collapse-layer's deep-merge would discard.
        [global-layer  _]  (econf/load-layer global-dir)
        [project-layer _]  (econf/load-layer project-d)
        resolved-config (econf/resolve-all
                          (concat
                            ;; Extract [dir raw-:config-value] pairs from global layer,

                            ;; one per format, in low→high precedence order (json → yaml → edn)
                            (->> [:json :yaml :edn]
                                 (keep (fn [fmt]
                                         (when-let [v (get-in global-layer [fmt :config])]
                                           [global-dir v])))
                                 vec)
                            ;; Same for project layer
                            (->> [:json :yaml :edn]
                                 (keep (fn [fmt]
                                         (when-let [v (get-in project-layer [fmt :config])]
                                           [project-d v])))
                                 vec)))
        packages   (into (vec (:packages  global-cfg)) (:packages  project-cfg))
        ;; Deep merge :env; project keys win on collision. Project cannot suppress
        ;; globals wholesale—set individual keys to nil to suppress them.
        env        (merge (:env global-cfg) (:env project-cfg))
        tasks      (merge (:tasks defaults)
                          (:tasks global-cfg)
                          (:tasks project-cfg))
        files      (merge-files (:files global-cfg) (:files project-cfg))
        merged (-> (merge defaults
                          (dissoc global-cfg :config)
                          (dissoc project-cfg :config))
                   (assoc :tasks      tasks)
                   (assoc :packages   packages)
                   (assoc :env        env)
                   (assoc :use        use-cfg)
                   (assoc :files      files))
        ;; Fold active plugins' :provides into merged :tasks and :files.
        ;; Project entries already in `merged` override plugin contributions
        ;; on name collision; cross-plugin collisions hard-error in collect-provides.
        gathered (envrc.provides/collect-provides plugins merged)
        merged   (envrc.provides/merge-into-cfg merged gathered)
        dispatch (envrc.plugin/build-dispatch plugins envrc.plugin/capability->prefix)
        layers       {:global  {:tasks (count (:tasks global-cfg))
                                 :files (count (:files global-cfg))
                                 :use   (count (:use   global-cfg))}
                      :project {:tasks (count (:tasks project-cfg))
                                :files (count (:files project-cfg))
                                :use   (count (:use   project-cfg))}}
        with-plugins (assoc merged :plugins plugins :dispatch dispatch :layers layers)
        ;; resolve-entry lives in envrc.project, which already requires this ns
        ;; (envrc.data); use requiring-resolve to break the load cycle.
        resolve-entry @(requiring-resolve 'envrc.project/resolve-entry)
        project-info (let [tl        (str (fs/canonicalize project-dir))
                           cfg-title (or (:title project-cfg)
                                         (:title with-plugins))
                           scopes    (get-in with-plugins [:use :scopes])
                           entry     (resolve-entry
                                       tl
                                       {:cfg-title cfg-title :slug-arg nil
                                        :scopes (or scopes {:default :ego
                                                            :entries {}})})
                           ws        (envrc.dirs/workspace-name
                                       {:branch-fn      (fn [] (envrc.git/branch tl))
                                        :main-branch-fn (fn [] (envrc.git/main-branch tl))})]
                       {:slug      (:slug entry)
                        :title     (:title entry)
                        :scope     (keyword (:scope entry))
                        :workspace ws})
        with-plugins (assoc with-plugins :project project-info)]
    (-> with-plugins
        validate-alias-targets
        (cond-> (seq resolved-config) (assoc :config resolved-config))
        normalize-tokens)))
