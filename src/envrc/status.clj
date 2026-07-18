(ns envrc.status
  (:require [cheshire.core :as json]
            [clj-commons.ansi :refer [compose]]
            [clojure.set]
            [clojure.string :as str]
            [envrc.api :as envrc.api]
            [envrc.schemas]
            [babashka.process :as p]))

(defn- service-count [cfg]
  (count (filter (fn [[_ t]] (true? (:service t))) (:tasks cfg))))

(defn- verbosity-level [opts]
  (cond (:vv opts) :vv
        (:v opts)  :v
        :else      :default))

(defn- meets-level? [view-level section-level]
  ;; section runs if its declared level ≤ current view verbosity:
  ;; :default always runs; :v runs at :v or :vv; :vv only at :vv.
  (case section-level
    :default true
    :v       (#{:v :vv} view-level)
    :vv      (= :vv view-level)
    false))

(defn- render-plugin-sections [cfg opts]
  (let [view-level (verbosity-level opts)]
    (doseq [[_id m] (:plugins cfg)
            [_section-name section] (:status m {})
            :when (meets-level? view-level (:level section :default))
            :when (:human section)]
      ((:human section) cfg {}))))

(defn- layer-suffix [cfg key]
  (let [g (get-in cfg [:layers :global key])
        p (get-in cfg [:layers :project key])]
    (when (and g p (or (pos? g) (pos? p)))
      (str "  (global: " g ", project: " p ")"))))

;; --- friendly menu renderer (lifted from envrc.menu) ---

(defn- pad [s w]
  (str s (apply str (repeat (max 0 (- w (count s))) " "))))

(def ^:private secret-key-re #"(?i)KEY|TOKEN|SECRET|PASSWORD")

(defn mask-secret
  "Mask a value when its key looks sensitive, unless show-secrets is true.
   Sensitive → first 4 chars + ***."
  [k v show-secrets?]
  (let [s (str v)]
    (if (and (not show-secrets?) (re-find secret-key-re (name k)))
      (str (subs s 0 (min 4 (count s))) "***")
      s)))

(defn- public-commands [cfg]
  (into {}
        (keep (fn [[alias task-name]]
                (when-let [task (get-in cfg [:tasks task-name])]
                  [alias task])))
        (get-in cfg [:use :aliases] {})))

(defn- group-by-category [commands]
  (->> commands
       (group-by (fn [[_ v]] (or (:group v) "general")))
       (sort-by (fn [[cat _]] (if (= cat "general") "" cat)))))

(defn- any-autostart? [commands]
  (some (fn [[_ v]] (some? (:show v))) commands))

(defn- render-cmd-line [name-w [k v]]
  (let [n (name k)
        styled-name (if (some? (:show v))
                      (compose [:bold.green (pad n name-w)])
                      (compose [:bold (pad n name-w)]))]
    (str "  " styled-name " - " (or (:label v) ""))))

(defn- render-category [name-w [cat cmds]]
  (let [label (if (= cat "general") "general commands" cat)
        header (compose [:bold.cyan (str "[" label "]")])]
    (str header "\n\n"
         (str/join "\n"
                   (for [entry (sort-by (comp name first) cmds)]
                     (render-cmd-line name-w entry))))))

(defn- render-friendly-overview [cfg project-name]
  (let [commands     (public-commands cfg)
        with-builtin (assoc commands :envrc {:label "print this menu"
                                             :group "general"})
        groups       (group-by-category with-builtin)
        all-entries  (mapcat second groups)
        name-w       (apply max 0 (map (comp count name first) all-entries))
        welcome      (compose [:bold "🔨 Welcome to "] [:bold.cyan project-name])
        legend       (when (any-autostart? commands)
                       (compose [:faint "  (autostart commands are highlighted)"]))
        parts        (cond-> [welcome]
                       legend (conj legend)
                       true   (into (map #(render-category name-w %) groups)))]
    (str/join "\n\n" parts)))

(defn- project-name-from-env []
  (or (System/getenv "ENVRC_PROJECT_SLUG") "envrc"))

(defn- render-essential-overview [cfg opts]
  (let [verbose? (#{:v :vv} (verbosity-level opts))]
    (if (:json opts)
      (println (json/generate-string
                 (cond-> {:tasks    (count (:tasks cfg))
                          :plugins  (count (:plugins cfg))
                          :services (service-count cfg)}
                   verbose? (assoc :layers (:layers cfg)))))
      (do
        (println (render-friendly-overview cfg (project-name-from-env)))
        (when verbose?
          (println)
          (println (str "tasks: "    (count (:tasks cfg))
                        (layer-suffix cfg :tasks)))
          (println (str "files: "    (count (:files cfg))
                        (layer-suffix cfg :files)))
          (println (str "plugins: "  (count (:plugins cfg))))
          (println (str "services: " (service-count cfg)))
          (println (str "use: "      (count (:use cfg))
                        (layer-suffix cfg :use))))
        (render-plugin-sections cfg opts)))))

(defn- status-root []
  (or (System/getenv "PWD") "."))

(defn overview [cfg opts]
  (render-essential-overview cfg opts)
  (envrc.api/with-context (envrc.api/build-context cfg (status-root))
    (doseq [[_ plugin] (:plugins cfg)
            label      (:handles plugin)
            :let       [h (get-in plugin [:cli :status])]
            :when      h]
      (h cfg {:label label :brief? true}))))

(defn tasks [cfg opts]
  (let [ts (sort-by key (:tasks cfg))]
    (if (:json opts)
      (println (json/generate-string (into {} ts)))
      (doseq [[k v] ts]
        (println (str "  " (name k)
                      (when (:label v) (str " — " (:label v)))
                      (when (:group v) (str " [" (:group v) "]"))))
        (doseq [[ek ev] (sort-by key (:env v))]
          (println (str "      " (name ek) "=" (mask-secret ek ev (boolean (:show-secrets opts))))))))))

(defn env
  "status env [--show-secrets] — print top-level :env, secrets masked."
  [cfg opts]
  (let [m    (into (sorted-map) (:env cfg))
        slug (get-in cfg [:project :slug] "project")
        show (boolean (:show-secrets opts))]
    (if (:json opts)
      (println (json/generate-string
                 (into {} (map (fn [[k v]] [k (mask-secret k v show)]) m))))
      (do
        (println (str "Env for " slug " (" (count m) " keys):"))
        (let [w (apply max 0 (map (comp count name key) m))]
          (doseq [[k v] m]
            (println (str "  " (pad (name k) w) "  " (mask-secret k v show)))))))))

(defn plugins [cfg opts]
  (let [ps (sort-by key (:plugins cfg))]
    (if (:json opts)
      (println (json/generate-string (into {} ps)))
      (doseq [[id m] ps]
        (println (str "  " id
                      (when (:description m) (str " — " (:description m)))
                      (when-let [c (:capability m)]
                        (str "  cap: " c))))))))

(defn validate [cfg _opts]
  ;; The loader already validated; this view re-prints validation state for human eyes.
  (println "validate: ok")
  (when-let [warnings (:warnings cfg)]
    (doseq [w warnings] (println "warn:" w))))

(defn watch [_cfg _opts]
  ;; Defer to existing envrc/watch-paths fn at runtime (avoid circular ns require).
  (let [watch-fn (requiring-resolve 'envrc/watch-paths)]
    (println (watch-fn (System/getProperty "user.dir")))))

(defn doctor [cfg _opts]
  (doseq [[id m] (:plugins cfg)
          req    (:requires m [])]
    (let [{:keys [exit]} (p/shell {:out :string :err :string :continue true}
                                  "command" "-v" req)]
      (when-not (zero? exit)
        (println (str "✗ plugin " id " requires `" req "` (not in PATH)")))))
  (println "doctor: done"))

(defn- task-on-set [task]
  (let [on (:on task)]
    (cond
      (nil? on)        #{}
      (keyword? on)    #{on}
      (sequential? on) (set on))))

(defn- known-events-from-cfg [cfg]
  (let [task-events (reduce clojure.set/union #{}
                            (map task-on-set (vals (:tasks cfg))))
        plugin-events (reduce clojure.set/union #{}
                              (map (fn [[_ m]] (or (:events m) #{}))
                                   (:plugins cfg)))]
    (clojure.set/union task-events plugin-events
                       envrc.schemas/core-events)))

(defn events [cfg opts]
  (let [evs (sort (known-events-from-cfg cfg))
        by-event (into {}
                       (for [ev evs]
                         [ev (->> (:tasks cfg)
                                  (filter (fn [[_ t]] (contains? (task-on-set t) ev)))
                                  (map (comp name key))
                                  sort
                                  vec)]))]
    (if (:json opts)
      (println (json/generate-string {:events (into (sorted-map) by-event)}))
      (doseq [ev evs
              :let [names (get by-event ev)]]
        (println (str "  " ev
                      "  " (count names) " task" (if (= 1 (count names)) "" "s")
                      (when (seq names)
                        (str "   (" (str/join ", " names) ")"))))))))

(def views
  {"tasks" tasks "plugins" plugins "validate" validate
   "watch" watch "doctor" doctor "events" events "env" env})

(def built-in-views (set (keys views)))

(defn- dispatch-built-in-view [cfg view opts]
  ((views view) cfg opts))

(defn dispatch-label-or-view
  "Called from envrc's status entrypoint when an arg is supplied.
   Built-in view names dispatch to the existing per-view fn.
   Otherwise, treat arg as a label and call each handling plugin's
   :cli {:status} with {:label kw :brief? false}."
  [cfg arg opts]
  (if (contains? built-in-views arg)
    (dispatch-built-in-view cfg arg opts)
    (let [label-kw (keyword arg)
          handlers (for [[_ plugin] (:plugins cfg)
                         :when (contains? (:handles plugin) label-kw)
                         :let  [h (get-in plugin [:cli :status])]
                         :when h]
                     h)
          handlers (vec handlers)]
      (if (seq handlers)
        (envrc.api/with-context (envrc.api/build-context cfg (status-root))
          (doseq [h handlers] (h cfg {:label label-kw :brief? false})))
        (let [known (concat built-in-views
                            (->> (:plugins cfg) vals (mapcat :handles) distinct (map name) sort))]
          (throw (ex-info (str "envrc status: unknown view/label `" arg
                               "`; known: " (str/join ", " known))
                          {:arg arg :available known})))))))

(defn dispatch [cfg view opts]
  (if view
    (dispatch-label-or-view cfg view opts)
    (overview cfg opts)))
