(ns envrc.plugin
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [envrc.capabilities :as caps]))

(defn- load-plugin-file [path source]
  (let [base    (fs/file-name (fs/strip-ext path))
        ns-name (str "envrc.plugin." (str/replace base "_" "-"))]
    (load-file (str path))
    (when-let [v (resolve (symbol ns-name "plugin"))]
      (-> @v
          (assoc :source source
                 :file   (str path)
                 :id     (or (:id @v) (str base)))))))


(defn- scan-dir [dir source]
  (if (and dir (fs/directory? dir))
    (->> (fs/list-dir dir "*.clj")
         (map #(load-plugin-file % source))
         (remove nil?))
    []))
(defn- normalize-roots [roots]
  (cond
    (nil? roots)        []
    (string? roots)     (if (str/blank? roots) [] [roots])
    (sequential? roots) (->> roots
                             (filter string?)
                             (remove str/blank?)
                             vec)
    :else (throw (ex-info "plugin roots must be nil, a string, or a sequence of strings"
                          {:roots roots}))))

(defn- scan-roots [roots source]
  (mapcat #(scan-dir % source) (normalize-roots roots)))

(def capability->prefix
  {:pane "pane" :workspace "ws" :service "services"})

(defn- disabled-by-use?
  "Plugin is disabled iff the user explicitly set :use {<id> false}."
  [use-cfg id]
  (false? (get use-cfg (keyword id))))

(defn discover
  "Returns {plugin-id manifest+source}. Project plugins override global on id collision.
   Plugins where the user set :use {<id> false} in cfg are dropped before
   capability conflict detection — this is how panes-supervisor coexists on disk
   with process-compose without the discovery hard-erroring at load."
  ([roots] (discover roots {}))
  ([{:keys [global project]} use-cfg]
   (let [global-ps  (scan-roots global "global")
         project-ps (scan-roots project "project")
         by-id (fn [ps] (into {} (map (juxt :id identity)) ps))
         all-merged (merge (by-id global-ps) (by-id project-ps))
         merged     (into {}
                          (remove (fn [[id _]] (disabled-by-use? use-cfg id)))
                          all-merged)]
     (doseq [cap-key (keys capability->prefix)]
       (let [holders (filter #(= cap-key (:capability %)) (vals merged))]
         (when (> (count holders) 1)
           (throw (ex-info (str "capability " cap-key " fulfilled by both: "
                                (str/join ", " (map :file holders))
                                "  (disable one via homeModules.envrc.use.<id> = false)")
                           {:capability cap-key :plugins (map :file holders)})))))
     (doseq [[_ m] merged]
       (caps/check-contract! m))
     merged)))

(defn- env-plugin-paths [env-name]
  (let [v (System/getenv env-name)]
    (if (str/blank? v)
      []
      (->> (str/split v #":")
           (remove str/blank?)
           vec))))

(defn default-roots
  "Resolves discovery roots from packaged defaults, user overrides, and the project."
  [project-dir]
  {:global  (into []
                   (concat
                    (env-plugin-paths "ENVRC_DEFAULT_PLUGIN_PATH")
                    (env-plugin-paths "ENVRC_PLUGIN_PATH")
                    [(str (or (System/getenv "XDG_CONFIG_HOME")
                              (str (System/getenv "HOME") "/.config"))
                          "/envrc")]))
   :project [(str project-dir "/.envrc")]})

(defn prefix-for [{:keys [capability id]}]
  (or (capability->prefix capability) id))

(defn find-by-capability
  "Returns dispatch entry {:plugin manifest :verbs cli-map} for the loaded
   plugin whose :capability matches cap, or nil. Capability uniqueness is
   already enforced at discovery time."
  [dispatch cap]
  (some (fn [[_ entry]]
          (when (= cap (get-in entry [:plugin :capability]))
            entry))
        dispatch))

(defn build-dispatch
  "Returns {prefix {:plugin manifest :verbs {verb-name handler}}}.
   Errors on prefix collision with capability prefixes."
  [plugins capability-prefix-table]
  (let [cap-prefixes (set (vals capability-prefix-table))]
    (reduce
     (fn [acc [_id m]]
       (let [prefix (prefix-for m)]
         (when (and (nil? (:capability m)) (cap-prefixes prefix))
           (throw (ex-info (str "plugin " (:id m) " id collides with capability prefix `" prefix "`")
                           {:plugin (:id m) :prefix prefix})))
         (when (contains? acc prefix)
           (throw (ex-info (str "two plugins claim prefix `" prefix "`: "
                                (:file (get-in acc [prefix :plugin])) " and " (:file m))
                           {:prefix prefix})))
         (assoc acc prefix {:plugin m :verbs (:cli m {})})))
     {}
     plugins)))
