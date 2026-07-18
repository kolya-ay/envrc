(ns envrc.gen
  "Generator dispatcher. Walks plugins' :extends.emitters, picks the named emitter,
   dispatches on :input (:cfg or :tasks) and :encode (:json :edn :raw).
   For :input :tasks + :encode :raw entries join with a blank line;
   for :encode :json/:edn the output is the encoded sequence of per-task entries."
  (:require [cheshire.core :as json]
            [babashka.fs :as fs]
            [clojure.set]
            [clojure.string]
            [envrc.run :as envrc.run]
            [envrc.schemas :as envrc.schemas]))

(def auto-triggers
  "Events that trigger automatically; tasks with these in :on are filtered out
   of editor-style task emitters (they're not user-invokable)."
  #{:shell :gen :workspace-new :workspace-removed :pre-spawn :post-spawn
    :pane-died :service-started :service-died :enter})

(defn user-invokable?
  "True when task does not have any auto-trigger in :on."
  [task]
  (let [on (:on task)
        on-set (cond
                 (nil? on)        #{}
                 (keyword? on)    #{on}
                 (sequential? on) (set on))]
    (empty? (clojure.set/intersection on-set auto-triggers))))

(defn collect-emitters [plugins]
  (apply merge (map (fn [[_ m]] (get-in m [:extends :emitters])) plugins)))

(defn known-events [cfg]
  (apply clojure.set/union
         envrc.schemas/core-events
         (map (fn [[_ m]] (or (:events m) #{})) (:plugins cfg))))

(defn event-emitter
  "Synthesize implicit emitter for a declared event."
  [event-kw]
  {:input :tasks
   :encode :raw
   :filter (fn [t] (let [on (:on t)]
                     (or (= on event-kw)
                         (and (sequential? on) (some #{event-kw} on)))))
   :transform (fn [cfg name t]
                (envrc.run/shell-body cfg name t
                                      {:event event-kw
                                       :root (System/getProperty "user.dir")}))})

(defn resolve-emitter
  "Explicit emitter wins; implicit event emitter as fallback when fmt is a
   declared event."
  [cfg fmt-kw]
  (let [explicit (get (collect-emitters (:plugins cfg)) fmt-kw)]
    (cond
      explicit                              explicit
      (contains? (known-events cfg) fmt-kw) (event-emitter fmt-kw)
      :else                                 nil)))

(defn- gen-cfg [emitter cfg]
  (let [{:keys [encode transform]} emitter
        result (transform cfg)]
    (case encode
      :json (json/generate-string result {:pretty true})
      :edn  (pr-str result)
      :raw  (str result))))

(defn- encode-entries [encode entries]
  (case encode
    :json (json/generate-string entries {:pretty true})
    :edn  (pr-str entries)
    :raw  (clojure.string/join "\n\n" entries)))

(defn- gen-tasks [emitter cfg]
  (let [{:keys [encode transform filter]} emitter
        task-filter (if filter
                      (fn [[_ t]] (filter t))
                      (comp user-invokable? val))
        selected (->> (:tasks cfg) (clojure.core/filter task-filter) (sort-by key))
        entries (keep (fn [[k t]] (transform cfg k t)) selected)]
    (encode-entries encode entries)))

(defn- alias-task-entries [cfg]
  (for [[alias task-name] (sort-by key (get-in cfg [:use :aliases] {}))
        :let [task (get-in cfg [:tasks task-name])]
        :when task]
    [alias task-name task]))

(defn- gen-aliases [emitter cfg]
  (let [{:keys [encode transform]} emitter
        entries (keep (fn [[alias task-name task]]
                        (transform cfg alias task-name task))
                      (alias-task-entries cfg))]
    (encode-entries encode entries)))

(defn gen!
  "Dispatches envrc gen <fmt>. cfg = merged config. fmt = keyword.
   opts {:stdout boolean :output string}."
  [cfg fmt-kw {:keys [stdout output]}]
  (let [emitter (resolve-emitter cfg fmt-kw)]
    (when-not emitter
      (let [available  (sort (concat (map name (keys (collect-emitters (:plugins cfg))))
                                     (map name (known-events cfg))))
            suggestion (envrc.schemas/closest-match (name fmt-kw) available)]
        (throw (ex-info (str "envrc: no emitter for `" (name fmt-kw) "`"
                             (when suggestion (str "; did you mean `" suggestion "`?"))
                             ". Available: " (vec available))
                        {:fmt fmt-kw :available (vec available)
                         :suggestion suggestion}))))
    (let [input (or (:input emitter) :tasks)
          serialized (case input
                       :cfg     (gen-cfg emitter cfg)
                       :tasks   (gen-tasks emitter cfg)
                       :aliases (gen-aliases emitter cfg))
          dest (or output (:path emitter))]
      (if (or stdout (nil? dest))
        (println serialized)
        (do (fs/create-dirs (fs/parent dest))
            (spit dest serialized)
            (println (str "Wrote " dest)))))))
