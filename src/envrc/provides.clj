(ns envrc.provides
  "Plugin :provides wiring. Active plugins contribute :tasks, :aliases, and
   :files entries into the merged cfg. Project tasks/aliases take precedence on
   collision; file labels are merged with plugin defaults preserved first.")


(defn active?
  "Plugin is active when (:use cfg <id>) is not false/nil; absent → active."
  [cfg plugin-id]
  (let [use-map (:use cfg)
        k (keyword plugin-id)]
    (if (contains? use-map k)
      (let [v (get use-map k)]
        (not (or (false? v) (nil? v))))
      true)))

(defn- merge-disjoint [acc plugin-id entries label]
  (reduce
    (fn [a [entry-name spec]]
      (if-let [existing (get a entry-name)]
        (throw (ex-info (str "envrc: " label " `" (name entry-name)
                             "` provided by both `" (:plugin existing)
                             "` and `" plugin-id "`")
                        {:reason :provides-collision
                         :kind label
                         :name entry-name
                         :plugins [(:plugin existing) plugin-id]}))
        (assoc a entry-name {:plugin plugin-id :spec spec})))
    acc
    entries))

(defn- merge-file-label-maps
  [left right]
  (let [ks (into #{} (concat (keys left) (keys right)))]
    (into {}
          (for [k ks]
            [k (vec (distinct (concat (get left k []) (get right k []))))]))))

(defn- merge-file-entries [acc plugin-id entries]
  (reduce
    (fn [a [label spec]]
      (if-let [existing (get a label)]
        (assoc a label {:plugin (:plugin existing)
                        :spec (vec (distinct (concat (:spec existing) spec)))})
        (assoc a label {:plugin plugin-id :spec spec})))
    acc
    entries))


(defn- normalize-provides [resolved]
  {:tasks   (or (:tasks resolved) {})
   :aliases (or (:aliases resolved) {})
   :files   (or (:files resolved) {})})

(defn- resolve-provides
  "If :provides is a fn, call it with the plugin's :use slot; otherwise use as-is."
  [plugin cfg]
  (let [p    (:provides plugin)
        slot (get-in cfg [:use (keyword (:id plugin))])]
    (cond
      (nil? p) {:tasks {} :aliases {} :files {}}
      (fn?  p) (normalize-provides (p slot))
      (map? p) (normalize-provides p))))

(defn collect-provides
  "Walks active plugins, gathers :provides."
  [plugins cfg]
  (reduce
    (fn [acc [_id m]]
      (let [{:keys [tasks aliases files]} (resolve-provides m cfg)]
        (-> acc
            (update :tasks #(merge-disjoint % (:id m) tasks "task"))
            (update :aliases #(merge-disjoint % (:id m) aliases "alias"))
            (update :files #(merge-file-entries % (:id m) files)))))
    {:tasks {} :aliases {} :files {}}
    (filter #(active? cfg (:id (val %))) plugins)))

(defn merge-into-cfg
  "Folds plugin-provided entries into cfg.
   Project tasks/aliases override plugin contributions; file labels merge."
  [cfg {:keys [tasks aliases files]}]
  (let [plugin-tasks   (into {} (map (fn [[k {:keys [spec]}]] [k spec])) tasks)
        plugin-aliases (into {} (map (fn [[k {:keys [spec]}]] [k spec])) aliases)
        plugin-files   (into {} (map (fn [[k {:keys [spec]}]] [k spec])) files)]
    (-> cfg
        (update :tasks #(merge plugin-tasks %))
        (update :files #(merge-file-label-maps plugin-files %))
        (update-in [:use :aliases] #(merge plugin-aliases %)))))
