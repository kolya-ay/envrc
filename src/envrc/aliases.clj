(ns envrc.aliases)

(def core-flat-verbs #{:status :gen :config :pane :ws :services :files})

(defn- type-name [v]
  (cond
    (string? v) "string"
    (vector? v) "vector"
    (map? v) "map"
    (keyword? v) "keyword"
    (nil? v) "nil"
    :else (-> v class .getSimpleName)))

(defn resolve-task [aliases name-or-keyword]
  (get aliases (keyword name-or-keyword)))

(defn validate-aliases!
  ([aliases] (validate-aliases! aliases core-flat-verbs))
  ([aliases reserved-verbs]
   (doseq [[k v] aliases]
    (when (contains? reserved-verbs k)
      (throw (ex-info (str "envrc: alias `" (name k) "` cannot shadow core verb")
                      {:alias k})))
    (when-not (keyword? v)
      (throw (ex-info (str "envrc: alias " k " must point to a task keyword, got " (type-name v))
                      {:alias k :value v}))))))
