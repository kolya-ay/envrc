(ns envrc.env
  (:require [clojure.string :as str]
            [envrc.ports :as ports]))

(def ^:private template-pattern #"\$\{([A-Za-z_][A-Za-z0-9_]*)\}")

(defn env-port-offset
  "Parse and range-check ENVRC_PORT_OFFSET. nil/blank => nil; bad value raises."
  []
  (let [raw (System/getenv "ENVRC_PORT_OFFSET")]
    (when-not (str/blank? raw)
      (let [n (try (Integer/parseInt (str/trim raw))
                   (catch Exception _
                     (throw (ex-info (str "envrc.env: ENVRC_PORT_OFFSET is not an integer: "
                                          (pr-str raw))
                                     {:value raw}))))]
        (when-not (<= 0 n 99)
          (throw (ex-info (str "envrc.env: ENVRC_PORT_OFFSET out of range 0..99: " n)
                          {:value n})))
        n))))

(defn- offset-for [cfg]
  (let [env-off (env-port-offset)
        cfg-off (get-in cfg [:use :ports :offset])
        handle  (get-in cfg [:project :workspace] "main")]
    (cond
      (some? env-off) env-off
      (some? cfg-off) cfg-off
      :else           (ports/offset handle))))

(defn- declared-non-nil [m]
  (->> (or m {})
       (remove (comp nil? val))
       (into {})))

(defn- template-refs [tpl]
  (mapv (comp keyword second) (re-seq template-pattern (str tpl))))

(defn- replace-ref [s dep value]
  (str/replace s
               (re-pattern (java.util.regex.Pattern/quote (str "${" (name dep) "}")))
               (java.util.regex.Matcher/quoteReplacement value)))

(defn- fail! [message data]
  (throw (ex-info message data)))

(defn- normalize-value [scope root value]
  (cond
    (string? value) value
    (number? value) (str value)
    (nil? value) nil
    :else
    (fail! (str "envrc.env: " (name root) " must be a string, number, or nil")
           {:scope scope :var root :value value :expected [:string :number :nil]})))

(defn- normalize-declared [scope declared]
  (into {}
        (map (fn [[k v]] [k (normalize-value scope k v)]))
        (or declared {})))

(defn- resolve-var [root k inherited declared stack cache]
  (if-let [cached (get cache k)]
    [cached cache]
    (cond
      (contains? declared k)
      (let [tpl (get declared k)]
        (when (nil? tpl)
          (fail! (str "envrc.env: " (name root) " references nil variable " (name k))
                 {:var root :stack stack :template tpl :target k}))
        (loop [result tpl
               deps   (seq (template-refs tpl))
               cache* cache]
          (if-let [dep (first deps)]
            (cond
              (contains? declared dep)
              (let [dep-tpl (get declared dep)]
                (when (nil? dep-tpl)
                  (fail! (str "envrc.env: " (name root) " references nil variable " (name dep))
                         {:var root :stack stack :template tpl :target dep}))
                (when (some #{dep} stack)
                  (fail! (str "envrc.env: cycle resolving " (name root))
                         {:var root
                          :stack (conj (vec stack) dep)
                          :template dep-tpl}))
                (let [[dep-v cache**] (resolve-var root dep inherited declared (conj (vec stack) dep) cache*)]
                  (recur (replace-ref result dep dep-v)
                         (next deps)
                         cache**)))

              (contains? inherited dep)
              (recur (replace-ref result dep (get inherited dep))
                     (next deps)
                     cache*)

              :else
              (fail! (str "envrc.env: unknown variable " (name dep))
                     {:var root :stack stack :template tpl :target dep}))
            [result (assoc cache* k result)])))

      (contains? inherited k)
      [(get inherited k) (assoc cache k (get inherited k))]

      :else
      (fail! (str "envrc.env: unknown variable " (name k))
             {:var root :stack stack :template nil :target k}))))

(defn- resolve-declared [scope inherited declared]
  (let [declared* (normalize-declared scope declared)]
    (reduce (fn [{:keys [cache resolved]} k]
              (let [[v cache*] (resolve-var k k inherited declared* [k] cache)]
                {:cache cache* :resolved (assoc resolved k v)}))
            {:cache {} :resolved {}}
            (keys (declared-non-nil declared*)))))

(defn port-env [cfg]
  (if-let [ports-cfg (get-in cfg [:use :ports])]
    (->> (ports/build-exports ports-cfg (offset-for cfg))
         (into {}))
    {}))

(defn global-env [cfg]
  (let [declared (normalize-declared :global (:env cfg))
        resolved (:resolved (resolve-declared :global (port-env cfg) declared))]
    (into {} (map (fn [[k _]] [k (get resolved k)])) (declared-non-nil declared))))

(defn task-env [cfg task]
  (let [ports-map     (port-env cfg)
        global-map    (global-env cfg)
        task-declared (normalize-declared :task (:env task))
        task-resolved (:resolved (resolve-declared :task (merge ports-map global-map) task-declared))
        unset         (->> task-declared
                           (keep (fn [[k v]]
                                   (when (and (nil? v)
                                              (or (contains? global-map k)
                                                  (contains? ports-map k)))
                                     k)))
                           sort
                           vec)]
    {:set (reduce dissoc (merge ports-map global-map task-resolved) unset)
     :unset unset}))
