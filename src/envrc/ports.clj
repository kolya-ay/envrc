(ns envrc.ports
  "Pure logic for deterministic per-worktree port assignment.
   Hash → offset (0..99), slot math, template expansion, shell-line builder.
   No I/O, no env reads — the thin plugin in envrc.plugin.ports handles those."
  (:require [clojure.string :as str]))

(defn md5-hex
  "Lowercase hex md5 of `s`. Distribution only — not security."
  [s]
  (let [md (java.security.MessageDigest/getInstance "MD5")
        bs (.digest md (.getBytes (str s) "UTF-8"))]
    (apply str (map #(format "%02x" %) bs))))

(defn offset
  "Deterministic 0..99 offset from a workspace handle.
   md5 → first 4 hex chars → mod 100. Matches workmux's algorithm."
  [handle]
  (-> (subs (md5-hex handle) 0 4)
      (Integer/parseInt 16)
      (mod 100)))

(defn slot-port
  "Port for the i-th slot in the per-worktree stride window."
  [base stride offset i]
  (+ base (* offset stride) i))

(defn expand-template
  "Expand `${VAR}` in `tpl` against `resolved` (map of keyword → string).
   Throws naming the missing key for any unresolved ${X}."
  [tpl resolved]
  (str/replace tpl #"\$\{([A-Za-z_][A-Za-z0-9_]*)\}"
               (fn [[_ var-name]]
                 (let [k (keyword var-name)]
                   (if-let [v (get resolved k)]
                     (str v)
                     (throw (ex-info (str "envrc.ports: unknown ${" var-name
                                          "} in :derive template")
                                     {:var var-name :template tpl})))))))

(defn- validate-stride!
  "Raise if stride < (count vars) — adjacent offsets would overlap."
  [stride vars]
  (let [n (count vars)]
    (when (< stride n)
      (throw (ex-info (str "envrc.ports: :stride " stride " < (count :vars) "
                           n " — adjacent worktree offsets would overlap")
                      {:stride stride :vars vars})))))

(defn- validate-template-value!
  "Raise if a derived value contains `'` — would break the single-quoted
   shell export the emitter produces."
  [k v]
  (when (str/includes? (str v) "'")
    (throw (ex-info (str "envrc.ports: derived value for " k
                         " contains single quote, which breaks shell quoting")
                    {:key k :value v}))))

(defn build-exports
  "Returns an ordered seq of [var-keyword value-string] pairs.
   Walks `:vars` first (literal port numbers), then `:derive` in declaration
   order with each derived value visible to subsequent derives."
  [{:keys [base stride vars derive] :or {stride 10}} off]
  (validate-stride! stride vars)
  (let [var-pairs (mapv (fn [i v] [v (str (slot-port base stride off i))])
                        (range) vars)
        resolved-init (into {} var-pairs)]
    (loop [resolved resolved-init
           acc      var-pairs
           todo     (seq derive)]
      (if-let [[k tpl] (first todo)]
        (let [v (expand-template tpl resolved)]
          (validate-template-value! k v)
          (recur (assoc resolved k v)
                 (conj acc [k v])
                 (rest todo)))
        acc))))

(defn shell-lines
  "Format `[var value]` pairs as `export VAR='value'` lines."
  [pairs]
  (->> pairs
       (map (fn [[k v]] (str "export " (name k) "='" v "'")))
       (str/join "\n")))
