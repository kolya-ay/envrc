(ns envrc.ports
  "Pure logic for deterministic per-worktree port assignment.
   Hash → offset (0..99), slot math, shell-line builder.
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

(defn- validate-stride!
  "Raise if stride < (count vars) — adjacent offsets would overlap."
  [stride vars]
  (let [n (count vars)]
    (when (< stride n)
      (throw (ex-info (str "envrc.ports: :stride " stride " < (count :vars) "
                           n " — adjacent worktree offsets would overlap")
                      {:stride stride :vars vars})))))


(defn build-exports
  "Returns an ordered seq of [var-keyword value-string] pairs for deterministic ports."
  [{:keys [base stride vars] :or {stride 10}} off]
  (validate-stride! stride vars)
  (mapv (fn [i v] [v (str (slot-port base stride off i))])
        (range)
        vars))

(defn shell-lines
  "Format `[var value]` pairs as `export VAR='value'` lines."
  [pairs]
  (->> pairs
       (map (fn [[k v]] (str "export " (name k) "='" v "'")))
       (str/join "\n")))
