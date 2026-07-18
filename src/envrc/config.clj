(ns envrc.config
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clj-yaml.core :as yaml]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(defn- load-map-file-by-ext [path label]
  (let [ext (last (str/split (str path) #"\."))
        body (slurp (str path))
        parsed (case ext
                 "edn"          (edn/read-string body)
                 ("yml" "yaml") (yaml/parse-string body)
                 "json"         (json/parse-string body true)
                 (throw (ex-info (str "envrc: " label
                                      " — unsupported extension (.edn/.yml/.yaml/.json only)")
                                 {:path (str path) :ext ext})))]
    (when-not (map? parsed)
      (throw (ex-info (str "envrc: " label " — file must contain a top-level map, got " (type parsed))
                      {:path (str path) :type (type parsed)})))
    parsed))

(defn xdg-config-dir []
  (or (System/getenv "XDG_CONFIG_HOME")
      (str (System/getenv "HOME") "/.config")))

(defn load-layer
  "Returns [layer filenames] where layer is {:edn cfg, :yaml cfg, :json cfg}
   for envrc.* files present in `dir` (missing files → key absent) and filenames
   is {:edn basename, :yaml basename, :json basename} recording the actual file
   chosen per format. `envrc.yaml` and `envrc.yml` both accepted under :yaml;
   `.yaml` takes precedence."
  [dir]
  (let [edn-path  (when (fs/exists? (str dir "/envrc.edn"))  (str dir "/envrc.edn"))
        yaml-path (or (when (fs/exists? (str dir "/envrc.yaml")) (str dir "/envrc.yaml"))
                      (when (fs/exists? (str dir "/envrc.yml"))  (str dir "/envrc.yml")))
        json-path (when (fs/exists? (str dir "/envrc.json")) (str dir "/envrc.json"))]
    [(cond-> {}
       edn-path  (assoc :edn  (load-map-file-by-ext edn-path (fs/file-name edn-path)))
       yaml-path (assoc :yaml (load-map-file-by-ext yaml-path (fs/file-name yaml-path)))
       json-path (assoc :json (load-map-file-by-ext json-path (fs/file-name json-path))))
     (cond-> {}
       edn-path  (assoc :edn  (fs/file-name edn-path))
       yaml-path (assoc :yaml (fs/file-name yaml-path))
       json-path (assoc :json (fs/file-name json-path)))]))

(defn- deep-merge [& maps]
  (let [maps (remove nil? maps)]
    (if (every? map? maps)
      (apply merge-with deep-merge maps)
      (last maps))))

(defn collapse-layer
  "Collapse a per-format layer to a single config map.
   Non-:config keys use format precedence edn > yaml > json, so higher-precedence
   formats overwrite lower-precedence ones when both are present. `:config`
   deep-merges json -> yaml -> edn so higher-precedence formats win per leaf."
  [layer]
  (let [merged-config  (deep-merge (get-in layer [:json :config])
                                   (get-in layer [:yaml :config])
                                   (get-in layer [:edn  :config]))
        without-config (->> [:json :yaml :edn]
                            (keep #(some-> (get layer %) (dissoc :config)))
                            (apply merge {}))]
    (cond-> without-config
      (seq merged-config) (assoc :config merged-config))))

(defn- resolve-value
  "Resolve a :config value. Map → returned as-is. String → load file at
   (base-dir / pointer-path), return its parsed top-level map."
  [value base-dir]
  (cond
    (map? value) value
    (string? value)
    (let [full (str (fs/path base-dir value))]
      (when-not (fs/exists? full)
        (throw (ex-info (str "envrc: :config " value " — file not found")
                        {:path full})))
      (load-map-file-by-ext full (str ":config " value)))
    :else (throw (ex-info (str "envrc: :config must be map or string, got " (pr-str value))
                          {:value value}))))

(defn resolve-all
  "Given a sequence of [base-dir, raw-:config-value] pairs (in low→high
   precedence order), resolve each value and deep-merge them.
   Returns the resolved config map, or {} if no sources."
  [sources]
  (->> sources
       (keep (fn [[base-dir value]]
               (when (some? value)
                 (resolve-value value base-dir))))
       (reduce deep-merge {})))
