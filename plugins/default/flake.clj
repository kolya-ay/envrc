(ns envrc.plugin.flake
  "Nix flake input wiring and flake.nix emission.
   Owns the :flake cfg-mode emitter under :extends.emitters."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [envrc.run :as run]))

(defn read-flake-lock []
  (let [lock-path (or (System/getenv "ENVRC_FLAKE_LOCK")
                      (str (System/getenv "HOME") "/.local/nix/flake.lock"))]
    (when (fs/exists? lock-path)
      (json/parse-string (slurp lock-path) true))))

(defn dedent [s]
  (let [lines (->> (str/split-lines s)
                   (drop-while str/blank?)
                   reverse (drop-while str/blank?) reverse)
        non-blank (remove str/blank? lines)]
    (if (empty? non-blank)
      ""
      (let [indent (apply min (map #(count (take-while #{\space} %)) non-blank))]
        (->> lines
             (map #(if (str/blank? %) "" (subs % (min indent (count %)))))
             (str/join "\n"))))))

(defn read-base-nix-config
  ([] (read-base-nix-config (str (System/getenv "HOME") "/.local/nix/flake.nix")))
  ([path]
   (when (fs/exists? path)
     (when-let [[_ body] (re-find #"(?s)nixConfig\s*=\s*\{(.*?)\n\s*\};" (slurp path))]
       (dedent body)))))

(defn url->github-parts [url]
  (when-let [[_ owner repo ref] (re-matches #"github:([^/]+)/([^/]+)(?:/(.+))?" url)]
    (cond-> {:type "github" :owner owner :repo repo}
      ref (assoc :ref ref))))

(defn match-locked [lock url]
  (when-let [parts (url->github-parts url)]
    (let [root-key   (keyword (:root lock))
          top-inputs (get-in lock [:nodes root-key :inputs])]
      (some (fn [[_ node-id]]
              (let [node (get-in lock [:nodes (keyword node-id)])]
                (when (= parts (:original node))
                  (:locked node))))
            top-inputs))))

(defn pin-url [lock url]
  (if-let [locked (match-locked lock url)]
    (str "github:" (:owner locked) "/" (:repo locked) "/" (:rev locked))
    url))

(defn resolve-inputs [config lock]
  (let [user    (->> (get-in config [:use :flake :inputs] {})
                     (into {} (map (fn [[k v]] [(keyword (name k)) v]))))
        nixpkgs (if-let [u (:nixpkgs user)]
                  (pin-url lock u)
                  (when-let [n (get-in lock [:nodes :nixpkgs :locked])]
                    (str "github:" (:owner n) "/" (:repo n) "/" (:rev n))))]
    (into {:nixpkgs nixpkgs}
          (for [[k url] (dissoc user :nixpkgs)]
            [k (pin-url lock url)]))))

(defn- nix-escape-str [s]
  (-> (str s)
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "${" "\\${")))

(defn- nix-escape-multi [s]
  (-> (str s)
      (str/replace "''" "'''")
      (str/replace "${" "''${")))

(defn render-inputs [resolved-inputs]
  (->> resolved-inputs
       (sort-by key)
       (keep (fn [[k url]]
               (when url
                 (str (name k) ".url = \"" (nix-escape-str url) "\";"))))
       (str/join "\n")))

(defn render-package [pkg]
  (if (vector? pkg)
    (let [[src attr] pkg]
      (str "[ \"" (nix-escape-str (name src)) "\" \"" (nix-escape-str (name attr)) "\" ]"))
    (str "\"" (nix-escape-str (name pkg)) "\"")))

(defn render-packages [packages]
  (->> packages
       (map render-package)
       (str/join "\n")))

(defn render-env [env]
  (->> env
       (sort-by key)
       (map (fn [[k v]]
              (str "\"" (nix-escape-str (name k)) "\" = \"" (nix-escape-str v) "\";")))
       (str/join "\n")))

(defn render-hooks [hooks]
  (->> hooks
       (map (fn [h]
              (let [lines (str/split-lines (nix-escape-multi h))]
                (str "''\n"
                     (str/join "\n" (map #(str "  " %) lines))
                     "\n''"))))
       (str/join "\n")))

(def ^:private sentinel-re #"^(\s*)#\s*@@(\w+)@@\s*$")

(defn- substitute-line [emitters line]
  (if-let [[_ indent key] (re-matches sentinel-re line)]
    (let [emitter  (get emitters (keyword key))
          rendered (when emitter (emitter))]
      (if (or (nil? rendered) (str/blank? rendered))
        ::drop
        (->> (str/split-lines rendered)
             (map #(str indent %))
             (str/join "\n"))))
    line))

(defn- render-template [template emitters]
  (->> (str/split-lines template)
       (map #(substitute-line emitters %))
       (remove #{::drop})
       (str/join "\n")))

(defn- template-path []
  (or (System/getenv "ENVRC_TEMPLATE")
      (str (or (System/getenv "ENVRC_SHARE")
               (System/getProperty "user.dir"))
           "/resources/templates/generated-flake.nix")))

(defn- render-menu-hook [config]
  (when (seq (:tasks config))
    (render-hooks ["envrc status"])))

(defn- collect-shell-hooks [cfg]
  (->> (:tasks cfg)
       (filter (fn [[_ t]] (or (= :shell (:on t))
                               (and (vector? (:on t)) (some #{:shell} (:on t))))))
       (sort-by key)
       (keep (fn [[k t]]
               (run/shell-body cfg k t {:root (System/getProperty "user.dir")})))
       vec))

(defn render
  ([config] (render config (slurp (template-path))))
  ([config template]
   (let [lock     (read-flake-lock)
         inputs   (resolve-inputs config lock)
         emitters {:inputs     #(render-inputs inputs)
                   :packages   #(render-packages (:packages config []))
                   :shellHooks #(render-hooks (collect-shell-hooks config))
                   :env        #(render-env (:env config {}))
                   :nixConfig  read-base-nix-config
                   :menu       #(render-menu-hook config)}]
     (render-template template emitters))))

(def plugin
  {:id "flake"
   :description "Nix flake input wiring and flake.nix emission"
   :extends
   {:use [:map
          [:inputs {:optional true} [:map-of keyword? string?]]]
    :emitters
    {:flake {:input :cfg
             :encode :raw
             :transform render}}}})
