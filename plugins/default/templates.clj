(ns envrc.plugin.templates
  (:require [babashka.fs :as fs]))

(defn default-template-dirs []
  [(str (or (System/getenv "ENVRC_SHARE")
            (System/getProperty "user.dir"))
        "/resources/templates")])

(defn template-dirs [cfg]
  (let [dirs (get-in cfg [:use :templates :dirs])]
    (if (seq dirs) dirs (default-template-dirs))))

(defn template-names [cfg]
  (->> (template-dirs cfg)
       (filter fs/directory?)
       (mapcat #(->> (fs/list-dir %)
                     (filter fs/directory?)
                     (map (comp str fs/file-name))))
       distinct
       sort
       vec))

(defn resolve-template-dir [cfg name]
  (or (some (fn [dir]
              (let [path (fs/path dir name)]
                (when (fs/directory? path)
                  (str (fs/canonicalize path)))))
            (template-dirs cfg))
      (throw (ex-info (str "unknown template: " name) {:name name}))))

(defn cli-list [cfg _ctx]
  (doseq [name (template-names cfg)]
    (println name)))

(defn cli-path [cfg {:keys [args]}]
  (let [name (first args)]
    (when-not name
      (binding [*out* *err*]
        (println "usage: envrc templates path <name>"))
      (System/exit 1))
    (println (resolve-template-dir cfg name))))

(def plugin
  {:id "templates"
   :description "Template registry; lists names and resolves template directories"
   :events #{}
   :extends {:use [:map {:closed true}
                   [:dirs {:optional true} [:sequential string?]]]}
   :cli {:list cli-list
         :path cli-path}})
