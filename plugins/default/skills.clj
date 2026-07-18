(ns envrc.plugin.skills
  (:require [babashka.fs :as fs]))

(defn default-skill-dirs []
  [(str (or (System/getenv "ENVRC_SHARE")
            (System/getProperty "user.dir"))
        "/resources/skills")])

(defn skill-dirs [cfg]
  (let [dirs (get-in cfg [:use :skills :dirs])]
    (if (seq dirs) dirs (default-skill-dirs))))

(defn list-skills [cfg]
  (->> (skill-dirs cfg)
       (filter fs/directory?)
       (mapcat #(->> (fs/list-dir %)
                     (filter fs/directory?)
                     (filter (fn [path] (fs/exists? (fs/path path "SKILL.md"))))
                     (map (comp str fs/file-name))))
       distinct
       sort
       vec))

(defn get-skill [cfg name]
  (or (some (fn [dir]
              (let [path (fs/path dir name "SKILL.md")]
                (when (fs/exists? path)
                  (slurp (str path)))))
            (skill-dirs cfg))
      (throw (ex-info (str "unknown skill: " name) {:name name}))))

(defn cli-list [cfg _ctx]
  (doseq [name (list-skills cfg)]
    (println name)))

(defn cli-get [cfg {:keys [args]}]
  (let [name (first args)]
    (when-not name
      (binding [*out* *err*]
        (println "usage: envrc skills get <name>"))
      (System/exit 1))
    (print (get-skill cfg name))))

(def plugin
  {:id "skills"
   :description "Skill registry; lists skills and reads SKILL.md content"
   :events #{}
   :extends {:use [:map {:closed true}
                   [:dirs {:optional true} [:sequential string?]]]}
   :cli {:list cli-list
         :get cli-get}})
