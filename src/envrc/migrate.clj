(ns envrc.migrate
  "One-shot live-state migration to the V2-dirs layout."
  (:require [babashka.fs :as fs]
            [envrc.dirs :as dirs]))

(defn- home [{:keys [home]}]
  (or home (System/getenv "HOME")))

(defn- plan-registry [opts]
  (let [old (str (home opts) "/.local/state/projects.json")
        new (str (dirs/state-base) "/envrc/projects.json")]
    (when (and (fs/exists? old) (not= old new))
      [{:kind :move :from old :to new}])))

(defn- plan-worktrees [opts]
  (let [old-base (str (home opts) "/.local/state/worktrees")
        lookup   @(requiring-resolve 'envrc.project/lookup-by-basename)]
    (when (fs/directory? old-base)
      (for [basename-dir (fs/list-dir old-base)
            branch-dir   (fs/list-dir basename-dir)
            :let [entry (lookup (fs/file-name basename-dir))]
            :when entry
            :let [new-base (dirs/categorical-dir
                             {:scope (keyword (get entry "scope"))
                              :slug  (get entry "slug")}
                             "worktrees")]]
        {:kind :move
         :from (str branch-dir)
         :to   (str new-base "/" (fs/file-name branch-dir))}))))

(defn- plan-ref [opts]
  (let [old-base (str (home opts) "/.local/state/envrc-ref")
        lookup   @(requiring-resolve 'envrc.project/lookup-by-basename)]
    (when (fs/directory? old-base)
      (for [basename-dir (fs/list-dir old-base)
            :let [entry (lookup (fs/file-name basename-dir))]
            :when entry
            :let [new (dirs/project-dir
                        {:scope (keyword (get entry "scope"))
                         :slug  (get entry "slug")}
                        "ref")]]
        {:kind :move :from (str basename-dir) :to new}))))

(defn plan [opts]
  (vec (concat (or (plan-registry opts) [])
               (or (plan-worktrees opts) [])
               (or (plan-ref opts) []))))

(defn apply! [_opts plan]
  (doseq [{:keys [from to]} plan]
    (fs/create-dirs (fs/parent to))
    (fs/move from to)))

(defn print-plan [plan]
  (if (empty? plan)
    (println "Nothing to migrate.")
    (doseq [{:keys [from to]} plan]
      (println from "->" to))))
