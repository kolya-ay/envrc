(ns envrc.plugin.project
  "Project registry plugin. Records rc-marked projects into
   ~/.local/state/projects.json and exposes list/check/prune over it.
   Pure logic lives in envrc.project."
  (:require [envrc.api :as e]
            [envrc.project :as project]
            [babashka.cli :as cli]
            [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]))

(defn- register-impl [_cfg {:keys [args]}]
  (let [opts (cli/parse-opts args {:spec {:dir {} :slug {}}})
        {:keys [entry status]}
        (project/register! {:dir (or (:dir opts) (e/root) ".")
                            :slug (:slug opts)})]
    (e/fire! :project-new {:status status :entry entry})
    entry))

(defn- list-impl [_cfg {:keys [args]}]
  (let [opts     (cli/parse-opts args {:spec {:json {:coerce :boolean}}})
        registry (project/read-registry (project/registry-path))]
    (if (:json opts)
      (println (project/registry-json registry))
      (run! println (project/list-lines registry)))))

(defn- check-impl [_cfg _]
  (let [registry (project/read-registry (project/registry-path))]
    (doseq [w (project/check-registry registry fs/exists?)]
      (binding [*out* *err*] (println "envrc project:" w)))))

(defn- prune-impl [_cfg _]
  (let [path (project/registry-path)
        [registry removed] (project/prune-registry
                             (project/read-registry path) fs/exists?)]
    (project/write-registry! path registry)
    (doseq [p removed] (println "pruned" p))))

(defn- git-worktrees [proj-path]
  (let [{:keys [exit out]} (p/shell {:out :string :err :string :continue true}
                                    "git" "-C" proj-path "worktree" "list" "--porcelain")]
    (if (zero? exit) (project/parse-worktree-porcelain out) [])))

(defn- worktrees-impl [_cfg _]
  (run! println
        (project/worktree-lines (project/read-registry (project/registry-path))
                                git-worktrees)))

(defn- bash-escape-double-quoted [s]
  (-> (str s)
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")))

(defn- resolve-project-fields [cfg]
  (let [slug-raw  (get-in cfg [:project :slug])
        title-raw (get-in cfg [:project :title])
        slug      (if (str/blank? (str slug-raw))
                    "project"
                    (str slug-raw))
        title     (if (str/blank? (str title-raw))
                    (project/humanize slug)
                    (str title-raw))
        scope     (get-in cfg [:project :scope] :ego)
        workspace (get-in cfg [:project :workspace] "main")]
    {:slug slug :title title :scope scope :workspace workspace}))

(defn project-shell [cfg]
  (let [{:keys [slug title scope workspace]} (resolve-project-fields cfg)]
    (str "export ENVRC_PROJECT=\"" (bash-escape-double-quoted title) "\"\n"
         "export ENVRC_PROJECT_SLUG=" slug "\n"
         "export ENVRC_PROJECT_SCOPE=" (name scope) "\n"
         "export ENVRC_WORKSPACE=" workspace)))

(def plugin
  {:id "project"
   :description "Records rc-marked projects to ~/.local/state/projects.json"
   :events #{:project-new}
   :cli {:register  register-impl
         :list      list-impl
         :check     check-impl
         :prune     prune-impl
         :worktrees worktrees-impl}
   :extends {:emitters {:project {:input :cfg
                                  :encode :raw
                                  :transform project-shell}}}})
