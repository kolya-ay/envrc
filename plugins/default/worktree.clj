(ns envrc.plugin.worktree
  "Minimal git-worktree workspace management. Drops scaff's scope-aware
   project creation in favor of plain `git worktree add/remove` plus
   `direnv allow/revoke`."
  (:require [envrc.api :as e]
            [envrc.dirs :as dirs]
            [envrc.files :as envrc.files]
            [envrc.git :as git]
            [babashka.process :as p]
            [babashka.fs :as fs]
            [clojure.string :as str]))

(defn- git-toplevel []
  (let [{:keys [exit out]} (p/shell {:out :string :err :string :continue true}
                                    "git" "rev-parse" "--show-toplevel")]
    (when (zero? exit) (str/trim out))))

(defn propagate-from
  "Resolve :link or :copy from cfg and apply the corresponding op from src→dst,
   printing one line per non-trivial action."
  [cfg src dst label]
  (let [paths (envrc.files/resolve (get-in cfg [:files label]) (:files cfg))
        op    (case label
                :link envrc.files/link-into
                :copy envrc.files/copy-into)]
    (doseq [p paths]
      (envrc.files/print-action (op src dst p) p))))

(defn- submodule-paths
  "Parse <src>/.gitmodules into a set of submodule paths. Empty set when absent."
  [src]
  (let [f (str src "/.gitmodules")]
    (if (fs/exists? f)
      (->> (str/split-lines (slurp f))
           (keep (fn [line]
                   (when-let [[_ p] (re-matches #"\s*path\s*=\s*(\S+)\s*" line)]
                     p)))
           set)
      #{})))

(defn- normalize-dirty
  "Resolve cfg's :use :worktree :dirty value to a category set or nil (off)."
  [cfg]
  (let [v (get-in cfg [:use :worktree :dirty])]
    (cond
      (true? v) #{:modified :staged :untracked}
      (set? v)  v
      :else     nil)))

(defn propagate-dirty-from
  "Carry main-checkout working-tree state from src into dst. Reads
   :use :worktree :dirty; off by default. Skips submodule paths.
   Prints one `carry` / `rm` line per action."
  [cfg src dst]
  (when-let [sel (normalize-dirty cfg)]
    (let [submods (submodule-paths src)]
      (doseq [{:keys [op categories path]} (git/dirty-paths src)
              :when (and (some sel categories)
                         (not (contains? submods path)))]
        (let [p     (envrc.files/normalize-path path)
              dst-p (str dst "/" p)]
          (case op
            :carried (let [src-p (str src "/" p)]
                       (when (fs/exists? src-p)
                         (fs/create-dirs (fs/parent dst-p))
                         (when (or (fs/exists? dst-p) (fs/sym-link? dst-p))
                           (if (fs/directory? dst-p) (fs/delete-tree dst-p) (fs/delete dst-p)))
                         (if (fs/directory? src-p) (fs/copy-tree src-p dst-p) (fs/copy src-p dst-p))
                         (println (format "  %-7s %s" "carry" path))))
            :removed (when (or (fs/exists? dst-p) (fs/sym-link? dst-p))
                       (if (fs/directory? dst-p) (fs/delete-tree dst-p) (fs/delete dst-p))
                       (println (format "  %-7s %s" "rm" path)))))))))

(defn- parse-submodule-status
  "Parse `git submodule status` output; return list of submodule paths.
   Each row: ` <sha> <path> (<ref>)` with leading space variants for state.
   Trailing `(ref)` token is optional — older git versions omit it on init."
  [out]
  (->> (str/split-lines out)
       (keep (fn [line]
               (when-let [[_ p] (re-matches #".\S+\s+(\S+)(?:\s+.*)?" line)]
                 p)))))

(defn propagate-submodules
  "If :use :worktree :submodules is truthy, run
   `git submodule update --init --recursive` in dst, then print
   one `submod` line per submodule from `git submodule status`."
  [cfg dst]
  (when (get-in cfg [:use :worktree :submodules])
    (let [{:keys [exit]} (p/shell {:continue true :dir dst}
                                  "git" "submodule" "update" "--init" "--recursive")]
      (when-not (zero? exit)
        (throw (ex-info "git submodule update failed" {:dst dst :exit exit}))))
    (let [{:keys [exit out]} (p/shell {:out :string :err :string :continue true :dir dst}
                                      "git" "submodule" "status" "--recursive")]
      (when (zero? exit)
        (doseq [p (parse-submodule-status out)]
          (println (format "  %-7s %s" "submod" p)))))))

(defn project-ctx [tl]
  (let [load-config @(requiring-resolve 'envrc.data/load-config)]
    (select-keys (:project (try (load-config tl) (catch Exception _ {})))
                 [:scope :slug])))

(defn worktree-dest [tl branch]
  (str (dirs/categorical-dir (project-ctx tl) "worktrees")
       "/" branch))

(defn- branch [cfg {:keys [args]}]
  (let [name (first args)
        tl   (git-toplevel)]
    (when-not name
      (throw (ex-info "envrc ws branch <name>: requires branch name" {})))
    (when-not tl
      (throw (ex-info "envrc ws branch must run inside a git project" {})))
    (let [dest (worktree-dest tl name)]
      (when (fs/exists? dest)
        (throw (ex-info (str "Worktree already exists at " dest) {:dest dest})))
      (fs/create-dirs (fs/parent dest))
      (if (zero? (:exit (p/shell {:continue true :dir tl}
                                 "git" "rev-parse" "--verify" name)))
        (p/shell "git" "-C" tl "worktree" "add" dest name)
        (p/shell "git" "-C" tl "worktree" "add" dest "-b" name))
      (propagate-from cfg tl dest :link)
      (propagate-from cfg tl dest :copy)
      (propagate-dirty-from cfg tl dest)
      (propagate-submodules cfg dest)
      (e/fire! :workspace-new {:src tl :dst dest :ctx (project-ctx tl)})
      (p/shell {:dir dest :continue true} "direnv" "allow")
      (println dest))))

(defn- go [_cfg {:keys [args]}]
  (let [name (first args)
        tl   (git-toplevel)]
    (when-not name
      (throw (ex-info "envrc ws go <name>: requires name" {})))
    (when-not tl
      (throw (ex-info "envrc ws go must run inside a git project" {})))
    (let [dest (worktree-dest tl name)]
      (if (fs/exists? dest)
        (println dest)
        (do (binding [*out* *err*]
              (println "no such worktree:" name))
            (System/exit 1))))))

(defn- rm [_cfg {:keys [args]}]
  (let [arg (first args)]
    (when-not arg
      (throw (ex-info "envrc ws rm <name-or-path>: requires arg" {})))
    (let [path (if (str/starts-with? arg "/")
                 arg
                 (when-let [tl (git-toplevel)]
                   (worktree-dest tl arg)))]
      (when-not path
        (throw (ex-info "envrc ws rm: not inside a git project and no absolute path given" {})))
      (p/shell {:continue true} "direnv" "revoke" path)
      (p/shell {:continue true} "git" "worktree" "remove" path)
      (e/fire! :workspace-removed {:dst path}))))

(defn- list-impl [_cfg _opts]
  (let [{:keys [exit out err]} (p/shell {:out :string :err :string :continue true}
                                        "git" "worktree" "list" "--porcelain")]
    (if (zero? exit)
      (doseq [block (str/split (str/trim out) #"\n\n")
              :let  [m (into {}
                              (keep (fn [line]
                                      (when-let [[_ k v] (re-matches #"^(worktree|HEAD|branch)\s+(.+)$" line)]
                                        [(keyword k) v]))
                                    (str/split-lines block)))]]
        (println (str (:worktree m)
                      (when (:branch m) (str "  " (:branch m)))
                      (when (:HEAD   m) (str "  " (subs (:HEAD m) 0 (min 8 (count (:HEAD m)))))))))
      (binding [*out* *err*]
        (println "git worktree list failed:" err)
        (System/exit (or exit 1))))))

(defn apply-impl [cfg {:keys [label]}]
  (let [src (git/main-worktree-root)
        dst (e/root)]
    (when (and src (not= src dst))
      (propagate-from cfg src dst label))
    nil))

(defn status-impl
  "Brief: counts. Verbose: per-path diff vs. main checkout."
  [cfg {:keys [label brief?]}]
  (let [paths (envrc.files/resolve (get-in cfg [:files label]) (:files cfg))
        src   (git/main-worktree-root)
        dst   (e/root)
        diffs (when (and src (not= src dst))
                (envrc.files/diff-paths label src dst paths))
        bad   (filter #(not= :ok (:state %)) diffs)]
    (println (str "  " (clojure.core/name label)
                  " — " (- (count paths) (count bad)) "/" (count paths) " ok"
                  (when (seq bad) (str "; " (count bad) " issues"))))
    (when (and (not brief?) (seq bad))
      (doseq [{:keys [path state]} bad] (println "   " (clojure.core/name state) ":" path))
      (println (str "    run `envrc apply " (clojure.core/name label) "` to re-propagate")))
    nil))

(def plugin
  {:id          "worktree"
   :description "Minimal git-worktree workspace management + file propagation"
   :requires    ["git" "direnv"]
   :capability  :workspace
   :events      #{:workspace-new :workspace-removed}
   :handles     #{:link :copy}
   :extends     {:use [:map
                       [:dirty      {:optional true}
                                    [:or :boolean
                                     [:set [:enum :modified :staged :untracked]]]]
                       [:submodules {:optional true} :boolean]]}
   :cli {:new    branch
         :branch branch
         :go     go
         :rm     rm
         :list   list-impl
         :apply  apply-impl
         :status status-impl}})
