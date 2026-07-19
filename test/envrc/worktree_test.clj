(ns envrc.worktree-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [envrc.api :as e]))

(def ^:private plugin-file
  (str (fs/canonicalize (fs/path (System/getProperty "user.dir") "plugins" "default" "worktree.clj"))))

(defn- wt-var [sym]
  (load-file plugin-file)
  @(resolve (symbol "envrc.plugin.worktree" (name sym))))

;; rm ends by firing :workspace-removed; declare it so fire! doesn't reject the event.
(def ^:private wt-ctx {:cfg {:plugins {:worktree {:events #{:workspace-removed}}}}})

(defn- git [dir & args]
  (apply p/shell {:dir dir :out :string :err :string :continue true} "git" args))

(defn- make-repo []
  (let [d (str (fs/create-temp-dir {:prefix "envrc-wt-"}))]
    (git d "init" "-q")
    (git d "config" "user.email" "t@t")
    (git d "config" "user.name" "t")
    (spit (str d "/f") "x")
    (git d "add" ".")
    (git d "commit" "-qm" "init")
    d))

(deftest ws-rm-removes-worktree-and-branch-by-abs-path-from-foreign-cwd
  (let [repo (make-repo)
        wt   (str repo "-wt")]
    (git repo "worktree" "add" "-q" wt "-b" "throwaway")
    (is (fs/exists? wt))
    ;; call rm with an absolute path; cwd is NOT the owning repo
    (e/with-context wt-ctx
      ((wt-var 'rm) {} {:args [wt]}))
    (is (not (fs/exists? wt)) "worktree dir removed")
    (is (str/blank? (:out (git repo "branch" "--list" "throwaway")))
        "branch safe-deleted")))
