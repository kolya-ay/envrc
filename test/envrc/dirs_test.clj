(ns envrc.dirs-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [envrc.api]
            [envrc.dirs :as dirs]))

(defmacro with-env [bindings & body]
  `(binding [dirs/*env-overrides* ~bindings]
     ~@body))

(deftest state-base-xdg-wins
  (with-env {"XDG_STATE_HOME" "/tmp/xdg-state" "HOME" "/home/u"}
    (is (= "/tmp/xdg-state" (dirs/state-base)))))

(deftest state-base-home-fallback
  (with-env {"XDG_STATE_HOME" nil "HOME" "/home/u"}
    (is (= "/home/u/.local/state" (dirs/state-base)))))

(deftest project-dir-shape
  (with-env {"XDG_STATE_HOME" "/s" "HOME" "/h"}
    (is (= "/s/envrc/ego/foo/ref"
           (dirs/project-dir {:scope :ego :slug "foo"} "ref")))))

(deftest categorical-dir-shape
  (with-env {"XDG_STATE_HOME" "/s" "HOME" "/h"}
    (is (= "/s/envrc/worktrees/ego/foo"
           (dirs/categorical-dir {:scope :ego :slug "foo"} "worktrees")))))

(deftest sanitize-branch-slashes
  (is (= "feat--foo"  (dirs/sanitize-branch "feat/foo")))
  (is (= "main"       (dirs/sanitize-branch "main")))
  (is (= "a--b--c"    (dirs/sanitize-branch "a/b/c"))))

(deftest workspace-name-from-env
  (with-env {"ENVRC_WORKSPACE" "feature-x"}
    (is (= "feature-x"
           (dirs/workspace-name {:branch-fn (constantly "ignored")
                                 :main-branch-fn (constantly "ignored")})))))

(deftest workspace-name-from-branch
  (with-env {"ENVRC_WORKSPACE" nil}
    (is (= "feat--foo"
           (dirs/workspace-name {:branch-fn (constantly "feat/foo")
                                 :main-branch-fn (constantly "main")})))))

(deftest workspace-name-from-main-fallback
  (with-env {"ENVRC_WORKSPACE" nil}
    (is (= "main"
           (dirs/workspace-name {:branch-fn (constantly nil)
                                 :main-branch-fn (constantly "main")})))))

(deftest workspace-dir-shape
  (with-env {"XDG_STATE_HOME" "/s"}
    (is (= "/s/envrc/ego/foo/main/logs"
           (dirs/workspace-dir {:scope :ego :slug "foo" :workspace "main"}
                               "logs")))))

(deftest cache-dir-shape
  (with-env {"XDG_CACHE_HOME" "/c"}
    (is (= "/c/envrc/ego/foo/main/process-compose"
           (dirs/cache-dir {:scope :ego :slug "foo" :workspace "main"}
                           "process-compose")))))

(deftest cache-dir-direnv-lifecycle
  (with-env {"XDG_CACHE_HOME" "/c"}
    (is (= "/c/envrc/direnv/ego/foo/main/process-compose"
           (dirs/cache-dir {:scope :ego :slug "foo" :workspace "main"}
                           "process-compose"
                           {:lifecycle :direnv})))))

(deftest runtime-dir-shape
  (with-env {"XDG_RUNTIME_DIR" "/run/u/1000"}
    (is (= "/run/u/1000/envrc/ego/foo/main/process-compose.sock"
           (dirs/runtime-dir {:scope :ego :slug "foo" :workspace "main"}
                             "process-compose.sock")))))

(deftest runtime-dir-throws-without-xdg
  (with-env {"XDG_RUNTIME_DIR" nil}
    (is (thrown? clojure.lang.ExceptionInfo
                 (dirs/runtime-dir {:scope :ego :slug "foo" :workspace "main"}
                                   "x.sock")))))

(deftest categorical-dir-uses-map-base-for-worktrees
  (binding [envrc.api/*context* {:cfg {:use {:dirs {:worktrees {:base "/tmp/r"}}}}}]
    (with-env {"XDG_STATE_HOME" "/s" "HOME" "/h"}
      (is (= "/tmp/r/ego/foo"
             (dirs/categorical-dir {:scope :ego :slug "foo"} "worktrees"))))))

(deftest services-dir-is-the-layout-dir
  (binding [envrc.api/*context* nil]
    (with-env {"XDG_CACHE_HOME" "/c" "PWD" "/home/u/proj"}
      (is (= (dirs/direnv-layout-base) (dirs/services-dir))))))

(deftest resolve-dir-socket-full-path-under-runtime
  (binding [envrc.api/*context* nil]
    (with-env {"XDG_RUNTIME_DIR" "/run/u/1000"}
      (is (= "/run/u/1000/envrc/ego/foo/main/process-compose.sock"
             (dirs/resolve-dir :socket {:scope :ego :slug "foo" :workspace "main"} nil))))))

(deftest services-dir-base-override
  (binding [envrc.api/*context* {:cfg {:use {:dirs {:services {:base "/srv"}}}}}]
    (with-env {"XDG_CACHE_HOME" "/c" "PWD" "/home/u/proj"}
      (is (= "/srv" (dirs/services-dir))))))

(deftest resolve-dir-socket-base-override
  (binding [envrc.api/*context* {:cfg {:use {:dirs {:socket {:base "/sock"}}}}}]
    (with-env {"XDG_RUNTIME_DIR" "/run/u/1000"}
      (is (= "/sock/envrc/ego/foo/main/process-compose.sock"
             (dirs/resolve-dir :socket {:scope :ego :slug "foo" :workspace "main"} nil))))))

(deftest direnv-layout-base-matches-stdlib-formula
  (with-env {"XDG_CACHE_HOME" "/c" "PWD" "/p"}
    (is (= (str "/c/direnv/layouts/" (subs (dirs/sha1-hex "/p") 0 10) "-p")
           (dirs/direnv-layout-base)))))

(deftest categorical-dir-falls-back-without-override
  (binding [envrc.api/*context* {:cfg {:use {:dirs {:worktrees "/w"}}}}]
    (with-env {"XDG_STATE_HOME" "/s" "HOME" "/h"}
      (is (= "/s/envrc/ref/ego/foo"
             (dirs/categorical-dir {:scope :ego :slug "foo"} "ref"))))))

(deftest categorical-dir-ignores-bare-string-worktrees-override
  (binding [envrc.api/*context* {:cfg {:use {:dirs {:worktrees "/w"}}}}]
    (with-env {"XDG_STATE_HOME" "/s" "HOME" "/h"}
      (is (= "/s/envrc/worktrees/ego/foo"
             (dirs/categorical-dir {:scope :ego :slug "foo"} "worktrees"))))))

(deftest categorical-dir-reads-base-from-map-override
  (binding [envrc.api/*context* {:cfg {:use {:dirs {:ref {:base "/tmp/r" :mirror "."}}}}}]
    (with-env {"XDG_STATE_HOME" "/s" "HOME" "/h"}
      (is (= "/tmp/r/ego/foo"
             (dirs/categorical-dir {:scope :ego :slug "foo"} "ref"))))))

(deftest categorical-dir-map-override-without-base-falls-back
  (binding [envrc.api/*context* {:cfg {:use {:dirs {:ref {:mirror "envrc"}}}}}]
    (with-env {"XDG_STATE_HOME" "/s" "HOME" "/h"}
      (is (= "/s/envrc/ref/ego/foo"
             (dirs/categorical-dir {:scope :ego :slug "foo"} "ref"))))))

(deftest categorical-dir-no-context-uses-default
  (binding [envrc.api/*context* nil]
    (with-env {"XDG_STATE_HOME" "/s" "HOME" "/h"}
      (is (= "/s/envrc/worktrees/ego/foo"
             (dirs/categorical-dir {:scope :ego :slug "foo"} "worktrees"))))))

(deftest state-base-config-override-wins-over-xdg
  (binding [envrc.api/*context* {:cfg {:use {:dirs {:state "/cfg/state"}}}}]
    (with-env {"XDG_STATE_HOME" "/tmp/xdg-state" "HOME" "/home/u"}
      (is (= "/cfg/state" (dirs/state-base))))))

(deftest cache-base-config-override-wins-over-xdg
  (binding [envrc.api/*context* {:cfg {:use {:dirs {:cache "/cfg/cache"}}}}]
    (with-env {"XDG_CACHE_HOME" "/tmp/xdg-cache" "HOME" "/home/u"}
      (is (= "/cfg/cache" (dirs/cache-base))))))

(deftest runtime-base-config-override-wins-over-xdg
  (binding [envrc.api/*context* {:cfg {:use {:dirs {:runtime "/cfg/run"}}}}]
    (with-env {"XDG_RUNTIME_DIR" "/run/u/1000"}
      (is (= "/cfg/run" (dirs/runtime-base))))))

(deftest base-fns-ignore-non-string-anchor-override
  (binding [envrc.api/*context* {:cfg {:use {:dirs {:state {:base "/x"}}}}}]
    (with-env {"XDG_STATE_HOME" "/s" "HOME" "/h"}
      (is (= "/s" (dirs/state-base))))))
