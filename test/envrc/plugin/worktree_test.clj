(ns envrc.plugin.worktree-test
  (:require [clojure.test :refer [deftest is]]
            [envrc.dirs :as dirs]))

(load-file "plugins/default/worktree.clj")

(deftest worktree-dest-uses-categorical-layout
  (binding [dirs/*env-overrides* {"XDG_STATE_HOME" "/s"}]
    (with-redefs [envrc.plugin.worktree/project-ctx
                  (fn [_tl] {:scope :ego :slug "foo"})]
      (is (= "/s/envrc/worktrees/ego/foo/feat-x"
             (envrc.plugin.worktree/worktree-dest "/home/u/Code/foo" "feat-x"))))))
