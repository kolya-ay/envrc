(ns envrc.worktree-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [envrc.api]
            [envrc.events]
            [envrc.runner]))

(load-file "plugins/default/worktree.clj")

(defn- wt-var [sym]
  (resolve (symbol "envrc.plugin.worktree" (name sym))))

(deftest manifest-shape
  (let [plugin @(wt-var 'plugin)]
    (is (= "worktree" (:id plugin)))
    (is (= :workspace (:capability plugin)))
    (is (= #{:link :copy} (:handles plugin)))
    (is (fn? (get-in plugin [:cli :list])))
    (is (fn? (get-in plugin [:cli :branch])))))

(deftest propagate-link
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))
        propagate-from (wt-var 'propagate-from)]
    (spit (str src "/AGENTS.md") "agents")
    (propagate-from {:files {:link [:agents] :agents ["AGENTS.md"]}}
                    src dst :link)
    (is (fs/sym-link? (str dst "/AGENTS.md")))))

(deftest propagate-copy
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))
        propagate-from (wt-var 'propagate-from)]
    (spit (str src "/.env.example") "X=1")
    (propagate-from {:files {:copy [".env.example"]}}
                    src dst :copy)
    (is (= "X=1" (slurp (str dst "/.env.example"))))))

(deftest list-verb-parses-porcelain-output
  (let [list-fn (wt-var 'list-impl)
        sample "worktree /tmp/main\nHEAD abc\nbranch refs/heads/main\n\nworktree /tmp/feat\nHEAD def\nbranch refs/heads/feat\n"]
    (with-redefs [babashka.process/shell
                  (constantly {:exit 0 :out sample :err ""})]
      (let [out (with-out-str (@list-fn {} {:args []}))]
        (is (re-find #"/tmp/main" out))
        (is (re-find #"/tmp/feat" out))))))

(deftest plugin-events-renamed
  (let [plugin @(wt-var 'plugin)]
    (is (= #{:workspace-new :workspace-removed} (:events plugin)))))

(deftest branch-fires-workspace-new-before-direnv-allow
  (let [calls   (atom [])
        branch  (wt-var 'branch)
        tmp-tl  (str (fs/create-temp-dir))
        tmp-dst (str (fs/create-temp-dir) "/wt-out")]
    (fs/create-dirs (str tmp-tl "/.git"))
    (with-redefs [envrc.git/main-worktree-root (constantly tmp-tl)
                  envrc.plugin.worktree/git-toplevel (constantly tmp-tl)
                  envrc.plugin.worktree/worktree-dest (constantly tmp-dst)
                  envrc.plugin.worktree/project-ctx (constantly {:scope :ego :slug "p"})
                  babashka.process/shell
                  (fn [opts-or-cmd & cmd]
                    (let [argv (if (map? opts-or-cmd) cmd (cons opts-or-cmd cmd))]
                      (cond
                        (and (= "git" (first argv)) (some #{"add"} argv))
                        (do (swap! calls conj :git-add) {:exit 0 :out "" :err ""})
                        (= "direnv" (first argv))
                        (do (swap! calls conj :direnv-allow) {:exit 0 :out "" :err ""})
                        :else {:exit 0 :out "" :err ""})))
                  envrc.api/fire!
                  (fn ([ev] (swap! calls conj [:fire ev]))
                      ([ev _payload] (swap! calls conj [:fire ev])))]
      (with-out-str (@branch {} {:args ["test-branch"]})))
    (let [order @calls
          fire-idx  (some #(when (= [:fire :workspace-new] (nth order %)) %) (range (count order)))
          allow-idx (some #(when (= :direnv-allow (nth order %)) %) (range (count order)))]
      (is (some? fire-idx))
      (is (some? allow-idx))
      (is (< fire-idx allow-idx)))))

(deftest propagate-from-prints-actions
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))
        propagate-from (wt-var 'propagate-from)]
    (spit (str src "/AGENTS.md") "x")
    (let [out (with-out-str
                (propagate-from {:files {:link ["AGENTS.md"]}} src dst :link))]
      (is (re-find #"  link\s+AGENTS\.md" out)))))

(deftest ws-branch-end-to-end-prefills-via-bus
  ;; End-to-end: branch fn fires :workspace-new through a real event bus,
  ;; ref + local subscribers run, all four labels land in dst before direnv allow.
  (require 'envrc.plugin.ref)
  (require 'envrc.plugin.local)
  (let [src        (str (fs/create-temp-dir))
        dst        (str (fs/create-temp-dir) "/wt")
        ref-target (str (fs/create-temp-dir) "/state-ref")
        branch     (wt-var 'branch)
        order      (atom [])
        worktree-plugin @(wt-var 'plugin)
        ref-plugin      @(resolve 'envrc.plugin.ref/plugin)
        local-plugin    @(resolve 'envrc.plugin.local/plugin)]
    (fs/create-dirs (str src "/.git"))
    (fs/create-dirs ref-target)
    (spit (str src "/AGENTS.md") "agents")
    (spit (str src "/.env.example") "X=1")
    (let [cfg {:files   {:link  ["AGENTS.md"]
                         :copy  [".env.example"]
                         :local [".env.example"]
                         :ref   []}
               :project {:scope :ego :slug "p" :workspace "main"}
               :use     {:ref {:root ".ref"}}
               :tasks   (merge (get-in ref-plugin   [:provides :tasks])
                               (get-in local-plugin [:provides :tasks]))
               :plugins {"worktree" worktree-plugin
                         "ref"      ref-plugin
                         "local"    local-plugin}}
          bus (envrc.runner/event-subscribers cfg)]
      (with-redefs [envrc.git/main-worktree-root      (constantly src)
                    envrc.plugin.worktree/git-toplevel (constantly src)
                    envrc.plugin.worktree/worktree-dest (constantly dst)
                    envrc.plugin.worktree/project-ctx  (constantly {:scope :ego :slug "p"})
                    envrc.dirs/categorical-dir         (fn [_ _] ref-target)
                    babashka.process/shell
                    (fn [opts-or-cmd & cmd]
                      (let [argv (if (map? opts-or-cmd) cmd (cons opts-or-cmd cmd))]
                        (cond
                          ;; Simulate git worktree add by creating the worktree dir with .git/info
                          (and (= "git" (first argv)) (some #{"add"} argv))
                          (do (fs/create-dirs (str dst "/.git/info"))
                              {:exit 0 :out "" :err ""})
                          (= "direnv" (first argv))
                          (do (swap! order conj :direnv-allow)
                              {:exit 0 :out "" :err ""})
                          :else {:exit 0 :out "" :err ""})))]
        (envrc.api/with-context {:cfg cfg :root src}
          (envrc.events/with-bus bus
            (with-out-str (@branch cfg {:args ["test-branch"]}))))))
    (is (fs/sym-link? (str dst "/AGENTS.md")) "link prefilled")
    (is (fs/exists?   (str dst "/.env.example")) "copy prefilled")
    (is (fs/sym-link? (str dst "/.ref")) "ref prefilled")
    (is (re-find #"\.env\.example" (slurp (str dst "/.git/info/exclude")))
        "local prefilled")
    (is (= [:direnv-allow] @order) "direnv allow ran exactly once, last")))

(deftest propagate-dirty-from-off-by-default
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))
        f   (wt-var 'propagate-dirty-from)]
    (with-redefs [envrc.git/dirty-paths
                  (constantly [{:op :carried :categories #{:modified} :path "x"}])]
      (spit (str src "/x") "edited")
      (@f {} src dst)
      (is (not (fs/exists? (str dst "/x"))) "no propagation when :dirty unset"))))

(deftest propagate-dirty-from-true-carries-all
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))
        f   (wt-var 'propagate-dirty-from)]
    (with-redefs [envrc.git/dirty-paths
                  (constantly [{:op :carried :categories #{:modified}  :path "m"}
                               {:op :carried :categories #{:staged}    :path "s"}
                               {:op :carried :categories #{:untracked} :path "u"}])]
      (spit (str src "/m") "M")
      (spit (str src "/s") "S")
      (spit (str src "/u") "U")
      (let [out (with-out-str
                  (@f {:use {:worktree {:dirty true}}} src dst))]
        (is (= "M" (slurp (str dst "/m"))))
        (is (= "S" (slurp (str dst "/s"))))
        (is (= "U" (slurp (str dst "/u"))))
        (is (re-find #"carry\s+m" out))
        (is (re-find #"carry\s+s" out))
        (is (re-find #"carry\s+u" out))))))

(deftest propagate-dirty-from-set-narrows
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))
        f   (wt-var 'propagate-dirty-from)]
    (with-redefs [envrc.git/dirty-paths
                  (constantly [{:op :carried :categories #{:modified}  :path "m"}
                               {:op :carried :categories #{:untracked} :path "u"}])]
      (spit (str src "/m") "M")
      (spit (str src "/u") "U")
      (@f {:use {:worktree {:dirty #{:untracked}}}} src dst)
      (is (not (fs/exists? (str dst "/m"))) "modified not selected")
      (is (= "U" (slurp (str dst "/u"))) "untracked selected"))))

(deftest propagate-dirty-from-applies-removal
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))
        f   (wt-var 'propagate-dirty-from)]
    (with-redefs [envrc.git/dirty-paths
                  (constantly [{:op :removed :categories #{:modified} :path "gone"}])]
      (spit (str dst "/gone") "still here")
      (let [out (with-out-str
                  (@f {:use {:worktree {:dirty true}}} src dst))]
        (is (not (fs/exists? (str dst "/gone"))) "removed from dst")
        (is (re-find #"rm\s+gone" out))))))

(deftest propagate-dirty-from-rename-removes-orig-and-carries-new
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))
        f   (wt-var 'propagate-dirty-from)]
    (with-redefs [envrc.git/dirty-paths
                  (constantly [{:op :removed :categories #{:staged}        :path "old.txt"}
                               {:op :carried :categories #{:staged}        :path "new.txt"}])]
      (spit (str src "/new.txt") "renamed")
      (spit (str dst "/old.txt") "original")
      (@f {:use {:worktree {:dirty true}}} src dst)
      (is (not (fs/exists? (str dst "/old.txt"))) "orig removed from dst")
      (is (= "renamed" (slurp (str dst "/new.txt"))) "new path carried"))))

(deftest propagate-dirty-from-skips-submodules
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))
        f   (wt-var 'propagate-dirty-from)]
    (spit (str src "/.gitmodules")
          "[submodule \"vendor/sub\"]\n\tpath = vendor/sub\n\turl = ./sub\n")
    (with-redefs [envrc.git/dirty-paths
                  (constantly [{:op :carried :categories #{:modified} :path "vendor/sub"}
                               {:op :carried :categories #{:modified} :path "regular.clj"}])]
      (fs/create-dirs (str src "/vendor/sub"))
      (spit (str src "/regular.clj") "code")
      (@f {:use {:worktree {:dirty true}}} src dst)
      (is (not (fs/exists? (str dst "/vendor/sub"))) "submodule path skipped")
      (is (= "code" (slurp (str dst "/regular.clj"))) "non-submodule carried"))))

(deftest propagate-dirty-from-silent-when-clean
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))
        f   (wt-var 'propagate-dirty-from)]
    (with-redefs [envrc.git/dirty-paths (constantly [])]
      (let [out (with-out-str
                  (@f {:use {:worktree {:dirty true}}} src dst))]
        (is (= "" out))))))

(deftest propagate-submodules-off-by-default
  (let [dst (str (fs/create-temp-dir))
        f   (wt-var 'propagate-submodules)
        calls (atom [])]
    (with-redefs [babashka.process/shell
                  (fn [opts-or-cmd & cmd]
                    (let [argv (if (map? opts-or-cmd) cmd (cons opts-or-cmd cmd))]
                      (swap! calls conj (vec argv))
                      {:exit 0 :out "" :err ""}))]
      (@f {} dst)
      (is (empty? @calls) "no shell calls when :submodules unset"))))

(deftest propagate-submodules-true-runs-init-and-prints-status
  (let [dst (str (fs/create-temp-dir))
        f   (wt-var 'propagate-submodules)
        calls (atom [])]
    (with-redefs [babashka.process/shell
                  (fn [opts-or-cmd & cmd]
                    (let [argv (if (map? opts-or-cmd) cmd (cons opts-or-cmd cmd))]
                      (swap! calls conj (vec argv))
                      (cond
                        (some #{"status"} argv)
                        {:exit 0 :out " abc123 vendor/y-power (heads/main)\n abc456 vendor/koso (heads/main)\n" :err ""}
                        :else {:exit 0 :out "" :err ""})))]
      (let [out (with-out-str
                  (@f {:use {:worktree {:submodules true}}} dst))]
        (is (some #(and (= "git" (first %)) (some #{"update"} %)) @calls)
            "submodule update called")
        (is (some #(and (= "git" (first %)) (some #{"status"} %)) @calls)
            "submodule status called")
        (is (re-find #"submod\s+vendor/y-power" out))
        (is (re-find #"submod\s+vendor/koso" out))))))

(deftest propagate-submodules-throws-on-update-failure
  (let [dst (str (fs/create-temp-dir))
        f   (wt-var 'propagate-submodules)]
    (with-redefs [babashka.process/shell
                  (fn [opts-or-cmd & cmd]
                    (let [argv (if (map? opts-or-cmd) cmd (cons opts-or-cmd cmd))]
                      (if (some #{"update"} argv)
                        {:exit 1 :out "" :err "fatal"}
                        {:exit 0 :out "" :err ""})))]
      (is (thrown? Exception
            (@f {:use {:worktree {:submodules true}}} dst))))))

(deftest propagate-submodules-silent-when-no-submodules
  (let [dst (str (fs/create-temp-dir))
        f   (wt-var 'propagate-submodules)]
    (with-redefs [babashka.process/shell
                  (fn [opts-or-cmd & cmd]
                    (let [argv (if (map? opts-or-cmd) cmd (cons opts-or-cmd cmd))]
                      {:exit 0 :out "" :err ""}))]
      (let [out (with-out-str
                  (@f {:use {:worktree {:submodules true}}} dst))]
        (is (= "" out))))))

(deftest branch-runs-dirty-and-submodule-propagators-before-workspace-new
  (require 'envrc.plugin.ref)
  (require 'envrc.plugin.local)
  (let [src        (str (fs/create-temp-dir))
        dst        (str (fs/create-temp-dir) "/wt")
        ref-target (str (fs/create-temp-dir) "/state-ref")
        branch     (wt-var 'branch)
        order      (atom [])
        worktree-plugin @(wt-var 'plugin)
        ref-plugin      @(resolve 'envrc.plugin.ref/plugin)
        local-plugin    @(resolve 'envrc.plugin.local/plugin)]
    (fs/create-dirs (str src "/.git"))
    (fs/create-dirs ref-target)
    (spit (str src "/AGENTS.md") "agents")
    (spit (str src "/.env.example") "X=1")
    (spit (str src "/scratch.md") "wip-notes")    ; untracked
    (let [cfg {:files   {:link  ["AGENTS.md"]
                         :copy  [".env.example"]
                         :local []
                         :ref   []}
               :project {:scope :ego :slug "p" :workspace "main"}
               :use     {:ref       {:root ".ref"}
                         :worktree  {:dirty      true
                                     :submodules true}}
               :tasks   (merge (get-in ref-plugin   [:provides :tasks])
                               (get-in local-plugin [:provides :tasks]))
               :plugins {"worktree" worktree-plugin
                         "ref"      ref-plugin
                         "local"    local-plugin}}
          bus (envrc.runner/event-subscribers cfg)]
      (with-redefs [envrc.git/main-worktree-root      (constantly src)
                    envrc.git/dirty-paths
                    (constantly [{:op :carried :categories #{:untracked} :path "scratch.md"}])
                    envrc.plugin.worktree/git-toplevel  (constantly src)
                    envrc.plugin.worktree/worktree-dest (constantly dst)
                    envrc.plugin.worktree/project-ctx   (constantly {:scope :ego :slug "p"})
                    envrc.dirs/categorical-dir          (fn [_ _] ref-target)
                    envrc.api/fire!
                    (fn ([ev] (swap! order conj [:fire ev]))
                        ([ev _payload] (swap! order conj [:fire ev])))
                    babashka.process/shell
                    (fn [opts-or-cmd & cmd]
                      (let [argv (if (map? opts-or-cmd) cmd (cons opts-or-cmd cmd))]
                        (cond
                          (and (= "git" (first argv)) (some #{"add"} argv))
                          (do (fs/create-dirs (str dst "/.git/info"))
                              (swap! order conj :git-worktree-add)
                              {:exit 0 :out "" :err ""})
                          (and (= "git" (first argv)) (some #{"submodule"} argv) (some #{"update"} argv))
                          (do (swap! order conj :git-submodule-update)
                              {:exit 0 :out "" :err ""})
                          (and (= "git" (first argv)) (some #{"submodule"} argv) (some #{"status"} argv))
                          {:exit 0 :out "" :err ""}
                          (= "direnv" (first argv))
                          (do (swap! order conj :direnv-allow)
                              {:exit 0 :out "" :err ""})
                          :else {:exit 0 :out "" :err ""})))]
        (envrc.api/with-context {:cfg cfg :root src}
          (envrc.events/with-bus bus
            (with-out-str (@branch cfg {:args ["test-branch"]}))))))
    (is (= "wip-notes" (slurp (str dst "/scratch.md"))) "dirty untracked file carried")
    (is (= [:git-worktree-add :git-submodule-update [:fire :workspace-new] :direnv-allow] @order)
        ":workspace-new fires after submodule update and before direnv allow")))

(deftest plugin-extends-use-with-dirty-and-submodules
  (let [plugin @(wt-var 'plugin)
        schema (get-in plugin [:extends :use])]
    (is (some? schema))
    (let [validator (requiring-resolve 'malli.core/validate)]
      (is (validator schema {:dirty true}))
      (is (validator schema {:dirty #{:modified :staged :untracked}}))
      (is (validator schema {:submodules true}))
      (is (validator schema {:dirty #{:modified} :submodules true})))))

;; merged from upstream original — tests the rm verb end-to-end with real git
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
    (envrc.api/with-context wt-ctx
      ((wt-var 'rm) {} {:args [wt]}))
    (is (not (fs/exists? wt)) "worktree dir removed")
    (is (str/blank? (:out (git repo "branch" "--list" "throwaway")))
        "branch safe-deleted")))
