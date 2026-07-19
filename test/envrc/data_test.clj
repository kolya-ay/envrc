(ns envrc.data-test
  (:require [clojure.test :refer [deftest is testing]]
            [babashka.fs :as fs]
            [envrc.config :as econf]
            [envrc.data :as d]
            [envrc.plugin]))

(def ^:private empty-roots
  (fn [_] {:global "/nonexistent-global" :project "/nonexistent-project"}))

(deftest ns-loads
  (testing "envrc.data namespace loads without errors"
    (is (some? (find-ns 'envrc.data)))))

(deftest aliases-target-existing-tasks-after-provides
  (let [cfg {:tasks {:test {:run ["bb" "test"]}}
             :use {:aliases {:t :test}}}]
    (is (= :test (get-in (#'d/validate-alias-targets cfg) [:use :aliases :t])))))

(deftest alias-to-missing-task-errors
  (is (thrown-with-msg? Exception #"alias :missing points to unknown task :nope"
        (#'d/validate-alias-targets {:tasks {} :use {:aliases {:missing :nope}}}))))

(deftest alias-to-task-without-run-errors
  (is (thrown-with-msg? Exception #"alias :bad points to task :bad without :run"
        (#'d/validate-alias-targets {:tasks {:bad {}} :use {:aliases {:bad :bad}}}))))

(deftest merge-additive-on-collections
  (with-redefs [d/load-global  (fn [] ["/tmp/fake-global"  {:packages [:nodejs :bun]
                                                               :env {:FOO "global" :BAR "global"}}])
                d/load-project (fn [_] ["/tmp/fake-project" {:packages [:rg]
                                                               :env {:BAR "project"}}])
                envrc.plugin/default-roots empty-roots]
    (let [cfg (d/load-config "/tmp")]
      (testing ":packages concat across layers (global + project)"
        (is (= [:nodejs :bun :rg] (:packages cfg))))
      (testing ":env merges (project keys win, global keys preserved)"
        (is (= {:FOO "global" :BAR "project"} (:env cfg)))))))

(deftest init-hooks-rejected-with-suggestion
  (let [tmp (str (fs/create-temp-dir))]
    (spit (str tmp "/envrc.edn") "{:init-hooks [\"echo hi\"]}")
    (with-redefs [d/load-global (constantly [tmp {}])
                  envrc.plugin/default-roots empty-roots]
      (let [err (try (d/load-config tmp) (catch Exception e (ex-data e)))]
        ;; V1-to-V2 alias `:init-hooks -> :tasks` was already absent; Levenshtein
        ;; ties multiple candidates so we assert presence, not identity.
        (is (= :unknown-key (:reason err)))
        (is (= :init-hooks  (:key err)))
        (is (some? (:suggestion err)))))))

(deftest yaml-only-project-loads-cleanly
  (testing "yaml-only project: load-config succeeds without edn-specific error"
    (let [tmp (str (System/getProperty "java.io.tmpdir") "/envrc-test-yaml-" (System/currentTimeMillis))]
      (try
        (.mkdirs (java.io.File. tmp))
        (spit (str tmp "/envrc.yml") "packages: []\nenv: {}\n")
        (with-redefs [d/load-global (fn [] ["/tmp/fake-global" {}])
                      envrc.plugin/default-roots empty-roots]
          (let [cfg (d/load-config tmp)]
            (is (map? cfg) "load-config should return a map for a yaml-only project")
            (is (= [] (:packages cfg)) "packages should be empty list from yaml")))
        (finally
          (fs/delete-tree tmp))))))

(deftest global-layer-duplicate-key-across-formats-allowed
  (testing "global layer with same key in both edn and json does not throw"
    ;; Simulates a user who has both ~/.config/envrc.edn (legacy) and
    ;; ~/.config/envrc.json (Nix-generated), both containing :packages.
    ;; validate-single-file! must NOT run on the global layer.
    (let [tmp (str (System/getProperty "java.io.tmpdir") "/envrc-test-global-" (System/currentTimeMillis))]
      (try
        (.mkdirs (java.io.File. tmp))
        (spit (str tmp "/envrc.edn") "{:packages [:git]}")
        (spit (str tmp "/envrc.json") "{\"packages\":[]}")
        (with-redefs [econf/xdg-config-dir (fn [] tmp)
                      d/load-project       (fn [_] ["/tmp/fake-project" {:packages []}])
                      envrc.plugin/default-roots empty-roots]
          (is (some? (d/load-config "/tmp"))
              "load-config should not throw when global layer has duplicate key across formats"))
        (finally
          (fs/delete-tree tmp))))))

(deftest yaml-extension-reported-in-error-message
  (testing ".yaml extension (not .yml) appears in duplicate-key error message"
    (let [tmp (str (System/getProperty "java.io.tmpdir") "/envrc-test-yaml-ext-" (System/currentTimeMillis))]
      (try
        (.mkdirs (java.io.File. tmp))
        ;; Use .yaml extension intentionally — error must say envrc.yaml not envrc.yml
        (spit (str tmp "/envrc.yaml") "packages: [a]
")
        (spit (str tmp "/envrc.edn") "{:packages [b]}")
        (is (thrown-with-msg? Exception #"envrc\.yaml"
              (d/load-project tmp)))
        (finally
          (fs/delete-tree tmp))))))

(deftest load-config-discovers-and-validates-plugins
  (let [tmp (str (fs/create-temp-dir))
        plugins-dir (str tmp "/.envrc")]
    (fs/create-dirs plugins-dir)
    (spit (str plugins-dir "/hello.clj")
          "(ns envrc.plugin.hello) (def plugin {:id \"hello\" :description \"\" :cli {:run (fn [_ _] :ok)}})")
    (spit (str tmp "/envrc.edn") "{:tasks {}}")
    (with-redefs [envrc.plugin/default-roots
                  (fn [_] {:global "/nonexistent-global" :project plugins-dir})]
      (let [result (d/load-config tmp)]
        (is (contains? (:plugins result) "hello"))
        (is (contains? (:dispatch result) "hello"))))))

(deftest use-key-deep-merges-global-and-project
  (let [tmp (str (fs/create-temp-dir))
        plugins-dir (str tmp "/.envrc")]
    (fs/create-dirs plugins-dir)
    ;; Stub a `konsole` plugin so check-use-plugin-ids! passes.
    (spit (str plugins-dir "/konsole.clj")
          (str "(ns envrc.plugin.konsole) "
               "(def plugin {:id \"konsole\" :description \"\" :capability :pane "
               ":cli {:spawn identity :list identity :focus identity :kill identity :send identity}})"))
    (spit (str tmp "/envrc.edn") "{:use {:konsole {:split \"vertical\"}}}")
    (with-redefs [envrc.data/load-global (constantly [tmp {:use {:konsole {:split "horizontal"
                                                                       :size 30}}}])
                  envrc.plugin/default-roots
                  (fn [_] {:global "/nonexistent-global" :project plugins-dir})]
      (let [cfg (d/load-config tmp)]
        (is (= "vertical" (get-in cfg [:use :konsole :split]))   "project leaf wins")
        (is (= 30        (get-in cfg [:use :konsole :size]))     "global leaf preserved")))))

(deftest use-nil-removes-plugin
  (let [tmp (str (fs/create-temp-dir))
        plugins-dir (str tmp "/.envrc")]
    (fs/create-dirs plugins-dir)
    (spit (str plugins-dir "/konsole.clj")
          (str "(ns envrc.plugin.konsole) "
               "(def plugin {:id \"konsole\" :description \"\" :capability :pane "
               ":cli {:spawn identity :list identity :focus identity :kill identity :send identity}})"))
    (spit (str tmp "/envrc.edn") "{:use {:konsole nil}}")
    (with-redefs [envrc.data/load-global (constantly [tmp {:use {:konsole {:split "horizontal"}}}])
                  envrc.plugin/default-roots
                  (fn [_] {:global "/nonexistent-global" :project plugins-dir})]
      (is (nil? (get-in (d/load-config tmp) [:use :konsole]))))))

(deftest tasks-replaces-commands
  (let [tmp (str (fs/create-temp-dir))]
    (spit (str tmp "/envrc.edn")
          "{:tasks {:test {:label \"Run tests\" :group \"dev\" :run [\"bun test\"]}}}")
    (with-redefs [envrc.data/load-global (constantly [tmp {}])
                  envrc.plugin/default-roots empty-roots]
      (let [cfg (d/load-config tmp)]
        (is (contains? (:tasks cfg) :test))
        (is (= "Run tests" (get-in cfg [:tasks :test :label])))
        (is (nil? (:commands cfg)) ":commands no longer surfaces")))))

(deftest commands-key-rejected-with-suggestion
  (let [tmp (str (fs/create-temp-dir))]
    (spit (str tmp "/envrc.edn") "{:commands {test {:help \"x\"}}}")
    (with-redefs [envrc.data/load-global (constantly [tmp {}])
                  envrc.plugin/default-roots empty-roots]
      (let [err (try (d/load-config tmp) (catch Exception e (ex-data e)))]
        ;; V1-to-V2 alias `:commands -> :tasks` dropped per migration policy.
        ;; Levenshtein scores tasks/packages/config equal so the suggestion
        ;; flips by set iteration order — assert presence, not identity.
        (is (= :unknown-key (:reason err)))
        (is (= :commands    (:key err)))
        (is (some? (:suggestion err)))))))

(deftest task-field-renames
  (let [tmp (str (fs/create-temp-dir))]
    ;; Use :on :shell (core event) instead of :on :open (a konsole plugin event)
    ;; since the test stubs out plugin discovery.
    (spit (str tmp "/envrc.edn")
          "{:tasks {:agent {:label \"Launch\" :group \"general\"
                            :run [\"claude\"] :show {:pane :agent}
                            :on :shell}}}")
    (with-redefs [envrc.data/load-global (constantly [tmp {}])
                  envrc.plugin/default-roots empty-roots]
      (let [cfg (d/load-config tmp)]
        (is (= "Launch" (get-in cfg [:tasks :agent :label])))
        (is (= "general" (get-in cfg [:tasks :agent :group])))
        (is (= :shell   (get-in cfg [:tasks :agent :on])))
        (is (= :agent   (get-in cfg [:tasks :agent :show :pane])))))))

(deftest help-field-rejected
  (let [tmp (str (fs/create-temp-dir))]
    (spit (str tmp "/envrc.edn") "{:tasks {:agent {:help \"x\" :run [\"hi\"]}}}")
    (with-redefs [envrc.data/load-global (constantly [tmp {}])
                  envrc.plugin/default-roots empty-roots]
      (let [err (try (d/load-config tmp) (catch Exception e (ex-data e)))]
        ;; :help is rejected as an unknown task field. V1-to-V2 alias mappings
        ;; were dropped per migration policy, so Levenshtein picks the closest
        ;; remaining task field rather than the historical :help -> :label hint.
        (is (= :help (:key err)))
        (is (some? (:suggestion err)))))))

(deftest services-key-rejected
  (let [tmp (str (fs/create-temp-dir))]
    (spit (str tmp "/envrc.edn") "{:services {db {:run [\"pg\"]}}}")
    (with-redefs [envrc.data/load-global (constantly [tmp {}])
                  envrc.plugin/default-roots empty-roots]
      (let [err (try (d/load-config tmp) (catch Exception e (ex-data e)))]
        ;; V1-to-V2 alias `:services -> :tasks` dropped per migration policy.
        ;; Bare Levenshtein picks `files` over `tasks` for `services`.
        (is (= :unknown-key (:reason err)))
        (is (= :services    (:key err)))
        (is (some? (:suggestion err)))))))

(deftest extends-use-rejects-shape-mismatch
  (testing "per-plugin :extends.use schema rejects a malformed user :use slot"
    (let [tmp         (str (fs/create-temp-dir))
          plugins-dir (str tmp "/.envrc")]
      (fs/create-dirs plugins-dir)
      (spit (str plugins-dir "/myplug.clj")
            (str "(ns envrc.plugin.myplug) "
                 "(def plugin {:id \"myplug\" :description \"\" "
                 ":cli {} "
                 ":extends {:use [:map [:split [:enum \"horizontal\" \"vertical\"]]]}})"))
      (spit (str tmp "/envrc.edn") "{:use {:myplug {:split \"diagonal\"}}}")
      (with-redefs [envrc.data/load-global (constantly [tmp {}])
                    envrc.plugin/default-roots
                    (fn [_] {:global  "/nonexistent-global"
                             :project plugins-dir})]
        (let [err (try (d/load-config tmp) (catch Exception e (ex-data e)))]
          (is (= :invalid-use-slot (:reason err))
              "loader rejects shorthand-violating :use slot with :invalid-use-slot")
          (is (some? (:errors err))
              "Malli explain output is surfaced for caller debugging"))))))

(deftest plugin-provides-merges-into-active-cfg
  (let [tmp (str (fs/create-temp-dir))
        plugins-dir (str tmp "/.envrc")]
    (fs/create-dirs plugins-dir)
    (spit (str plugins-dir "/konsole.clj")
          (str "(ns envrc.plugin.konsole) "
               "(def plugin {:id \"konsole\" :description \"\" :capability :pane "
               ":cli {:spawn identity :list identity :focus identity :kill identity :send identity} "
               ":provides {:tasks {:agent {:label \"Launch\" :run [\"x\"]}}}})"))
    (spit (str tmp "/envrc.edn") "{:tasks {}}")
    (with-redefs [envrc.data/load-global (constantly [tmp {}])
                  envrc.plugin/default-roots
                  (fn [_] {:global "/nonexistent-global"
                           :project (str tmp "/.envrc")})]
      (let [cfg (d/load-config tmp)]
        (is (= "Launch" (get-in cfg [:tasks :agent :label])))))))

(deftest plugin-provides-static-aliases
  (let [tmp (str (fs/create-temp-dir))
        plugins-dir (str tmp "/.envrc")]
    (fs/create-dirs plugins-dir)
    (spit (str plugins-dir "/static_alias.clj")
          (str "(ns envrc.plugin.static-alias) "
               "(def plugin {:id \"static-alias\" :description \"\" "
               ":provides {:tasks {:agent {:run [\"agent\"]}} "
               ":aliases {:sa :agent}}})"))
    (spit (str tmp "/envrc.edn") "{:tasks {}}")
    (with-redefs [envrc.data/load-global (constantly [tmp {}])
                  envrc.plugin/default-roots
                  (fn [_] {:global "/nonexistent-global"
                           :project (str tmp "/.envrc")})]
      (let [cfg (d/load-config tmp)]
        (is (= :agent (get-in cfg [:use :aliases :sa])))))))

(deftest project-task-overrides-plugin-provides
  (let [tmp (str (fs/create-temp-dir))
        plugins-dir (str tmp "/.envrc")]
    (fs/create-dirs plugins-dir)
    (spit (str plugins-dir "/konsole.clj")
          (str "(ns envrc.plugin.konsole) "
               "(def plugin {:id \"konsole\" :description \"\" :capability :pane "
               ":cli {:spawn identity :list identity :focus identity :kill identity :send identity} "
               ":provides {:tasks {:agent {:run [\"plugin\"]}}}})"))
    (spit (str tmp "/envrc.edn") "{:tasks {:agent {:run [\"project\"]}}}")
    (with-redefs [envrc.data/load-global (constantly [tmp {}])
                  envrc.plugin/default-roots
                  (fn [_] {:global "/nonexistent-global"
                           :project (str tmp "/.envrc")})]
      (let [cfg (d/load-config tmp)]
        (is (= ["project"] (get-in cfg [:tasks :agent :run])))))))

(deftest use-unknown-plugin-suggests-closest
  (with-redefs [envrc.plugin/default-roots
                (fn [_] {:global "/tmp/nonexistent-global"
                         :project "/tmp/nonexistent-project"})]
    (let [cfg {:use {:procees-compose {}}}    ; typo
          plugins {"process-compose" {:id "process-compose"}}]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"did you mean `process-compose`"
                            (d/check-use-plugin-ids! cfg plugins))))))

(deftest use-false-disables-plugin-provides
  (let [tmp (str (fs/create-temp-dir))
        plugins-dir (str tmp "/.envrc")]
    (fs/create-dirs plugins-dir)
    (spit (str plugins-dir "/konsole.clj")
          (str "(ns envrc.plugin.konsole) "
               "(def plugin {:id \"konsole\" :description \"\" :capability :pane "
               ":cli {:spawn identity :list identity :focus identity :kill identity :send identity} "
               ":provides {:tasks {:agent {:run [\"plugin\"]}}}})"))
    (spit (str tmp "/envrc.edn") "{:tasks {} :use {:konsole false}}")
    (with-redefs [envrc.data/load-global (constantly [tmp {}])
                  envrc.plugin/default-roots
                  (fn [_] {:global "/nonexistent-global"
                           :project (str tmp "/.envrc")})]
      (let [cfg (d/load-config tmp)]
        (is (nil? (get-in cfg [:tasks :agent])))))))

(deftest files-merge-concat-per-label
  (let [g {:envrc ["envrc.edn"] :agents ["AGENTS.md"]}
        p {:envrc ["envrc.json"] :specs ["specs/"]}
        merged (envrc.data/merge-files g p)]
    (is (= {:envrc  ["envrc.edn" "envrc.json"]
            :agents ["AGENTS.md"]
            :specs  ["specs/"]}
           merged))))

(deftest files-merge-dedupes-per-label
  (let [g {:envrc ["envrc.edn" "envrc.json"]}
        p {:envrc ["envrc.json" "extra.edn"]}
        merged (envrc.data/merge-files g p)]
    (is (= {:envrc ["envrc.edn" "envrc.json" "extra.edn"]} merged))))

(deftest files-merge-keeps-label-refs
  (let [merged (envrc.data/merge-files {:standard [:envrc :agents]} nil)]
    (is (= {:standard [:envrc :agents]} merged))))

(deftest files-label-refs-normalized-from-json-strings
  ;; Simulate the JSON-parsed shape: ":envrc" comes in as a string.
  (let [cfg {:files {:standard [":envrc" "literal-path"]}}
        normalized (envrc.data/normalize-files-label-refs cfg)]
    (is (= {:standard [:envrc "literal-path"]}
           (:files normalized)))))

(deftest files-label-refs-edn-already-keyword
  (let [cfg {:files {:standard [:envrc "literal-path"]}}]
    (is (= {:standard [:envrc "literal-path"]}
           (:files (envrc.data/normalize-files-label-refs cfg))))))

(deftest files-label-refs-scalar-keyword-sugared
  (let [cfg {:files {:local :taskmd}}]
    (is (= {:local [:taskmd]}
           (:files (envrc.data/normalize-files-label-refs cfg))))))

(deftest files-label-refs-scalar-string-sugared
  (let [cfg {:files {:dot-local "*.local.*"}}]
    (is (= {:dot-local ["*.local.*"]}
           (:files (envrc.data/normalize-files-label-refs cfg))))))

(deftest files-label-refs-scalar-colon-string-sugared-to-keyword
  (let [cfg {:files {:local ":taskmd"}}]
    (is (= {:local [:taskmd]}
           (:files (envrc.data/normalize-files-label-refs cfg))))))

(deftest normalize-tokens-coerces-dirty-vector-to-set
  (let [in  {:use {:worktree {:dirty ["modified" "staged"]}}}
        out (#'d/normalize-tokens in)]
    (is (= #{:modified :staged}
           (get-in out [:use :worktree :dirty])))))

(deftest normalize-tokens-passes-through-dirty-true
  (let [in  {:use {:worktree {:dirty true}}}
        out (#'d/normalize-tokens in)]
    (is (true? (get-in out [:use :worktree :dirty])))))

(deftest normalize-tokens-passes-through-dirty-set
  (let [in  {:use {:worktree {:dirty #{:modified}}}}
        out (#'d/normalize-tokens in)]
    (is (= #{:modified} (get-in out [:use :worktree :dirty])))))

(deftest normalize-tokens-coerces-prefixed-keywords
  (let [in  {:use {:worktree {:dirty [":modified" ":untracked"]}}}
        out (#'d/normalize-tokens in)]
    (is (= #{:modified :untracked}
           (get-in out [:use :worktree :dirty])))))

