(ns envrc.schemas-test
  (:require [clojure.test :refer [deftest is testing]]
            [envrc.schemas :as schemas]
            [envrc.validate :as validate]
            [malli.core :as m]))

(deftest aliases-are-synonyms-only-no-v1-migrations
  (testing "natural synonyms present"
    (is (= "tasks"    (get schemas/aliases "cmds")))
    (is (= "tasks"    (get schemas/aliases "scripts")))
    (is (= "packages" (get schemas/aliases "deps")))
    (is (= "use"      (get schemas/aliases "imports"))))
  (testing "V1-to-V2 legacy entries removed"
    (is (nil? (get schemas/aliases "commands")))
    (is (nil? (get schemas/aliases "services")))
    (is (nil? (get schemas/aliases "init-hooks")))
    (is (nil? (get schemas/aliases "help")))
    (is (nil? (get schemas/aliases "category")))
    (is (nil? (get schemas/aliases "autostart")))))

(deftest closest-match-uses-aliases-then-levenshtein
  (testing "alias hit wins"
    (is (= "tasks" (schemas/closest-match "cmds" ["tasks" "files" "panes"]))))
  (testing "alias hit only if target is in candidates"
    (is (nil? (schemas/closest-match "cmds" ["unrelated" "stuff"]))))
  (testing "Levenshtein falls in when no alias"
    (is (= "tasks" (schemas/closest-match "taks" ["tasks" "files"]))))
  (testing "no suggestion when far from anything"
    (is (nil? (schemas/closest-match "completely-unrelated" ["tasks" "files"]))))
)

(deftest reserved-sets-exist
  (is (>= (count (schemas/reserved-top-level-keys)) 6))
  (is (contains? (schemas/reserved-top-level-keys) :tasks))
  (is (contains? (schemas/reserved-top-level-keys) :use))
  (is (contains? (schemas/reserved-task-fields) :run))
  (is (contains? schemas/core-events :shell))
  (is (contains? schemas/reserved-task-names :status)))

(deftest taskname-rejects-reserved-verbs
  (is (some? (m/explain schemas/TaskName :gen)))
  (is (some? (m/explain schemas/TaskName :status)))
  (is (some? (m/explain schemas/TaskName :pane)))
  (is (some? (m/explain schemas/TaskName :ws)))
  (is (some? (m/explain schemas/TaskName :config)))
  (is (some? (m/explain schemas/TaskName :services))))

(deftest taskname-rejects-dash-prefix
  (is (some? (m/explain schemas/TaskName :-list)))
  (is (some? (m/explain schemas/TaskName :-x))))

(deftest taskname-accepts-ordinary-keywords
  (is (nil? (m/explain schemas/TaskName :dev)))
  (is (nil? (m/explain schemas/TaskName :build))))

(deftest task-accepts-run-as-string
  (let [task {:label "echo" :run "echo hi"}]
    (is (nil? (m/explain schemas/Task task)))))

(deftest task-accepts-run-as-vector
  (let [task {:label "echo" :run ["echo" "hi"]}]
    (is (nil? (m/explain schemas/Task task)))))

(deftest task-run-accepts-function
  (is (true? (m/validate schemas/Task {:run (fn [_] nil)}))))

(deftest task-and-top-level-env-accept-numeric-values
  (is (true? (m/validate schemas/Task {:env {:A 1 :B 1.5 :C nil :D "x"}})))
  (is (true? (m/validate schemas/EnvrcEdn-base {:env {:A 1 :B 1.5 :C nil :D "x"}})))
  (let [effective (validate/build-effective-schemas {})]
    (is (true? (m/validate (:Task effective) {:env {:A 1 :B 1.5 :C nil :D "x"}})))
    (is (true? (m/validate (:EnvrcEdn effective)
                           {:env {:A 1 :B 1.5 :C nil :D "x"}
                            :tasks {:serve {:env {:X 2.5}}}})))))

(deftest aliases-are-keyword-to-keyword
  (is (true? (m/validate schemas/EnvrcEdn-base {:use {:aliases {:t :test}}})))
  (is (false? (m/validate schemas/EnvrcEdn-base {:use {:aliases {:t "test"}}})))
  (is (false? (m/validate schemas/EnvrcEdn-base {:use {:aliases {:t ["test"]}}}))))

(deftest task-rejects-legacy-script-and-exec
  (is (m/explain schemas/Task {:script ["echo"]}))
  (is (m/explain schemas/Task {:exec ["echo"]})))

(deftest task-show-replaces-presentation
  (let [task {:label "agent" :run "claude" :show {:pane :agent}}]
    (is (nil? (m/explain schemas/Task task)))))

(deftest task-rejects-legacy-presentation
  (is (m/explain schemas/Task {:run "x" :presentation {:pane :a}})))

(deftest task-rejects-supervisor-fields-in-base
  (testing "supervisor fields are NOT in base Task — only in :extends.tasks via process-compose"
    (let [task {:run "x" :availability {:restart "always"}}]
      (is (m/explain schemas/Task task)))))

(deftest task-accepts-tolerant-as-base-field
  (testing ":tolerant lifted to base Task (capability-generic — event subscriber tolerance)"
    (is (nil? (m/explain schemas/Task {:run "x" :tolerant true})))
    (is (nil? (m/explain schemas/Task {:run "x" :tolerant false})))))

(deftest envrc-edn-base-rejects-unknown-top-level-key
  (is (some? (m/explain schemas/EnvrcEdn-base {:commands {}}))))

(deftest envrc-edn-base-accepts-canonical-keys
  (is (nil? (m/explain schemas/EnvrcEdn-base
                        {:tasks {} :files {} :env {} :packages []
                         :use {} :config {} :title "x"}))))

(deftest plugin-manifest-rejects-missing-id
  (is (some? (m/explain schemas/PluginManifest {:description ""}))))

(deftest plugin-manifest-accepts-minimal-shape
  (is (nil? (m/explain schemas/PluginManifest
                        {:id "x" :description ""
                         :cli {}}))))

(deftest plugin-manifest-accepts-provided-files
  (is (nil? (m/explain schemas/PluginManifest
                       {:id "x" :description ""
                        :provides {:files {:local [:graphify]
                                           :graphify [".ref/graphify-out/"]}}}))))

(deftest plugin-manifest-capability-singular
  (testing ":capability is a single keyword, not a set"
    (is (nil? (m/explain schemas/PluginManifest
                         {:id "x" :description "" :capability :pane :cli {}})))
    (is (some? (m/explain schemas/PluginManifest
                          {:id "x" :description "" :capabilities #{:pane}})))))

(deftest plugin-extension-slot-removed
  (testing "PluginExtensionSlot var is gone"
    (is (not (resolve 'envrc.schemas/PluginExtensionSlot)))))

(deftest levenshtein-equivalence
  (testing "memoized recursive impl matches prior Java-array behavior"
    (is (= "tasks"    (schemas/closest-match "taks" #{"tasks" "files" "env"})))
    (is (= "tasks"    (schemas/closest-match "cmds" #{"tasks" "files"})))
    (is (= "packages" (schemas/closest-match "deps" #{"packages" "env" "use"})))
    (is (= nil         (schemas/closest-match "completely-unrelated" #{"a" "b"})))
    (is (= "tasks"    (schemas/closest-match "tasks" #{"tasks"})))))

(deftest reserved-task-fields-derived-from-schema
  (testing "reserved-task-fields is a function deriving keys from Task schema"
    (let [fields (schemas/reserved-task-fields)]
      (is (contains? fields :label))
      (is (contains? fields :run))
      (is (contains? fields :show))
      (is (contains? fields :env)))))

(deftest reserved-top-level-keys-derived-from-schema
  (testing "reserved-top-level-keys is a function deriving keys from EnvrcEdn-base"
    (let [keys (schemas/reserved-top-level-keys)]
      (is (contains? keys :tasks))
      (is (contains? keys :files))
      (is (contains? keys :use)))))

(deftest envrc-edn-no-longer-allows-top-level-inputs
  (let [plugins {}
        e (-> (validate/build-effective-schemas plugins) :EnvrcEdn)
        cfg-with-inputs {:inputs {:nixpkgs "github:NixOS/nixpkgs/nixos-26.05"}}]
    (is (some? (m/explain e cfg-with-inputs)))))

(deftest flake-plugin-extends-use-with-inputs
  (load-file "plugins/default/flake.clj")
  (let [plugin-var @(resolve 'envrc.plugin.flake/plugin)
        use-schema (get-in plugin-var [:extends :use])]
    (is (= :map (first use-schema)))
    (is (some (fn [child] (= :inputs (first child))) (rest use-schema)))))

(deftest plugin-manifest-accepts-handles
  (is (m/validate envrc.schemas/PluginManifest
                  {:id "x" :description "y" :handles #{:foo :bar}})))

(deftest plugin-manifest-handles-optional
  (is (m/validate envrc.schemas/PluginManifest
                  {:id "x" :description "y"})))

(deftest plugin-manifest-handles-must-be-set-of-kw
  (is (not (m/validate envrc.schemas/PluginManifest
                       {:id "x" :description "y" :handles ["foo"]}))))

(deftest files-schema-accepts-label-dict
  (is (m/validate envrc.schemas/Files
                  {:envrc ["envrc.edn"] :standard [:envrc "literal"]})))

(deftest files-schema-rejects-vector
  (is (not (m/validate envrc.schemas/Files
                       [{:ignore {:paths [".direnv"]}}]))))

(deftest dirs-schema-accepts-anchors-and-category-maps
  (is (m/validate envrc.schemas/Use
                  {:dirs {:state   "/var/state"
                          :cache   "/var/cache"
                          :runtime "/run/x"
                          :worktrees {:base "/var/lib/wt"}
                          :ref       {:base "/var/lib/ref" :mirror "."}
                          :services  {:base "/srv"}
                          :socket    {:base "/sock"}}})))

(deftest dirs-schema-worktrees-is-map-only
  (is (m/validate envrc.schemas/Use {:dirs {:worktrees {:base "/wt"}}}))
  (is (not (m/validate envrc.schemas/Use {:dirs {:worktrees "/wt"}})))
  (is (not (m/validate envrc.schemas/Use {:dirs {:worktrees {:base 42}}}))))

(deftest dirs-schema-services-and-socket-are-maps
  (is (m/validate envrc.schemas/Use {:dirs {:services {:base "/s"}}}))
  (is (m/validate envrc.schemas/Use {:dirs {:socket {:base "/k"}}}))
  (is (not (m/validate envrc.schemas/Use {:dirs {:services "/s"}})))
  (is (not (m/validate envrc.schemas/Use {:dirs {:socket "/k"}}))))

(deftest dirs-schema-anchors-must-be-strings
  (is (not (m/validate envrc.schemas/Use {:dirs {:state 42}})))
  (is (not (m/validate envrc.schemas/Use {:dirs {:cache {:base "/c"}}}))))

(deftest dirs-schema-requires-ref-map
  (is (m/validate envrc.schemas/Use
                  {:dirs {:ref {:base "/var/lib/ref" :mirror "envrc"}}}))
  (is (m/validate envrc.schemas/Use
                  {:dirs {:ref {:mirror "."}}}))
  (is (not (m/validate envrc.schemas/Use {:dirs {:ref "/var/lib/ref"}})))
  (is (not (m/validate envrc.schemas/Use {:dirs {:ref {:base 42}}}))))

(deftest dirs-schema-allows-empty
  (is (m/validate envrc.schemas/Use {:dirs {}}))
  (is (m/validate envrc.schemas/Use {})))
