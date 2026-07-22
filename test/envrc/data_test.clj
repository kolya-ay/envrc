(ns envrc.data-test
  (:require [clojure.test :refer [deftest is testing]]
            [babashka.fs :as fs]
            [envrc.config :as econf]
            [envrc.data :as d]
            [envrc.env :as env]
            [envrc.plugin]))

(load-file "plugins/default/ports.clj")

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

(deftest load-config-resolves-top-level-env-after-project-metadata
  (let [tmp (str (fs/create-temp-dir))]
    (spit (str tmp "/envrc.edn")
          "{:use {:ports {:base 2000 :stride 10 :offset 0 :vars [:PI_ROUTE_PORT]}}
             :env {:PORT 3000
                   :URL \"http://127.0.0.1:${PI_ROUTE_PORT}:${PORT}\"}}")
    (with-redefs [d/load-global (constantly [tmp {}])
                  env/env-port-offset (constantly nil)
                  envrc.plugin/default-roots (fn [_] {:global [] :project ["plugins/default"]})
                  envrc.git/branch (constantly "main")
                  envrc.git/main-branch (constantly "main")]
      (let [cfg (d/load-config tmp)]
        (is (= {:PORT "3000"
                :URL "http://127.0.0.1:2000:3000"}
               (:env cfg)))
        (is (not (contains? (:env cfg) :PI_ROUTE_PORT)))))))

(deftest init-hooks-rejected-with-suggestion
  (let [tmp (str (fs/create-temp-dir))]
    (spit (str tmp "/envrc.edn") "{:init-hooks [\"echo hi\"]}")
    (with-redefs [d/load-global (constantly [tmp {}])
                  envrc.plugin/default-roots empty-roots]
      (let [err (try (d/load-config tmp) (catch Exception e (ex-data e)))]
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
            (is (map? cfg))
            (is (= [] (:packages cfg)))))
        (finally
          (fs/delete-tree tmp))))))

(deftest global-layer-duplicate-key-across-formats-allowed
  (testing "global layer with same key in both edn and json does not throw"
    (let [tmp (str (System/getProperty "java.io.tmpdir") "/envrc-test-global-" (System/currentTimeMillis))]
      (try
        (.mkdirs (java.io.File. tmp))
        (spit (str tmp "/envrc.edn") "{:packages [:git]}")
        (spit (str tmp "/envrc.json") "{\"packages\":[]}")
        (with-redefs [econf/xdg-config-dir (fn [] tmp)
                      d/load-project       (fn [_] ["/tmp/fake-project" {:packages []}])
                      envrc.plugin/default-roots empty-roots]
          (is (some? (d/load-config "/tmp"))))
        (finally
          (fs/delete-tree tmp))))))

(deftest yaml-extension-reported-in-error-message
  (testing ".yaml extension (not .yml) appears in duplicate-key error message"
    (let [tmp (str (System/getProperty "java.io.tmpdir") "/envrc-test-yaml-ext-" (System/currentTimeMillis))]
      (try
        (.mkdirs (java.io.File. tmp))
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
        (is (= "vertical" (get-in cfg [:use :konsole :split])))
        (is (= 30        (get-in cfg [:use :konsole :size])))))))

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
        (is (nil? (:commands cfg)))))))

(deftest commands-key-rejected-with-suggestion
  (let [tmp (str (fs/create-temp-dir))]
    (spit (str tmp "/envrc.edn") "{:commands {test {:help \"x\"}}}")
    (with-redefs [envrc.data/load-global (constantly [tmp {}])
                  envrc.plugin/default-roots empty-roots]
      (let [err (try (d/load-config tmp) (catch Exception e (ex-data e)))]
        (is (= :unknown-key (:reason err)))
        (is (= :commands    (:key err)))
        (is (some? (:suggestion err)))))))

(deftest task-field-renames
  (let [tmp (str (fs/create-temp-dir))]
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
        (is (= :help (:key err)))
        (is (some? (:suggestion err)))))))

(deftest services-key-rejected
  (let [tmp (str (fs/create-temp-dir))]
    (spit (str tmp "/envrc.edn") "{:services {db {:run [\"pg\"]}}}")
    (with-redefs [envrc.data/load-global (constantly [tmp {}])
                  envrc.plugin/default-roots empty-roots]
      (let [err (try (d/load-config tmp) (catch Exception e (ex-data e)))]
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
          (is (= :invalid-use-slot (:reason err)))
          (is (some? (:errors err))))))))
