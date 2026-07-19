(ns envrc.config-multiformat-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [babashka.fs :as fs]
            [envrc.config :as econf]
            [envrc.data :as kc]
            [envrc.plugin]))

;; Isolate from live `~/.config/envrc/` and `./.envrc/` plugin directories.
;; Same pattern as test.envrc.data-test/empty-roots.
(use-fixtures :each
  (fn [f]
    (with-redefs [envrc.plugin/default-roots
                  (fn [_] {:global  "/tmp/nonexistent-global"
                           :project "/tmp/nonexistent-project"})]
      (f))))

(defn- mk-layer
  "Create a tmp dir with the given filename->content map, return its path."
  [files]
  (let [dir (str (fs/create-temp-dir))]
    (doseq [[name content] files]
      (spit (str dir "/" name) content))
    dir))

(deftest load-layer-empty-dir
  (let [dir (mk-layer {})
        [layer _] (econf/load-layer dir)]
    (is (= {} layer))))

(deftest load-layer-edn-only
  (let [dir (mk-layer {"envrc.edn" "{:packages [bash]}"})
        [layer filenames] (econf/load-layer dir)]
    (is (= [:bash] (mapv keyword (get-in layer [:edn :packages]))))
    (is (nil? (:yaml layer)))
    (is (nil? (:json layer)))
    (is (= "envrc.edn" (:edn filenames)))))

(deftest load-layer-yaml-only
  (let [dir (mk-layer {"envrc.yml" "packages:\n  - bash\n"})
        [layer filenames] (econf/load-layer dir)]
    (is (= ["bash"] (get-in layer [:yaml :packages])))
    (is (= "envrc.yml" (:yaml filenames)))))

(deftest load-layer-yaml-extension-yaml
  (let [dir (mk-layer {"envrc.yaml" "packages:\n  - bash\n"})
        [layer filenames] (econf/load-layer dir)]
    (is (= ["bash"] (get-in layer [:yaml :packages])))
    (is (= "envrc.yaml" (:yaml filenames)))))

(deftest load-layer-json-only
  (let [dir (mk-layer {"envrc.json" "{\"packages\": [\"bash\"]}"})
        [layer _] (econf/load-layer dir)]
    (is (= ["bash"] (get-in layer [:json :packages])))))

(deftest load-layer-all-three-present
  (let [dir (mk-layer {"envrc.edn"  "{:packages [a]}"
                       "envrc.yml"  "commands:\n  dev:\n    script:\n      - echo dev\n"
                       "envrc.json" "{\"config\": {\"k\": 1}}"})
        [layer _] (econf/load-layer dir)]
    (is (some? (:edn layer)))
    (is (some? (:yaml layer)))
    (is (some? (:json layer)))))

(deftest load-layer-yaml-takes-precedence-over-yml-when-both-present
  (let [dir (mk-layer {"envrc.yaml" "packages:\n  - from-yaml\n"
                       "envrc.yml"  "packages:\n  - from-yml\n"})
        [layer _] (econf/load-layer dir)]
    (is (= ["from-yaml"] (get-in layer [:yaml :packages])))))

(deftest validate-single-file-allows-config-in-multiple
  (let [layer {:edn  {:config {:a 1}}
               :yaml {:config {:b 2}}}
        filenames {:edn "envrc.edn" :yaml "envrc.yml"}]
    (is (nil? (#'kc/validate-single-file! layer filenames "project")))))

(deftest validate-single-file-errors-name-three-files
  (let [layer {:edn  {:packages [:a]}
               :yaml {:packages [:b]}
               :json {:packages [:c]}}
        filenames {:edn "envrc.edn" :yaml "envrc.yml" :json "envrc.json"}]
    (is (thrown-with-msg? Exception #":packages appears in both .*envrc"
          (#'kc/validate-single-file! layer filenames "project")))))

(deftest collapse-layer-empty
  (is (= {} (econf/collapse-layer {}))))

(deftest collapse-layer-single-format
  (is (= {:packages [:bash]}
         (econf/collapse-layer {:yaml {:packages [:bash]}}))))

(deftest collapse-layer-edn-wins-over-yaml-for-config
  (let [layer {:edn  {:config {:a 1 :common 100}}
               :yaml {:config {:b 2 :common 200}}}]
    (is (= {:a 1 :b 2 :common 100}
           (:config (econf/collapse-layer layer))))))

(deftest collapse-layer-deep-merges-config-across-all-three
  (let [layer {:edn  {:config {:a 1}}
               :yaml {:config {:b 2}}
               :json {:config {:c 3 :a 999}}}]
    (is (= {:a 1 :b 2 :c 3}
           (:config (econf/collapse-layer layer))))))

(deftest load-config-merges-config-across-layers
  ;; global layer + project layer + project's :config string pointer
  (let [global-dir  (mk-layer {"envrc.json" "{\"config\": {\"region\": \"us-east\"}}"})
        project-dir (mk-layer {"envrc.edn" "{:config {:foo 1}}"
                               "envrc.yml" "config: ./conf.yml"})
        _           (spit (str project-dir "/conf.yml") "bar: 2\nbaz: 3\n")]
    ;; load-global returns [dir cfg] pair; real load-layer inside load-config
    ;; reaches the files in global-dir via the returned global-dir path
    (with-redefs [kc/load-global (fn []
                                   (let [[layer _] (econf/load-layer global-dir)]
                                     [global-dir (kc/normalize-tokens (econf/collapse-layer layer))]))]
      (let [cfg (kc/load-config project-dir)]
        (is (= {:region "us-east" :foo 1 :bar 2 :baz 3} (:config cfg)))))))

(deftest load-config-validates-project-single-file
  (let [project-dir (mk-layer {"envrc.edn" "{:packages [a]}"
                               "envrc.yml" "packages: [b]"})]
    (is (thrown-with-msg? Exception #"appears in both"
          (kc/load-config project-dir)))))
