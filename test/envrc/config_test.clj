(ns envrc.config-test
  (:require [clojure.test :refer [deftest is testing]]
            [babashka.fs :as fs]))

(require '[envrc.config :as ec])

(deftest resolve-inline-map-passes-through
  (is (= {:db "postgres"} (#'ec/resolve-value {:db "postgres"} "/tmp"))))

(deftest resolve-string-pointer-loads-yaml
  (let [dir (str (fs/create-temp-dir))
        f   (str dir "/conf.yml")]
    (spit f "key: yamlvalue\n")
    (is (= {:key "yamlvalue"} (#'ec/resolve-value "./conf.yml" dir)))))

(deftest resolve-string-pointer-loads-edn
  (let [dir (str (fs/create-temp-dir))
        f   (str dir "/conf.edn")]
    (spit f "{:key \"ednvalue\"}")
    (is (= {:key "ednvalue"} (#'ec/resolve-value "./conf.edn" dir)))))

(deftest resolve-string-pointer-loads-json
  (let [dir (str (fs/create-temp-dir))
        f   (str dir "/conf.json")]
    (spit f "{\"key\": \"jsonvalue\"}")
    (is (= {:key "jsonvalue"} (#'ec/resolve-value "./conf.json" dir)))))

(deftest resolve-missing-file-errors
  (is (thrown-with-msg? Exception #"file not found"
        (#'ec/resolve-value "./nonexistent.yml" "/tmp"))))

(deftest resolve-unsupported-extension-errors
  (let [dir (str (fs/create-temp-dir))
        f   (str dir "/conf.toml")]
    (spit f "k = 1")
    (is (thrown-with-msg? Exception #"unsupported extension"
          (#'ec/resolve-value "./conf.toml" dir)))))

(deftest resolve-non-map-top-level-errors
  (let [dir (str (fs/create-temp-dir))
        f   (str dir "/conf.yml")]
    (spit f "- a\n- b\n- c\n")  ; YAML top-level list
    (is (thrown-with-msg? Exception #"top-level map"
          (#'ec/resolve-value "./conf.yml" dir)))))

(deftest resolve-all-deep-merges-multiple-inline-sources
  (is (= {:a 1 :b 2 :nested {:x 1 :y 2}}
         (ec/resolve-all [["/" {:a 1 :nested {:x 1}}]
                          ["/" {:b 2 :nested {:y 2}}]]))))

(deftest resolve-all-later-source-wins-at-leaf
  (is (= {:a 99} (ec/resolve-all [["/" {:a 1}] ["/" {:a 99}]]))))

(deftest resolve-all-empty-returns-empty-map
  (is (= {} (ec/resolve-all []))))

(deftest resolve-all-skips-nil-values
  (is (= {:a 1} (ec/resolve-all [["/" nil] ["/" {:a 1}] ["/" nil]]))))
