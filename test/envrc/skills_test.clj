(ns envrc.skills-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]))

(def ^:private plugin-path
  "plugins/default/skills.clj")

(defn- skills-var [sym]
  (load-file plugin-path)
  (resolve (symbol "envrc.plugin.skills" (name sym))))

(def ^:private cfg
  {:use {:skills {:dirs ["resources/skills"]}}})

(deftest list-skills
  (let [list-skills @(skills-var 'list-skills)
        skills (list-skills cfg)]
    (is (vector? skills))
    (is (some #{"core"} skills))))

(deftest get-skill-known
  (let [get-skill @(skills-var 'get-skill)
        content (get-skill cfg "core")]
    (is (string? content))
    (is (pos? (count content)))))

(deftest get-skill-unknown
  (let [get-skill @(skills-var 'get-skill)]
    (is (thrown? Exception (get-skill cfg "no-such-skill")))))
