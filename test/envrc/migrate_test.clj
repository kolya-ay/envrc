(ns envrc.migrate-test
  (:require [clojure.test :refer [deftest is]]
            [envrc.migrate :as m]
            [envrc.dirs :as dirs]
            [babashka.fs :as fs]
            [clojure.string :as str]))

(defn- mktmp []
  (str (fs/create-temp-dir)))

(deftest plan-registry-move
  (let [tmp (mktmp)]
    (try
      (fs/create-dirs (str tmp "/.local/state"))
      (spit (str tmp "/.local/state/projects.json") "{}")
      (binding [dirs/*env-overrides* {"HOME" tmp "XDG_STATE_HOME" (str tmp "/.local/state")}]
        (let [plan (m/plan {:home tmp})]
          (is (some #(and (= (str tmp "/.local/state/projects.json") (:from %))
                          (= (str tmp "/.local/state/envrc/projects.json") (:to %)))
                    plan))))
      (finally (fs/delete-tree tmp)))))

(deftest apply-moves-registry-file
  (let [tmp (mktmp)]
    (try
      (fs/create-dirs (str tmp "/.local/state"))
      (spit (str tmp "/.local/state/projects.json") "{}")
      (binding [dirs/*env-overrides* {"HOME" tmp "XDG_STATE_HOME" (str tmp "/.local/state")}]
        (m/apply! {:home tmp} (m/plan {:home tmp})))
      (is (fs/exists? (str tmp "/.local/state/envrc/projects.json")))
      (is (not (fs/exists? (str tmp "/.local/state/projects.json"))))
      (finally (fs/delete-tree tmp)))))

(deftest plan-skips-when-nothing-to-migrate
  (let [tmp (mktmp)]
    (try
      (fs/create-dirs (str tmp "/.local/state"))
      (binding [dirs/*env-overrides* {"HOME" tmp "XDG_STATE_HOME" (str tmp "/.local/state")}]
        (is (= [] (m/plan {:home tmp}))))
      (finally (fs/delete-tree tmp)))))
