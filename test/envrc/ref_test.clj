(ns envrc.ref-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]))

(load-file "plugins/default/ref.clj")

(defn- ref-var [sym]
  (resolve (symbol "envrc.plugin.ref" (name sym))))

(deftest manifest-shape
  (let [plugin @(ref-var 'plugin)]
    (is (= "ref" (:id plugin)))
    (is (= #{:ref} (:handles plugin)))
    (is (fn? (get-in plugin [:cli :apply])))
    (is (fn? (get-in plugin [:cli :status])))
    (is (map? (get-in plugin [:provides :tasks])))))

(deftest apply-creates-symlink-when-absent
  (let [root    (str (fs/create-temp-dir))
        state   (str (fs/create-temp-dir) "/state-ref")
        apply-impl (ref-var 'apply-impl)
        cfg     {:files {} :project {:scope :ego :slug "p" :workspace "main"}
                 :use   {:ref {:root ".myref"}}}]
    (with-redefs [envrc.api/root (constantly root)
                  envrc.dirs/categorical-dir (fn [_ _] state)]
      (apply-impl cfg {}))
    (is (fs/sym-link? (str root "/.myref")))
    (is (= state (str (fs/read-link (str root "/.myref")))))))

(deftest apply-mirrors-ref-paths
  (let [root  (str (fs/create-temp-dir))
        state (str (fs/create-temp-dir) "/state-ref")
        apply-impl (ref-var 'apply-impl)
        cfg   {:files {:specs ["specs/"] :ref [:specs]}
               :project {:scope :ego :slug "p" :workspace "main"}
               :use {:ref {:root ".ref"}}}]
    (fs/create-dirs (str root "/specs"))
    (spit (str root "/specs/x.md") "content")
    (with-redefs [envrc.api/root (constantly root)
                  envrc.dirs/categorical-dir (fn [_ _] state)
                  envrc.git/main-checkout?   (constantly true)]
      (apply-impl cfg {}))
    (is (fs/exists? (str root "/.ref/specs/x.md")))
    (is (not (fs/exists? (str root "/.ref/envrc/specs/x.md"))))))

(deftest apply-migrates-real-dir-destructively
  (let [root  (str (fs/create-temp-dir))
        state (str (fs/create-temp-dir) "/state-ref")
        apply-impl (ref-var 'apply-impl)
        cfg   {:files {} :project {:scope :ego :slug "p" :workspace "main"}
               :use {:ref {:root ".ref"}}}]
    (fs/create-dirs (str root "/.ref"))
    (spit (str root "/.ref/keep.md") "user-content")
    (with-redefs [envrc.api/root (constantly root)
                  envrc.dirs/categorical-dir (fn [_ _] state)]
      (apply-impl cfg {}))
    (is (fs/sym-link? (str root "/.ref")))
    (is (fs/exists? (str state "/keep.md")))
    (is (= "user-content" (slurp (str state "/keep.md"))))))

(deftest apply-impl-uses-root-override
  (let [override-root (str (fs/create-temp-dir))
        state         (str (fs/create-temp-dir) "/state-ref")
        apply-impl    (ref-var 'apply-impl)]
    (with-redefs [envrc.api/root             (constantly "/should-not-be-used")
                  envrc.dirs/categorical-dir (fn [_ _] state)
                  envrc.git/main-checkout?   (constantly true)]
      (apply-impl {:files {} :project {:scope :ego :slug "p" :workspace "main"}
                   :use   {:ref {:root ".ref"}}}
                  {:root override-root}))
    (is (fs/sym-link? (str override-root "/.ref")))))

(deftest ensure-symlink-creates-when-absent
  (let [root          (str (fs/create-temp-dir))
        target        (str (fs/create-temp-dir) "/target")
        ensure-symlink! (ref-var 'ensure-symlink!)]
    (fs/create-dirs target)
    (ensure-symlink! {:local (str root "/.ref") :target target})
    (is (fs/sym-link? (str root "/.ref")))
    (is (= target (str (fs/read-link (str root "/.ref")))))))

(deftest ensure-symlink-no-op-when-already-correct
  (let [root          (str (fs/create-temp-dir))
        target        (str (fs/create-temp-dir) "/target")
        ensure-symlink! (ref-var 'ensure-symlink!)]
    (fs/create-dirs target)
    (fs/create-sym-link (str root "/.ref") target)
    (ensure-symlink! {:local (str root "/.ref") :target target})
    (is (= target (str (fs/read-link (str root "/.ref")))))))

(deftest ensure-symlink-does-not-create-target
  (let [root            (str (fs/create-temp-dir))
        target          (str (fs/create-temp-dir) "/absent-target")
        ensure-symlink! (ref-var 'ensure-symlink!)]
    (ensure-symlink! {:local (str root "/.ref") :target target})
    (is (not (fs/exists? target)))))

(deftest apply-impl-skips-mirror-from-worktree
  (let [root  (str (fs/create-temp-dir))
        state (str (fs/create-temp-dir) "/state-ref")
        apply-impl (ref-var 'apply-impl)]
    (fs/create-dirs (str root "/specs"))
    (spit (str root "/specs/x.md") "content")
    (with-redefs [envrc.api/root             (constantly root)
                  envrc.dirs/categorical-dir (fn [_ _] state)
                  envrc.git/main-checkout?   (constantly false)]
      (apply-impl {:files {:specs ["specs/"] :ref [:specs]}
                   :project {:scope :ego :slug "p" :workspace "main"}
                   :use {:ref {:root ".ref"}}}
                  {}))
    (is (fs/sym-link? (str root "/.ref")))
    (is (not (fs/exists? (str state "/specs"))))))

(deftest apply-impl-mirrors-from-main
  (let [root  (str (fs/create-temp-dir))
        state (str (fs/create-temp-dir) "/state-ref")
        apply-impl (ref-var 'apply-impl)]
    (fs/create-dirs (str root "/specs"))
    (spit (str root "/specs/x.md") "content")
    (with-redefs [envrc.api/root             (constantly root)
                  envrc.dirs/categorical-dir (fn [_ _] state)
                  envrc.git/main-checkout?   (constantly true)]
      (apply-impl {:files {:specs ["specs/"] :ref [:specs]}
                   :project {:scope :ego :slug "p" :workspace "main"}
                   :use {:ref {:root ".ref"}}}
                  {}))
    (is (fs/exists? (str state "/specs/x.md")))))

(deftest provides-workspace-new-subscriber
  (let [plugin @(ref-var 'plugin)
        task   (get-in plugin [:provides :tasks :ref-on-workspace-new])]
    (is (= :workspace-new (:on task)))
    (is (true? (:tolerant task)))
    (is (fn? (:run task)))))

(deftest workspace-new-subscriber-no-ops-when-target-absent
  (let [dst    (str (fs/create-temp-dir))
        target (str (fs/create-temp-dir) "/never-created")
        plugin @(ref-var 'plugin)
        run-fn (get-in plugin [:provides :tasks :ref-on-workspace-new :run])]
    (with-redefs [envrc.dirs/categorical-dir (fn [_ _] target)]
      (let [out (with-out-str
                  (run-fn {:cfg     {:use {:ref {:root ".ref"}}}
                           :payload {:dst dst :ctx {:scope :ego :slug "p"}}}))]
        (is (re-find #"absent" out))
        (is (not (fs/exists? (str dst "/.ref"))))))))

(deftest workspace-new-subscriber-links-when-target-present
  (let [dst    (str (fs/create-temp-dir))
        target (str (fs/create-temp-dir) "/state-ref")
        plugin @(ref-var 'plugin)
        run-fn (get-in plugin [:provides :tasks :ref-on-workspace-new :run])]
    (fs/create-dirs target)
    (with-redefs [envrc.dirs/categorical-dir (fn [_ _] target)]
      (run-fn {:cfg     {:use {:ref {:root ".ref"}}}
               :payload {:dst dst :ctx {:scope :ego :slug "p"}}}))
    (is (fs/sym-link? (str dst "/.ref")))
    (is (= target (str (fs/read-link (str dst "/.ref")))))))

(deftest apply-impl-throws-on-unknown-state
  (let [root  (str (fs/create-temp-dir))
        state (str (fs/create-temp-dir) "/state-ref")
        apply-impl (ref-var 'apply-impl)]
    (spit (str root "/.ref") "regular file (not dir, not symlink)")
    (with-redefs [envrc.api/root             (constantly root)
                  envrc.dirs/categorical-dir (fn [_ _] state)]
      (is (thrown? Exception
                   (apply-impl {:files {} :project {:scope :ego :slug "p" :workspace "main"}
                                :use   {:ref {:root ".ref"}}}
                               {}))))))

(deftest status-impl-notes-mirror-managed-from-main
  (let [root  (str (fs/create-temp-dir))
        state (str (fs/create-temp-dir) "/state-ref")
        status-impl (ref-var 'status-impl)]
    (with-redefs [envrc.api/root             (constantly root)
                  envrc.dirs/categorical-dir (fn [_ _] state)
                  envrc.git/main-checkout?   (constantly false)]
      (let [out (with-out-str
                  (status-impl {:files {} :project {:scope :ego :slug "p" :workspace "main"}
                                :use   {:ref {:root ".ref"}}}
                               {:brief? false}))]
        (is (re-find #"mirror managed from main checkout" out))))))

(deftest apply-mirrors-to-subdir-when-mirror-set
  (let [root  (str (fs/create-temp-dir))
        state (str (fs/create-temp-dir) "/state-ref")
        apply-impl (ref-var 'apply-impl)
        cfg   {:files {:specs ["specs/"] :ref [:specs]}
               :project {:scope :ego :slug "p" :workspace "main"}
               :use {:ref {:root ".ref"} :dirs {:ref {:mirror "envrc"}}}}]
    (fs/create-dirs (str root "/specs"))
    (spit (str root "/specs/x.md") "content")
    (with-redefs [envrc.api/root             (constantly root)
                  envrc.dirs/categorical-dir (fn [_ _] state)
                  envrc.git/main-checkout?   (constantly true)]
      (apply-impl cfg {}))
    (is (fs/exists? (str root "/.ref/envrc/specs/x.md")))
    (is (not (fs/exists? (str root "/.ref/specs/x.md"))))))

(deftest apply-warns-on-basename-collision
  (let [root  (str (fs/create-temp-dir))
        state (str (fs/create-temp-dir) "/state-ref")
        apply-impl (ref-var 'apply-impl)
        cfg   {:files {:ref ["a/conf.edn" "b/conf.edn"]}
               :project {:scope :ego :slug "p" :workspace "main"}
               :use {:ref {:root ".ref"}}}]
    (fs/create-dirs (str root "/a"))
    (fs/create-dirs (str root "/b"))
    (spit (str root "/a/conf.edn") "AAA")
    (spit (str root "/b/conf.edn") "BBB")
    (let [err (with-redefs [envrc.api/root             (constantly root)
                            envrc.dirs/categorical-dir (fn [_ _] state)
                            envrc.git/main-checkout?   (constantly true)]
                (binding [*err* (java.io.StringWriter.)]
                  (apply-impl cfg {})
                  (str *err*)))]
      (is (re-find #"conf\.edn" err))
      (is (re-find #"last wins" err))
      (is (fs/exists? (str root "/.ref/conf.edn")))
      (is (= "BBB" (slurp (str root "/.ref/conf.edn")))))))

(deftest status-impl-reports-collision-count
  (let [root  (str (fs/create-temp-dir))
        state (str (fs/create-temp-dir) "/state-ref")
        status-impl (ref-var 'status-impl)
        cfg   {:files {:ref ["a/conf.edn" "b/conf.edn"]}
               :project {:scope :ego :slug "p" :workspace "main"}
               :use {:ref {:root ".ref"}}}]
    (with-redefs [envrc.api/root             (constantly root)
                  envrc.dirs/categorical-dir (fn [_ _] state)
                  envrc.git/main-checkout?   (constantly true)]
      (let [out (with-out-str (status-impl cfg {:brief? false}))]
        (is (re-find #"\d+ basename collision" out))))))
