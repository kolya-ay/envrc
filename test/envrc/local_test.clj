(ns envrc.local-test
  (:require [clojure.test :refer [deftest is]]
            [envrc.files :as files]
            [babashka.fs :as fs]))

(load-file "plugins/default/local.clj")

(defn- mock-cfg [root files-dict]
  {:files files-dict
   :project {:scope :ego :slug "p" :workspace "main"}
   :plugins {"local" @(resolve 'envrc.plugin.local/plugin)}})

(deftest apply-impl-resolves-and-appends
  (let [root (str (fs/create-temp-dir))
        apply-impl (resolve 'envrc.plugin.local/apply-impl)]
    (fs/create-dirs (str root "/.git/info"))
    (let [cfg (assoc (mock-cfg root {:envrc ["envrc.edn"]
                                     :local [:envrc]})
                    ::root root)]
      (with-redefs [envrc.api/root (constantly root)]
        (@apply-impl cfg {}))
      (is (re-find #"envrc\.edn"
                   (slurp (str root "/.git/info/exclude")))))))

(deftest apply-impl-no-op-on-empty-label
  (let [root (str (fs/create-temp-dir))
        apply-impl (resolve 'envrc.plugin.local/apply-impl)]
    (fs/create-dirs (str root "/.git/info"))
    (with-redefs [envrc.api/root (constantly root)]
      (@apply-impl (mock-cfg root {}) {}))
    (is (or (not (fs/exists? (str root "/.git/info/exclude")))
            (= "" (slurp (str root "/.git/info/exclude")))))))

(deftest plugin-manifest-shape
  (let [plugin @(resolve 'envrc.plugin.local/plugin)]
    (is (= "local" (:id plugin)))
    (is (= #{:local} (:handles plugin)))
    (is (fn? (get-in plugin [:cli :apply])))
    (is (fn? (get-in plugin [:cli :status])))))

(deftest apply-impl-uses-root-override
  (let [override-root (str (fs/create-temp-dir))
        apply-impl    (resolve 'envrc.plugin.local/apply-impl)]
    (fs/create-dirs (str override-root "/.git/info"))
    (with-redefs [envrc.api/root (constantly "/should-not-be-used")]
      (@apply-impl (mock-cfg override-root {:envrc ["envrc.edn"] :local [:envrc]})
                   {:root override-root}))
    (is (re-find #"envrc\.edn"
                 (slurp (str override-root "/.git/info/exclude"))))))

(deftest apply-impl-returns-added-lines
  (let [root (str (fs/create-temp-dir))
        apply-impl (resolve 'envrc.plugin.local/apply-impl)]
    (fs/create-dirs (str root "/.git/info"))
    (with-redefs [envrc.api/root (constantly root)]
      (is (= ["envrc.edn"]
             (@apply-impl (mock-cfg root {:envrc ["envrc.edn"] :local [:envrc]})
                          {}))))))

(deftest provides-workspace-new-subscriber
  (let [plugin @(resolve 'envrc.plugin.local/plugin)
        task   (get-in plugin [:provides :tasks :local-on-workspace-new])]
    (is (= :workspace-new (:on task)))
    (is (true? (:tolerant task)))
    (is (fn? (:run task)))))

(deftest workspace-new-subscriber-writes-to-dst
  (let [main-root  (str (fs/create-temp-dir))
        dst        (str (fs/create-temp-dir))
        plugin     @(resolve 'envrc.plugin.local/plugin)
        run-fn     (get-in plugin [:provides :tasks :local-on-workspace-new :run])]
    (fs/create-dirs (str main-root "/.git/info"))
    (fs/create-dirs (str dst "/.git/info"))
    (with-redefs [envrc.api/root (constantly main-root)]
      (run-fn {:cfg     (mock-cfg main-root {:envrc ["envrc.edn"] :local [:envrc]})
               :payload {:dst dst :ctx {:scope :ego :slug "p"}}}))
    (is (re-find #"envrc\.edn"
                 (slurp (str dst "/.git/info/exclude"))))
    (is (not (re-find #"envrc\.edn"
                      (or (and (fs/exists? (str main-root "/.git/info/exclude"))
                               (slurp (str main-root "/.git/info/exclude")))
                          ""))))))
