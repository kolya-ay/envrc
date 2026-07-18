(ns envrc.templates-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]))

(def ^:private plugin-file
  (str (fs/canonicalize (fs/path (System/getProperty "user.dir") "plugins" "default" "templates.clj"))))

(defn- plugin-var [sym]
  (load-file plugin-file)
  (resolve (symbol "envrc.plugin.templates" (name sym))))

(deftest cli-list-prints-template-names-from-configured-dirs
  (let [root (fs/create-temp-dir)
        dir-a (str (fs/path root "a"))
        dir-b (str (fs/path root "b"))]
    (fs/create-dirs (fs/path dir-a "alpha"))
    (fs/create-dirs (fs/path dir-b "beta"))
    (is (= "alpha\nbeta\n"
           (with-out-str
             ((deref (plugin-var 'cli-list))
              {:use {:templates {:dirs [dir-a dir-b]}}}
              {}))))))

(deftest cli-path-prints-resolved-template-dir
  (let [root (fs/create-temp-dir)
        dir-a (str (fs/path root "templates"))
        wanted (str (fs/path dir-a "alpha"))]
    (fs/create-dirs wanted)
    (is (= (str wanted "\n")
           (with-out-str
             ((deref (plugin-var 'cli-path))
              {:use {:templates {:dirs [dir-a]}}}
              {:args ["alpha"]}))))))

(deftest cli-path-throws-on-unknown-template
  (let [root (fs/create-temp-dir)
        dir-a (str (fs/path root "templates"))]
    (fs/create-dirs dir-a)
    (is (thrown-with-msg? Exception #"unknown template"
          ((deref (plugin-var 'cli-path))
           {:use {:templates {:dirs [dir-a]}}}
           {:args ["missing"]})))))
