(ns envrc.ports-status-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]))

(def ^:private plugin-file
  (str (fs/canonicalize (fs/path (System/getProperty "user.dir") "plugins" "default" "ports.clj"))))

(defn- ports-var [sym]
  (load-file plugin-file)
  @(resolve (symbol "envrc.plugin.ports" (name sym))))

(def cfg
  {:project {:workspace "feature--auth"}
   :use {:ports {:base 4000
                 :stride 10
                 :vars [:AIST_PORT :UI_PORT]
                 :offset 7}}})

(deftest brief-status-renders-resolved-ports
  (let [status-impl (ports-var 'status-impl)
        out (with-out-str (status-impl cfg {:label :ports :brief? true}))]
    (is (str/includes? out "ports — offset=7"))
    (is (str/includes? out "AIST_PORT=4070"))
    (is (str/includes? out "UI_PORT=4071"))
    (is (not (str/includes? out "SERVER_URL")))))

(deftest verbose-status-renders-table
  (let [status-impl (ports-var 'status-impl)
        out (with-out-str (status-impl cfg {:label :ports :brief? false}))]
    (is (str/starts-with? out "Ports for feature--auth (offset 7):"))
    (is (str/includes? out "AIST_PORT"))
    (is (str/includes? out "4070"))
    (is (str/includes? out "UI_PORT"))
    (is (str/includes? out "4071"))
    (is (not (str/includes? out "SERVER_URL")))))

(deftest verbose-status-reports-unconfigured
  (is (= "Ports: not configured.\n"
         (with-out-str ((ports-var 'status-impl) {} {:label :ports :brief? false})))))
