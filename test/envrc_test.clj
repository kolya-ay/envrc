(ns envrc-test
  (:require [clojure.test :refer [deftest is testing]]))

(require '[envrc :as e])
(require '[babashka.fs :as fs])
(require '[babashka.process :as p])


(require '[envrc.data :as kc])
(require '[envrc.plugin])
(require '[envrc.git])

(def ^:private empty-roots
  (fn [_] {:global "/nonexistent-global" :project "/nonexistent-project"}))

(deftest normalize-string-keyword-pane-reference
  (let [global {:tasks {:dev {:show {:pane "side"}}}}
        normed (#'kc/normalize-tokens global)]
    (is (= :side (get-in normed [:tasks :dev :show :pane])))))

(deftest normalize-leaves-run-strings-alone
  (let [global {:tasks {:dev {:run ["bun run dev"]}}}
        normed (#'kc/normalize-tokens global)]
    (is (= ["bun run dev"] (get-in normed [:tasks :dev :run])))))

(deftest load-config-merges-commands-from-global-and-project
  ;; project :tasks extends, not replaces, global :tasks so global-defined
  ;; tasks remain available alongside project-defined commands.
  (with-redefs [kc/load-global (fn [] ["/tmp/fake-global" {:tasks {:lint {:run ["eslint"]}
                                                                   :fmt  {:run ["prettier"]}}}])
                kc/load-project (fn [_] ["/tmp/fake-project" {:tasks {:dev {:run ["bun" "dev"]}}}])
                envrc.plugin/default-roots empty-roots
                envrc.git/branch (constantly "main")
                envrc.git/main-branch (constantly "main")]
    (let [cfg (kc/load-config "/tmp/fake")]
      (is (= #{:lint :fmt :dev} (set (keys (:tasks cfg))))))))

(deftest build-invocation-resolves-run-with-direnv-wrap
  (let [cfg {:tasks {:dev {:run ["bun" "run" "dev"]}}
             :use {:aliases {:dev :dev}}}
        argv (#'e/build-invocation cfg "dev" {:toplevel "/tmp/proj"} [])]
    (is (= ["direnv" "exec" "/tmp/proj" "bun" "run" "dev"] argv))))

(deftest build-invocation-appends-extra-args
  (let [cfg {:tasks {:dev {:run ["echo" "$@"]}}
             :use {:aliases {:dev :dev}}}
        argv (#'e/build-invocation cfg "dev" {:toplevel "/tmp/proj"} ["a" "b c"])]
    (is (= ["direnv" "exec" "/tmp/proj" "echo" "$@" "a" "b c"] argv))))

(deftest build-invocation-unaliased-task-errors
  (let [cfg {:tasks {:setup {:run ["bun" "install"]}}}]
    (is (thrown-with-msg? Exception #"unknown command `setup`"
          (#'e/build-invocation cfg "setup" {:toplevel "/tmp/proj"} [])))))

(deftest build-invocation-string-run-passed-verbatim
  (let [cfg {:tasks {:dev {:run "bun run dev"}}
             :use {:aliases {:dev :dev}}}
        argv (#'e/build-invocation cfg "dev" {:toplevel "/tmp/proj"} [])]
    (is (= ["direnv" "exec" "/tmp/proj" "bash" "-c" "bun run dev" "--"] argv))))

(deftest build-invocation-unknown-command-errors
  (let [cfg {:tasks {:dev {:run ["x"]}} :use {:aliases {:dev :dev}}}]
    (is (thrown-with-msg? Exception #"unknown command `bogus`"
          (#'e/build-invocation cfg "bogus" {:toplevel "/tmp/proj"} [])))))

(require '[cheshire.core])

(deftest config-json-empty-when-no-config-key
  (let [cfg {}]
    (is (= "{}" (#'e/config-json cfg false)))))

(deftest config-json-emits-resolved-payload
  (let [cfg {:config {:db "postgres" :nested {:x 1}}}
        out (#'e/config-json cfg false)
        parsed (cheshire.core/parse-string out true)]
    (is (= "postgres" (:db parsed)))
    (is (= 1 (:x (:nested parsed))))))

(deftest config-json-pretty-adds-whitespace
  (let [cfg {:config {:a 1}}]
    (is (re-find #"\n" (#'e/config-json cfg true)))))

(deftest watch-list-empty-dir
  (let [dir (str (fs/create-temp-dir))]
    (is (= "" (#'e/watch-paths dir)))))

(deftest watch-list-includes-existing-envrc-files
  (let [dir (str (fs/create-temp-dir))]
    (spit (str dir "/envrc.edn") "{}")
    (spit (str dir "/envrc.yml") "{}")
    (let [out (#'e/watch-paths dir)]
      (is (re-find (re-pattern "envrc.edn") out))
      (is (re-find (re-pattern "envrc.yml") out)))))

(deftest watch-list-includes-resolved-config-pointer
  (let [dir (str (fs/create-temp-dir))]
    (spit (str dir "/envrc.edn") "{:config \"./conf.yml\"}")
    (spit (str dir "/conf.yml")  "k: v\n")
    (let [out (#'e/watch-paths dir)]
      (is (re-find (re-pattern "envrc.edn") out))
      (is (re-find (re-pattern "conf.yml") out)))))

