(ns envrc.cli-test
  (:require [clojure.test :refer [deftest is]]
            [babashka.fs :as fs]
            [envrc :as cli]
            [envrc :as envrc]
            [envrc.api :as envrc.api]))

(deftest build-invocation-resolves-alias-to-task
  (let [cfg {:tasks {:test {:run ["bb" "test"]}}
             :use {:aliases {:t :test}}}
        invocation (#'cli/build-invocation cfg "t" {:toplevel "/repo"} [])]
    (is (= ["direnv" "exec" "/repo" "bb" "test"] invocation))))

(deftest build-invocation-applies-task-env
  (let [cfg {:tasks {:test {:run ["sh" "-c" "test \"$FOO\" = bar"]
                            :env {:FOO "bar"}}}
             :use {:aliases {:t :test}}}
        invocation (#'cli/build-invocation cfg "t" {:toplevel "/repo"} [])]
    (is (= ["direnv" "exec" "/repo" "env" "FOO=bar"
            "sh" "-c" "test \"$FOO\" = bar"]
           invocation))))

(deftest build-invocation-does-not-fallback-to-task-name
  (let [cfg {:tasks {:test {:run ["bb" "test"]}}
             :use {:aliases {}}}]
    (is (thrown-with-msg? Exception #"unknown command `test`"
          (#'cli/build-invocation cfg "test" {:toplevel "/repo"} [])))))

(deftest build-invocation-suggests-alias-not-hidden-task
  (let [cfg {:tasks {:test {:run ["bb" "test"]}
                     :hidden {:run ["hidden"]}}
             :use {:aliases {:t :test}}}]
    (try (#'cli/build-invocation cfg "te" {:toplevel "/repo"} [])
         (is false "expected throw")
         (catch Exception e
           (is (some #{"t"} (:available (ex-data e))))
           (is (not (some #{"hidden"} (:available (ex-data e)))))))))

(deftest apply-dispatches-to-handling-plugin
  (let [called (atom nil)
        cfg    {:plugins {"foo" {:id "foo"
                                 :handles #{:foo}
                                 :cli     {:apply (fn [_ args]
                                                    (reset! called args))}}}}]
    (envrc/dispatch-apply cfg ["foo" "extra1" "extra2"])
    (is (= {:label :foo :args ["extra1" "extra2"]} @called))))

(deftest apply-errors-on-unknown-label
  (let [cfg {:plugins {}}]
    (is (thrown? Exception (envrc/dispatch-apply cfg ["nonexistent"])))))

(deftest apply-fans-out-to-multiple-handlers
  (let [calls (atom 0)
        plug  (fn [id] {:id id :handles #{:multi}
                        :cli {:apply (fn [_ _] (swap! calls inc))}})
        cfg   {:plugins {"a" (plug "a") "b" (plug "b")}}]
    (envrc/dispatch-apply cfg ["multi"])
    (is (= 2 @calls))))

(deftest apply-binds-context-end-to-end
  ;; Verifies that dispatch-apply wraps handlers in envrc.api/with-context so
  ;; envrc.api/root returns the project root instead of nil.
  (let [root (str (fs/create-temp-dir))
        seen-root (atom nil)
        test-plugin {:id "test" :handles #{:foo}
                     :cli {:apply (fn [_ _] (reset! seen-root (envrc.api/root)))}}
        cfg {:plugins {"test" test-plugin}
             :dispatch {} :files {}}]
    (with-redefs [envrc/toplevel (constantly root)]
      (envrc/dispatch-apply cfg ["foo"]))
    (is (= root @seen-root)
        "envrc.api/root should equal the project root inside the apply handler")))

(deftest verb-alias-ls-resolves-to-list
  (let [calls (atom [])
        cfg   {:dispatch {"x" {:plugin   {:id "xp"}
                               :verbs    {:list (fn [_ {:keys [args]}]
                                                  (swap! calls conj [:list args]))}}}
               :plugins  {"xp" {}}}]
    (with-redefs [envrc/toplevel (constantly "/tmp")
                  envrc.api/build-context (constantly {})]
      (#'envrc/dispatch-capability cfg "x" "ls" {} ["a"]))
    (is (= [[:list ["a"]]] @calls))))

(deftest verb-lookup-falls-back-to-canonical-when-no-alias
  ;; When the typed verb isn't aliased, it's looked up canonically.
  ;; (Verifies the fallback path used by e.g. worktree's `:rm`.)
  (let [calls (atom [])
        cfg   {:dispatch {"x" {:plugin {:id "xp"}
                               :verbs  {:rm (fn [_ {:keys [args]}]
                                              (swap! calls conj [:rm args]))}}}
               :plugins  {"xp" {}}}]
    (with-redefs [envrc/toplevel (constantly "/tmp")
                  envrc.api/build-context (constantly {})]
      (#'envrc/dispatch-capability cfg "x" "rm" {} []))
    (is (= [[:rm []]] @calls))))

(deftest sync-not-in-builtin-subcommands
  (is (not (contains? @#'envrc/builtin-subcommands "sync"))))

(deftest dirs-not-in-builtin-subcommands
  (is (not (contains? @#'envrc/builtin-subcommands "dirs"))))

(deftest dispatch-apply-routes-to-plugin-with-matching-handles
  (let [calls (atom [])
        cfg   {:plugins {"wt" {:handles #{:link :copy}
                                :cli {:apply (fn [_ {:keys [label args]}]
                                                (swap! calls conj [label args]))}}}}]
    (with-redefs [envrc/toplevel (constantly "/tmp")
                  envrc.api/build-context (constantly {})]
      (#'envrc/dispatch-apply cfg ["link" "extra"]))
    (is (= [[:link ["extra"]]] @calls))))

(deftest dispatch-apply-errors-on-unknown-label
  (let [cfg {:plugins {"wt" {:handles #{:link}
                              :cli {:apply (fn [_ _] nil)}}}}]
    (with-redefs [envrc/toplevel (constantly "/tmp")
                  envrc.api/build-context (constantly {})]
      (is (thrown-with-msg? Exception #"unknown label `copy`"
            (#'envrc/dispatch-apply cfg ["copy"]))))))
