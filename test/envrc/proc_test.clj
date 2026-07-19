(ns envrc.proc-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]
            [envrc.proc :as proc]))

(deftest read-environ-self
  (let [env (proc/read-environ (.pid (java.lang.ProcessHandle/current)))]
    (is (map? env))
    (is (contains? env "PATH"))))

(deftest read-environ-missing
  (is (nil? (proc/read-environ 99999999))))

(deftest parent-pid-returns-nil-on-unreadable
  ;; PID 0 doesn't exist in /proc
  (is (nil? (proc/parent-pid 0))))

(deftest parent-pid-handles-comm-with-spaces
  (with-redefs [fs/read-all-bytes (constantly (.getBytes "1234 (weird (comm) name) S 567 1 1 0 -1 ..." "UTF-8"))]
    (is (= 567 (proc/parent-pid 1234)))))

(deftest parent-pid-handles-comm-with-parens
  (with-redefs [fs/read-all-bytes (constantly (.getBytes "42 (a)b)c) S 99 1 ..." "UTF-8"))]
    (is (= 99 (proc/parent-pid 42)))))

(deftest parent-pid-integration-against-real-proc
  ;; PID 1 always exists on Linux. Its parent is 0.
  (is (= 0 (proc/parent-pid 1))))

(deftest walk-parents-integration-from-self
  ;; Self should appear first; walk should terminate.
  (let [chain (vec (proc/walk-parents (.pid (java.lang.ProcessHandle/current))))]
    (is (seq chain))
    (is (= (.pid (java.lang.ProcessHandle/current)) (:pid (first chain))))))

(deftest walk-parents-yields-pid-and-environ
  (with-redefs [proc/read-environ (fn [pid] (when (#{1 5 10} pid) {"PID" (str pid)}))
                proc/parent-pid   (fn [pid] (case pid 10 5, 5 1, 1 nil, nil))]
    (let [chain (proc/walk-parents 10)]
      (is (= [10 5 1] (mapv :pid chain)))
      (is (= "10" (-> chain first :environ (get "PID")))))))

(deftest walk-parents-yields-nil-environ-on-unreadable
  ;; Walk continues past unreadable environ so comm/exe-based detection at
  ;; later tiers can still run. The :environ is nil for that ancestor.
  (with-redefs [proc/read-environ (fn [pid] (when (= pid 10) {"X" "y"}))
                proc/parent-pid   (fn [pid] (case pid 10 5, 5 nil, nil))]
    (let [chain (vec (proc/walk-parents 10))]
      (is (= [10 5] (mapv :pid chain)))
      (is (= {"X" "y"} (-> chain first :environ)))
      (is (nil? (-> chain second :environ))))))

(deftest walk-parents-stops-at-pid-1
  (with-redefs [proc/read-environ (constantly {"X" "y"})
                proc/parent-pid   (fn [pid] (case pid 10 1, 1 nil, nil))]
    (is (= [10 1] (mapv :pid (proc/walk-parents 10))))))

(deftest agent-identity-finds-nearest-ancestor
  (with-redefs [proc/read-environ (fn [pid]
                                    (case pid
                                      10 {"ENVRC_AGENT" "irrelevant-self"}
                                      5  {"ENVRC_AGENT" "claude"}
                                      1  {"ENVRC_AGENT" "init-shouldnt-match"}
                                      nil))
                proc/parent-pid   (fn [pid] (case pid 10 5, 5 1, 1 nil, nil))]
    ;; Walker yields self first; agent-identity stops at the first match.
    (is (= {:agent "irrelevant-self" :pid 10} (proc/agent-identity 10)))))

(deftest agent-identity-walks-to-find-match
  (with-redefs [proc/read-environ (fn [pid]
                                    (case pid
                                      10 {"HOME" "/x"}
                                      5  {"HOME" "/x"}
                                      1  {"ENVRC_AGENT" "claude"}
                                      nil))
                proc/parent-pid   (fn [pid] (case pid 10 5, 5 1, 1 nil, nil))]
    ;; PID 1 stops the walk because parent-pid returns nil; but PID 1 itself
    ;; is included with environ {"ENVRC_AGENT" "claude"}.
    (is (= {:agent "claude" :pid 1} (proc/agent-identity 10)))))

(deftest agent-identity-nil-when-no-match
  (with-redefs [proc/read-environ (constantly {"HOME" "/x"})
                proc/detect-agent (constantly nil)
                proc/parent-pid   (fn [pid] (case pid 10 1, 1 nil, nil))]
    (is (nil? (proc/agent-identity 10)))))

;; --- detect-agent --------------------------------------------------------

(deftest detect-agent-matches-known-binary-shapes
  ;; Bare, dot-prefixed wrapper, -wrapped suffix, -cli suffix — exact match
  ;; after normalization. Non-agents return nil.
  (doseq [[name expected] {"claude"          "claude"
                           ".claude-wrapped" "claude"
                           "claude-cli"      "claude"
                           "goose"           "goose"
                           "gemini"          "gemini"
                           "codex"           "codex"
                           "amp"             "amp"
                           "aider"           "aider"
                           "bash"            nil
                           "lamp"            nil
                           "tamper"          nil
                           "samples"         nil}]
    (with-redefs [proc/read-exe-basename (constantly name)
                  proc/read-comm         (constantly nil)]
      (is (= expected (proc/detect-agent 1))
          (str "name=" name " expected=" expected)))))

(deftest detect-agent-falls-back-to-comm-when-exe-unreadable
  (with-redefs [proc/read-exe-basename (constantly nil)
                proc/read-comm         (constantly ".claude-wrapped")]
    (is (= "claude" (proc/detect-agent 1)))))

(deftest detect-agent-nil-when-both-unreadable
  (with-redefs [proc/read-exe-basename (constantly nil)
                proc/read-comm         (constantly nil)]
    (is (nil? (proc/detect-agent 1)))))

;; --- agent-identity comm/exe fallback ------------------------------------

(deftest agent-identity-falls-back-to-detect-agent-when-no-env
  ;; No ENVRC_AGENT in any ancestor's environ. detect-agent matches at pid 5.
  (with-redefs [proc/read-environ (constantly {"HOME" "/x"})
                proc/parent-pid   (fn [pid] (case pid 10 5, 5 1, 1 nil, nil))
                proc/detect-agent (fn [pid] (when (= pid 5) "claude"))]
    (is (= {:agent "claude" :pid 5} (proc/agent-identity 10)))))

(deftest agent-identity-env-tier-wins-over-comm-tier
  ;; Env var on the current pid; detect-agent would also match at ancestor.
  (with-redefs [proc/read-environ (fn [pid] (when (= pid 10) {"ENVRC_AGENT" "from-env"}))
                proc/parent-pid   (fn [pid] (case pid 10 5, 5 nil, nil))
                proc/detect-agent (fn [pid] (when (= pid 10) "from-comm"))]
    (is (= {:agent "from-env" :pid 10} (proc/agent-identity 10)))))
