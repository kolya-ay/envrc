(ns envrc.pane-test
  (:require [clojure.test :refer [deftest is]]
            [envrc.pane :as ep]))

(deftest first-token-strips-comments-and-builtins
  (is (= "bun"   (#'ep/first-token "bun run dev")))
  (is (= "bun"   (#'ep/first-token "# comment\nbun run dev")))
  (is (= "bun"   (#'ep/first-token "cd /tmp\nbun run dev")))
  (is (= "bun"   (#'ep/first-token "export FOO=1\nbun run dev")))
  (is (= "exec"  (#'ep/first-token "exec bash"))))

(deftest first-token-handles-blank-and-whitespace
  (is (nil? (#'ep/first-token "")))
  (is (nil? (#'ep/first-token "\n\n")))
  (is (= "ls" (#'ep/first-token "\n   ls"))))

(deftest matches-running-by-basename
  (is (true?  (#'ep/matches? "bun" "/nix/store/abc/bin/bun")))
  (is (true?  (#'ep/matches? "bun" "bun")))
  (is (false? (#'ep/matches? "bun" "node"))))

(deftest matches-handles-nil-fg
  (is (false? (#'ep/matches? "bun" nil))))

(deftest decide-action-from-pane-state
  (is (= :spawn   (ep/decide nil "bun")))
  (is (= :focus   (ep/decide "/usr/bin/bun" "bun")))
  (is (= :restart (ep/decide "/usr/bin/node" "bun"))))
