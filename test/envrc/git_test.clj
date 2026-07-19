(ns envrc.git-test
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.test :refer [deftest is]]
            [envrc.git :as git]))

(deftest main-worktree-root-handles-relative-dotgit
  (with-redefs [p/shell (constantly {:exit 0 :out ".git\n" :err ""})]
    (let [root (git/main-worktree-root)]
      (is (string? root))
      (is (not (re-find #"/\.git$" root))))))

(deftest main-worktree-root-handles-absolute-dotgit
  (with-redefs [p/shell (constantly {:exit 0 :out "/tmp/repo/.git\n" :err ""})]
    (let [root (git/main-worktree-root)]
      (is (= "/tmp/repo" root)))))

(deftest main-worktree-root-nil-on-error
  (with-redefs [p/shell (constantly {:exit 128 :out "" :err "fatal"})]
    (is (nil? (git/main-worktree-root)))))

(deftest main-checkout?-true-when-paths-match
  (let [tmp (str (fs/canonicalize (fs/create-temp-dir)))]
    (with-redefs [git/main-worktree-root (constantly tmp)]
      (is (true? (git/main-checkout? tmp))))))

(def ^:private NUL (str (char 0)))

(deftest parse-porcelain-z-empty
  (is (= [] (#'git/parse-porcelain-z ""))))

(deftest parse-porcelain-z-untracked
  (is (= [{:op :carried :categories #{:untracked} :path "new.txt"}]
         (#'git/parse-porcelain-z (str "?? new.txt" NUL)))))

(deftest parse-porcelain-z-modified
  (is (= [{:op :carried :categories #{:modified} :path "edit.txt"}]
         (#'git/parse-porcelain-z (str " M edit.txt" NUL)))))

(deftest parse-porcelain-z-staged
  (is (= [{:op :carried :categories #{:staged} :path "staged.txt"}]
         (#'git/parse-porcelain-z (str "M  staged.txt" NUL)))))

(deftest parse-porcelain-z-staged-and-modified
  (is (= [{:op :carried :categories #{:staged :modified} :path "both.txt"}]
         (#'git/parse-porcelain-z (str "MM both.txt" NUL)))))

(deftest parse-porcelain-z-deleted-worktree
  (is (= [{:op :removed :categories #{:modified} :path "gone.txt"}]
         (#'git/parse-porcelain-z (str " D gone.txt" NUL)))))

(deftest parse-porcelain-z-deleted-staged
  (is (= [{:op :removed :categories #{:staged} :path "staged-del.txt"}]
         (#'git/parse-porcelain-z (str "D  staged-del.txt" NUL)))))

(deftest parse-porcelain-z-rename
  (is (= [{:op :removed :categories #{:staged} :path "old"}
          {:op :carried :categories #{:staged} :path "new"}]
         (#'git/parse-porcelain-z (str "R  new" NUL "old" NUL)))))

(deftest parse-porcelain-z-rename-with-worktree-modify
  ;; "RM new\0old\0" — staged rename + worktree modification on the new path
  (is (= [{:op :removed :categories #{:staged}                :path "old"}
          {:op :carried :categories #{:staged :modified}      :path "new"}]
         (#'git/parse-porcelain-z (str "RM new" NUL "old" NUL)))))

(deftest parse-porcelain-z-copy
  (is (= [{:op :carried :categories #{:staged} :path "new"}]
         (#'git/parse-porcelain-z (str "C  new" NUL "orig" NUL)))))

(deftest parse-porcelain-z-unmerged-skipped
  (doseq [code ["UU" "AA" "DD" "AU" "UA" "DU" "UD"]]
    (is (= [] (#'git/parse-porcelain-z (str code " conflict.txt" NUL)))
        (str "expected " code " to be skipped"))))

(deftest parse-porcelain-z-multiple-records
  (is (= [{:op :carried :categories #{:untracked} :path "a"}
          {:op :carried :categories #{:modified}  :path "b"}
          {:op :removed :categories #{:modified}  :path "c"}]
         (#'git/parse-porcelain-z
          (str "?? a" NUL " M b" NUL " D c" NUL)))))

(deftest parse-porcelain-z-handles-paths-with-newlines
  (is (= [{:op :carried :categories #{:untracked} :path "weird\nname"}]
         (#'git/parse-porcelain-z (str "?? weird\nname" NUL)))))
