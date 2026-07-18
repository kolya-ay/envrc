(ns envrc.git
  "Shared git shell-out helpers."
  (:require [babashka.fs :as fs]
            [babashka.process :as p]
            [clojure.string :as str]))

(defn branch
  "Returns the current branch name for `dir`, or nil on error / detached HEAD."
  [dir]
  (let [{:keys [exit out]} (p/shell
                             {:out :string :err :string :continue true :dir dir}
                             "git" "rev-parse" "--abbrev-ref" "HEAD")]
    (when (zero? exit) (str/trim out))))

(defn main-branch
  "Returns the default branch name for `dir` via origin/HEAD; falls back to \"main\"."
  [dir]
  (let [{:keys [exit out]} (p/shell
                             {:out :string :err :string :continue true :dir dir}
                             "git" "symbolic-ref" "refs/remotes/origin/HEAD")]
    (or (when (zero? exit)
          (last (str/split (str/trim out) #"/")))
        "main")))

(defn main-worktree-root
  "Return the main checkout's toplevel, even from inside a linked worktree.
   Uses `git rev-parse --git-common-dir`."
  []
  (let [{:keys [exit out]} (p/shell {:out :string :err :string :continue true}
                                    "git" "rev-parse" "--git-common-dir")]
    (when (zero? exit)
      (let [common (str/trim out)
            root   (cond
                     (= common ".git")               "."
                     (str/ends-with? common "/.git") (subs common 0 (- (count common) 5))
                     :else                           common)]
        (str (fs/canonicalize root))))))

(defn main-checkout?
  "True when `root` (after canonicalization) is the main checkout's toplevel."
  [root]
  (= (str (fs/canonicalize root)) (main-worktree-root)))

(def ^:private unmerged-codes
  #{"UU" "AA" "DD" "AU" "UA" "DU" "UD"})

(defn- categorize
  "Given porcelain X Y chars (each \\space or a status letter),
   return the category set for this entry."
  [x y]
  (cond-> #{}
    (#{\M \D} y)          (conj :modified)
    (#{\A \M \D \R \C} x) (conj :staged)))

(defn- parse-record
  "Parse one porcelain record. records is the seq of remaining NUL-split chunks.
   Returns [ops remaining-records], where ops is a vector of {:op :categories :path}."
  [head records]
  (let [code (subs head 0 2)
        path (subs head 3)
        x    (nth head 0)
        y    (nth head 1)]
    (cond
      (= code "??")
      [[{:op :carried :categories #{:untracked} :path path}] records]

      (= code "!!")
      [[] records]

      (contains? unmerged-codes code)
      [[] records]

      (or (= \R x) (= \C x))
      (let [orig (first records)
            rest (next records)
            cats (categorize x y)]
        (if (= \R x)
          [[{:op :removed :categories #{:staged} :path orig}
            {:op :carried :categories cats      :path path}]
           rest]
          [[{:op :carried :categories cats :path path}] rest]))

      (= \D y)
      [[{:op :removed :categories (categorize x y) :path path}] records]

      (and (= \D x) (= \space y))
      [[{:op :removed :categories #{:staged} :path path}] records]

      :else
      (let [cats (categorize x y)]
        (if (seq cats)
          [[{:op :carried :categories cats :path path}] records]
          [[] records])))))

(defn parse-porcelain-z
  "Parse NUL-delimited porcelain v1 output into a flat ops list.
   Each record is \"XY path\"; rename/copy records are followed by the
   original path as a second NUL-delimited chunk."
  [s]
  (let [nul-re (re-pattern (str (char 0)))]
    (loop [records (remove empty? (str/split s nul-re))
           ops     []]
      (if (empty? records)
        ops
        (let [[new-ops remaining] (parse-record (first records) (next records))]
          (recur remaining (into ops new-ops)))))))

(defn dirty-paths
  "Run `git status --porcelain=v1 -z -uall` in `dir`; return parsed ops.
   `-uall` recurses into untracked directories so callers see leaves
   instead of opaque dir entries (matters for carry-into-worktree).
   Returns [] on git failure (matches no-changes semantics); logs stderr
   to *err* on non-zero exit."
  [dir]
  (let [{:keys [exit out err]} (p/shell {:out :string :err :string :continue true :dir dir}
                                        "git" "status" "--porcelain=v1" "-z" "-uall")]
    (if (zero? exit)
      (parse-porcelain-z out)
      (do (binding [*out* *err*]
            (println "envrc warning: git status failed in" dir ":" err))
          []))))
