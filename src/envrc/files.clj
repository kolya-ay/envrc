(ns envrc.files
  "File helpers: path normalization, git-exclude management, and
   link/copy/mirror/diff primitives used by envrc plugins."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]))

(defn resolve
  "Recursively expand a vector of refs (strings = paths, keywords = labels)
   against the :files dict. Cycle-safe; preserves first-seen order; dedups."
  [refs files]
  (let [seen (volatile! #{})]
    (letfn [(walk [rs]
              (mapcat
                (fn [r]
                  (cond
                    (string? r) [r]
                    (@seen r)   []
                    :else       (do (vswap! seen conj r)
                                    (walk (get files r [])))))
                rs))]
      (vec (distinct (walk refs))))))

(defn normalize-path
  "Strip a single leading `/` (workspace-root anchor) before joining.
   Preserves trailing `/` so callers can detect directory intent."
  [p]
  (if (str/starts-with? p "/") (subs p 1) p))

(defn read-git-exclude-lines [root]
  (let [exclude (str root "/.git/info/exclude")]
    (if (fs/exists? exclude)
      (vec (str/split-lines (slurp exclude)))
      [])))

(defn append-git-exclude
  "Append missing entries to .git/info/exclude. Returns the vector of lines that
   were newly appended (empty if nothing added). No-op when .git/ is absent
   (returns nil)."
  [root paths]
  (let [exclude (str root "/.git/info/exclude")]
    (when (fs/directory? (str root "/.git"))
      (fs/create-dirs (fs/parent exclude))
      (let [existing  (read-git-exclude-lines root)
            existing? (set existing)
            new-lines (vec (remove existing? paths))]
        (when (seq new-lines)
          (spit exclude (str (str/join "\n" (concat existing new-lines)) "\n")))
        new-lines))))

(defn link-into
  "Symlink <src-root>/<path> at <dst-root>/<path>, stripping leading `/`.
   Returns :linked on create/replace, :ok when already correct,
   :skipped when dst is a pre-existing real directory, nil if src missing."
  [src-root dst-root path]
  (let [p   (normalize-path path)
        src (str src-root "/" p)
        dst (str dst-root "/" p)]
    (when (fs/exists? src)
      (fs/create-dirs (fs/parent dst))
      (cond
        (and (fs/sym-link? dst) (= src (str (fs/read-link dst))))
        :ok

        ;; real (non-symlink) directory already present — likely git-tracked
        ;; content checked out in the worktree; leave it, don't clobber.
        (and (fs/directory? dst) (not (fs/sym-link? dst)))
        :skipped

        (or (fs/exists? dst) (fs/sym-link? dst))
        (do (fs/delete dst) (fs/create-sym-link dst src) :linked)

        :else
        (do (fs/create-sym-link dst src) :linked)))))

(defn copy-into
  "Copy <src-root>/<path> to <dst-root>/<path>, stripping leading `/`.
   Returns :copied on create, nil if dst already present or src missing."
  [src-root dst-root path]
  (let [p   (normalize-path path)
        src (str src-root "/" p)
        dst (str dst-root "/" p)]
    (when (and (fs/exists? src)
               (not (or (fs/exists? dst) (fs/sym-link? dst))))
      (fs/create-dirs (fs/parent dst))
      (if (fs/directory? src) (fs/copy-tree src dst) (fs/copy src dst))
      :copied)))

(defn mirror-into
  "Copy <src-root>/<path> into <envrc-sub>/<basename(path)>, always overwriting.
   Returns :mirrored when src present, nil when src missing."
  [envrc-sub src-root path]
  (let [p   (normalize-path path)
        src (str src-root "/" p)
        dst (str envrc-sub "/" (fs/file-name (str/replace p #"/$" "")))]
    (when (fs/exists? src)
      (fs/create-dirs envrc-sub)
      (when (or (fs/exists? dst) (fs/sym-link? dst))
        (if (fs/directory? dst) (fs/delete-tree dst) (fs/delete dst)))
      (if (fs/directory? src) (fs/copy-tree src dst) (fs/copy src dst))
      :mirrored)))

(defn diff-paths
  "Return [{:path :state}] reports for `op` ∈ {:link :copy} semantics.
   States:
     :ok       — dst matches expected (symlink to src for :link; exists for :copy)
     :missing  — no filesystem entry of any kind at dst (or src absent)
     :diverged — dst entry exists but differs; broken symlink counts as diverged."
  [op src-root dst-root paths]
  (vec
    (for [path paths
          :let [p            (normalize-path path)
                src          (str src-root "/" p)
                dst          (str dst-root "/" p)
                dst-present? (or (fs/exists? dst) (fs/sym-link? dst))]]
      {:path  path
       :state (cond
                (not (fs/exists? src))  :missing
                (not dst-present?)      :missing
                (= :link op)            (if (and (fs/sym-link? dst)
                                                 (= src (str (fs/read-link dst))))
                                          :ok
                                          :diverged)
                (= :copy op)            (if (fs/directory? src)
                                          :ok
                                          (if (= (slurp src) (slurp dst))
                                            :ok
                                            :diverged))
                :else                   :ok)})))

(defn print-action
  "One-line action report. status is a keyword (:linked :copied :mirrored
   :ref :local) or nil/:ok (skip).
   Output: `  <verb-padded-to-7>  <subject>`."
  [status subject]
  (when-let [verb (case status
                    :linked   "link"
                    :copied   "copy"
                    :mirrored "mirror"
                    :ref      "ref"
                    :local    "local"
                    :skipped  "skip"
                    nil)]
    (println (format "  %-7s %s" verb subject))))
