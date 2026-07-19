(ns envrc.files-test
  (:require [clojure.test :refer [deftest is testing]]
            [envrc.files :as files]
            [babashka.fs :as fs]
            [clojure.string :as str]))


(deftest resolve-flat-paths-pass-through
  (is (= ["a" "b"] (files/resolve ["a" "b"] {}))))

(deftest resolve-walks-label-refs
  (let [dict {:envrc  ["envrc.edn" "envrc.json"]
              :agents ["AGENTS.md" "CLAUDE.md"]}]
    (is (= ["envrc.edn" "envrc.json" "AGENTS.md" "CLAUDE.md"]
           (files/resolve [:envrc :agents] dict)))))

(deftest resolve-recursive-with-cycles
  (let [dict {:a [:b "p1"]
              :b [:c "p2"]
              :c [:a "p3"]}]
    ;; depth-first: :a -> (:b "p1") -> (:c "p2") -> (:a "p3") [cycle] -> "p3" -> "p2" -> "p1"
    (is (= ["p3" "p2" "p1"]
           (files/resolve [:a] dict)))))

(deftest resolve-mixed-strings-and-keywords
  (let [dict {:standard [:envrc :agents]
              :envrc    ["envrc.edn"]
              :agents   ["AGENTS.md"]}]
    (is (= ["envrc.edn" "AGENTS.md" ".envrc"]
           (files/resolve [:standard ".envrc"] dict)))))

(deftest resolve-missing-label-empty
  (is (= [] (files/resolve [:nonexistent] {}))))

(deftest resolve-distinct-preserves-first-seen
  (let [dict {:a ["x" "y"]
              :b ["y" "z"]}]
    (is (= ["x" "y" "z"]
           (files/resolve [:a :b] dict)))))

(deftest normalize-path-strips-leading-slash
  (is (= "flake.nix"  (files/normalize-path "/flake.nix")))
  (is (= "specs/foo"  (files/normalize-path "specs/foo")))
  (is (= "specs/"     (files/normalize-path "/specs/"))))

(deftest append-git-exclude-creates-file
  (let [root (str (fs/create-temp-dir))]
    (fs/create-dirs (str root "/.git/info"))
    (files/append-git-exclude root [".direnv" "*.swp"])
    (let [content (slurp (str root "/.git/info/exclude"))]
      (is (re-find #"\.direnv" content))
      (is (re-find #"\*\.swp"  content)))))

(deftest append-git-exclude-dedupes
  (let [root (str (fs/create-temp-dir))]
    (fs/create-dirs (str root "/.git/info"))
    (spit (str root "/.git/info/exclude") ".direnv\n")
    (files/append-git-exclude root [".direnv" "*.swp"])
    (let [lines (str/split-lines (slurp (str root "/.git/info/exclude")))]
      (is (= 1 (count (filter #(= ".direnv" %) lines))))
      (is (some #(= "*.swp" %) lines)))))

(deftest read-git-exclude-lines-returns-vec
  (let [root (str (fs/create-temp-dir))]
    (fs/create-dirs (str root "/.git/info"))
    (spit (str root "/.git/info/exclude") "a\nb\nc\n")
    (is (= ["a" "b" "c"] (files/read-git-exclude-lines root)))))

(deftest read-git-exclude-missing-empty
  (let [root (str (fs/create-temp-dir))]
    (is (= [] (files/read-git-exclude-lines root)))))

(deftest link-into-creates-symlink-stripping-slash
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))]
    (spit (str src "/flake.nix") "{}")
    (files/link-into src dst "/flake.nix")
    (is (fs/sym-link? (str dst "/flake.nix")))))

(deftest link-into-relinks-stale
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))
        other (str (fs/create-temp-dir))]
    (spit (str src "/AGENTS.md") "real")
    (spit (str other "/AGENTS.md") "stale")
    (fs/create-sym-link (str dst "/AGENTS.md") (str other "/AGENTS.md"))
    (files/link-into src dst "AGENTS.md")
    (is (= "real" (slurp (str dst "/AGENTS.md"))))))

(deftest copy-into-skips-existing
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))]
    (spit (str src "/.env.example") "X=src")
    (spit (str dst "/.env.example") "X=dst")
    (files/copy-into src dst ".env.example")
    (is (= "X=dst" (slurp (str dst "/.env.example"))))))

(deftest copy-into-copies-when-missing
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))]
    (spit (str src "/.env.example") "X=src")
    (files/copy-into src dst ".env.example")
    (is (= "X=src" (slurp (str dst "/.env.example"))))))

(deftest mirror-into-overwrites-flat
  (let [sub (str (fs/create-temp-dir) "/envrc-sub")
        root (str (fs/create-temp-dir))]
    (fs/create-dirs (str root "/specs"))
    (spit (str root "/specs/note.md") "v1")
    (files/mirror-into sub root "specs/")
    (is (fs/exists? (str sub "/specs")))
    (is (= "v1" (slurp (str sub "/specs/note.md"))))))

(deftest diff-paths-link-state
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))]
    (spit (str src "/A.md") "1")
    (spit (str src "/B.md") "2")
    (fs/create-sym-link (str dst "/A.md") (str src "/A.md"))
    (let [report (files/diff-paths :link src dst ["A.md" "B.md" "C.md"])]
      (is (= :ok       (->> report (filter #(= "A.md" (:path %))) first :state)))
      (is (= :missing  (->> report (filter #(= "B.md" (:path %))) first :state)))
      (is (= :missing  (->> report (filter #(= "C.md" (:path %))) first :state))))))

(deftest link-into-handles-broken-symlink
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))]
    (spit (str src "/A.md") "content")
    (fs/create-sym-link (str dst "/A.md") (str dst "/missing-target"))
    (files/link-into src dst "A.md")
    (is (fs/sym-link? (str dst "/A.md")))
    (is (= (str src "/A.md") (str (fs/read-link (str dst "/A.md")))))))

(deftest copy-into-skips-broken-symlink
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))]
    (spit (str src "/B.md") "src")
    (fs/create-sym-link (str dst "/B.md") (str dst "/missing"))
    (files/copy-into src dst "B.md")
    ;; Skipped because broken symlink occupies the slot
    (is (fs/sym-link? (str dst "/B.md")))))

(deftest mirror-into-clears-broken-symlink
  (let [sub  (str (fs/create-temp-dir) "/sub")
        root (str (fs/create-temp-dir))]
    (spit (str root "/C.md") "v1")
    (fs/create-dirs sub)
    (fs/create-sym-link (str sub "/C.md") (str sub "/missing"))
    (files/mirror-into sub root "C.md")
    (is (= "v1" (slurp (str sub "/C.md"))))))

(deftest diff-paths-broken-symlink-is-diverged
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))]
    (spit (str src "/D.md") "1")
    (fs/create-sym-link (str dst "/D.md") (str dst "/missing"))
    (let [report (files/diff-paths :link src dst ["D.md"])]
      (is (= :diverged (:state (first report)))))))

(deftest link-into-returns-linked-on-create
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))]
    (spit (str src "/a") "x")
    (is (= :linked (files/link-into src dst "a")))))

(deftest link-into-returns-ok-when-already-correct
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))]
    (spit (str src "/a") "x")
    (files/link-into src dst "a")
    (is (= :ok (files/link-into src dst "a")))))

(deftest link-into-returns-linked-on-replace-stale
  (let [src   (str (fs/create-temp-dir))
        dst   (str (fs/create-temp-dir))
        other (str (fs/create-temp-dir))]
    (spit (str src "/a") "real")
    (spit (str other "/a") "stale")
    (fs/create-sym-link (str dst "/a") (str other "/a"))
    (is (= :linked (files/link-into src dst "a")))))

(deftest link-into-returns-nil-when-src-missing
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))]
    (is (nil? (files/link-into src dst "a")))))

(deftest copy-into-returns-copied-on-create
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))]
    (spit (str src "/a") "x")
    (is (= :copied (files/copy-into src dst "a")))))

(deftest copy-into-returns-nil-when-dst-present
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))]
    (spit (str src "/a") "x")
    (spit (str dst "/a") "y")
    (is (nil? (files/copy-into src dst "a")))))

(deftest copy-into-returns-nil-when-src-missing
  (let [src (str (fs/create-temp-dir))
        dst (str (fs/create-temp-dir))]
    (is (nil? (files/copy-into src dst "a")))))

(deftest mirror-into-returns-mirrored
  (let [sub  (str (fs/create-temp-dir) "/sub")
        root (str (fs/create-temp-dir))]
    (spit (str root "/A.md") "v1")
    (is (= :mirrored (files/mirror-into sub root "A.md")))))

(deftest mirror-into-returns-nil-when-src-missing
  (let [sub  (str (fs/create-temp-dir) "/sub")
        root (str (fs/create-temp-dir))]
    (is (nil? (files/mirror-into sub root "missing")))))

(deftest append-git-exclude-returns-new-lines
  (let [root (str (fs/create-temp-dir))]
    (fs/create-dirs (str root "/.git/info"))
    (spit (str root "/.git/info/exclude") ".direnv\n")
    (is (= ["*.swp"] (files/append-git-exclude root [".direnv" "*.swp"])))))

(deftest append-git-exclude-returns-empty-when-no-new
  (let [root (str (fs/create-temp-dir))]
    (fs/create-dirs (str root "/.git/info"))
    (spit (str root "/.git/info/exclude") ".direnv\n")
    (is (= [] (files/append-git-exclude root [".direnv"])))))

(deftest print-action-renders-verb-and-path
  (let [out (with-out-str (files/print-action :linked ".envrc.local"))]
    (is (= "  link    .envrc.local\n" out))))

(deftest print-action-renders-ref-arrow-form
  (let [out (with-out-str (files/print-action :ref "→ /state/ref/ego/p"))]
    (is (= "  ref     → /state/ref/ego/p\n" out))))

(deftest print-action-skips-non-actions
  (is (= "" (with-out-str (files/print-action nil  ".envrc.local"))))
  (is (= "" (with-out-str (files/print-action :ok  ".envrc.local")))))

(deftest link-into-skips-real-directory-instead-of-crashing
  (let [root (fs/create-temp-dir {:prefix "envrc-link-"})
        src  (str root "/src")
        dst  (str root "/dst")]
    (fs/create-dirs (str src "/envrc"))
    (spit (str src "/envrc/a") "1")
    (fs/create-dirs (str dst "/envrc"))       ; pre-existing real, non-empty dir
    (spit (str dst "/envrc/keep") "2")
    (is (= :skipped (files/link-into src dst "envrc")) "returns :skipped, no throw")
    (is (fs/directory? (str dst "/envrc")) "dst dir left intact")
    (is (not (fs/sym-link? (str dst "/envrc"))))
    (is (fs/exists? (str dst "/envrc/keep")) "existing content preserved")))
