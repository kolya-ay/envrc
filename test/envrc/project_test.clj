(ns envrc.project-test
  (:require [clojure.test :refer [deftest is testing]]
            [envrc.project :as project]
            [babashka.fs :as fs]
            [cheshire.core :as json]))

(deftest slugify-normalizes
  (is (= "my-cool-app" (project/slugify "My Cool App")))
  (is (= "my-cool-app" (project/slugify "my_cool.app")))
  (is (= "foo-bar"     (project/slugify "  Foo   Bar!! ")))
  (is (= "already-slug" (project/slugify "already-slug")))
  (is (= ""            (project/slugify "***"))))

(deftest humanize-title-cases
  (is (= "My Cool App" (project/humanize "my-cool-app")))
  (is (= "Foo"         (project/humanize "foo"))))

(deftest registry-roundtrip
  (let [path (str (fs/create-temp-dir) "/projects.json")]
    (testing "missing file reads as empty"
      (is (= {} (project/read-registry path))))
    (testing "write then read preserves string-keyed shape"
      (let [reg {"app" {"title" "App" "paths" ["/a"] "scope" "ego"}}]
        (project/write-registry! path reg)
        (is (= reg (project/read-registry path)))
        (is (fs/exists? path) "file exists after write")))))

(deftest registry-json-is-sorted-pretty
  (let [reg {"b" {"title" "B" "paths" ["/b"] "scope" "ego"}
             "a" {"title" "A" "paths" ["/a"] "scope" "ego"}}
        out (project/registry-json reg)]
    (is (< (.indexOf out "\"a\"") (.indexOf out "\"b\"")) "keys sorted")
    (is (re-find #"\n" out) "pretty-printed")))

(deftest infer-title-prefers-cfg-then-files
  (let [dir (str (fs/create-temp-dir))]
    (spit (str dir "/flake.nix") "{ description = \"From Flake\"; }")
    (is (= "Cfg Title" (project/infer-title dir "Cfg Title")) "cfg :title wins")
    (is (= "From Flake" (project/infer-title dir nil)) "flake description next"))
  (testing "package.json then pyproject"
    (let [dir (str (fs/create-temp-dir))]
      (spit (str dir "/package.json") (json/generate-string {"name" "pkg-name"}))
      (is (= "pkg-name" (project/infer-title dir nil))))
    (let [dir (str (fs/create-temp-dir))]
      (spit (str dir "/pyproject.toml") "[project]\nname = \"py-name\"\n")
      (is (= "py-name" (project/infer-title dir nil))))
    (let [dir (str (fs/create-temp-dir))]
      (is (nil? (project/infer-title dir nil)) "nothing → nil"))))

(deftest infer-slug-chain
  (let [dir (str (fs/path (fs/create-temp-dir) "Some Project"))]
    (fs/create-dirs dir)
    (is (= "pinned" (project/infer-slug dir "Title Here" "pinned")) "arg wins")
    (is (= "title-here" (project/infer-slug dir "Title Here" nil)) "slugify title")
    (is (= "some-project" (project/infer-slug dir nil nil)) "basename fallback")))

(deftest derive-scope-longest-prefix
  (let [scopes {:default "ego"
                :entries {:ego  {:dir "/home/ay/Code"}
                          :work {:dir "/home/ay/Work"}}}]
    (is (= "ego"  (project/derive-scope "/home/ay/Code/foo" scopes)))
    (is (= "work" (project/derive-scope "/home/ay/Work/bar/baz" scopes)))
    (is (= "ego"  (project/derive-scope "/tmp/elsewhere" scopes)) "fallback to default")))

(def scopes-fix {:default "ego" :entries {:ego {:dir "/home/ay/Code"}}})

(deftest resolve-entry-fills-title-fallback
  (let [dir (str (fs/path (fs/create-temp-dir) "bare-thing"))]
    (fs/create-dirs dir)
    (let [e (project/resolve-entry dir {:cfg-title nil :slug-arg nil :scopes scopes-fix})]
      (is (= "bare-thing" (:slug e)))
      (is (= "Bare Thing" (:title e)) "humanized slug when no title source")
      (is (= dir (:path e)))
      (is (= "ego" (:scope e))))))

(deftest merge-entry-appends-and-warns
  (testing "fresh slug, no warnings"
    (let [[reg w] (project/merge-entry {} {:slug "app" :title "App" :scope "ego" :path "/a"})]
      (is (= {"app" {"title" "App" "paths" ["/a"] "scope" "ego"}} reg))
      (is (empty? w))))
  (testing "same slug + new path + different title → collision warning"
    (let [reg0 {"app" {"title" "App" "paths" ["/a"] "scope" "ego"}}
          [reg w] (project/merge-entry reg0 {:slug "app" :title "Other" :scope "ego" :path "/b"})]
      (is (= ["/a" "/b"] (get-in reg ["app" "paths"])))
      (is (some #(re-find #"may be a different project" %) w))))
  (testing "same path + changed title → drift warning, title updated"
    (let [reg0 {"app" {"title" "Old" "paths" ["/a"] "scope" "ego"}}
          [reg w] (project/merge-entry reg0 {:slug "app" :title "New" :scope "ego" :path "/a"})]
      (is (= "New" (get-in reg ["app" "title"])))
      (is (= ["/a"] (get-in reg ["app" "paths"])) "no dup path")
      (is (some #(re-find #"changed" %) w)))))

(deftest entry-status-classifies
  (let [reg0 {"app" {"title" "App" "paths" ["/a"] "scope" "ego"}}]
    (testing "new slug → :created"
      (is (= :created (project/entry-status reg0 {:slug "new" :title "New" :path "/n"}))))
    (testing "known path + same title → :unchanged"
      (is (= :unchanged (project/entry-status reg0 {:slug "app" :title "App" :path "/a"}))))
    (testing "known path + changed title → :updated"
      (is (= :updated (project/entry-status reg0 {:slug "app" :title "Renamed" :path "/a"}))))
    (testing "new path + same title → :updated"
      (is (= :updated (project/entry-status reg0 {:slug "app" :title "App" :path "/b"}))))
    (testing "new path + different title → :conflict"
      (is (= :conflict (project/entry-status reg0 {:slug "app" :title "Other" :path "/b"}))))))

(deftest notification-for-maps-status
  (let [entry {:slug "app" :title "App" :scope "ego" :path "/a"}]
    (testing ":created → normal-urgency 'registered'"
      (let [n (project/notification-for :created entry)]
        (is (= "Project registered" (:summary n)))
        (is (= "normal" (:urgency n)))
        (is (re-find #"App" (:body n)))))
    (testing ":updated → low-urgency 'updated'"
      (let [n (project/notification-for :updated entry)]
        (is (= "Project updated" (:summary n)))
        (is (= "low" (:urgency n)))))
    (testing ":conflict → critical-urgency"
      (let [n (project/notification-for :conflict entry)]
        (is (= "Project slug conflict" (:summary n)))
        (is (= "critical" (:urgency n)))))
    (testing ":unchanged → nil (silent)"
      (is (nil? (project/notification-for :unchanged entry))))))

(deftest check-and-prune-stale-paths
  (let [reg {"app" {"title" "App" "paths" ["/gone" "/here"] "scope" "ego"}
             "dead" {"title" "Dead" "paths" ["/gone"] "scope" "ego"}}
        exists? #{"/here"}]
    (is (= 2 (count (project/check-registry reg exists?))) "two missing paths")
    (let [[reg' removed] (project/prune-registry reg exists?)]
      (is (= ["/here"] (get-in reg' ["app" "paths"])))
      (is (nil? (get reg' "dead")) "slug with no surviving paths dropped")
      (is (= #{"/gone"} (set removed))))))

(deftest list-lines-tab-delimited
  (let [reg {"app" {"title" "My App" "paths" ["/code/app" "/alt/app"] "scope" "ego"}}
        lines (project/list-lines reg)]
    (is (= 2 (count lines)) "one line per path")
    (is (= "My App\tapp\tego\t/alt/app" (first lines)))
    (is (= "My App\tapp\tego\t/code/app" (second lines)) "sorted by path")))

(deftest apply-register-integrates
  (let [dir (str (fs/path (fs/create-temp-dir) "widget"))]
    (fs/create-dirs dir)
    (let [[reg entry warnings]
          (project/apply-register {} dir {:cfg-title nil :slug-arg nil :scopes scopes-fix})]
      (is (= "widget" (:slug entry)))
      (is (empty? warnings))
      (is (= {"widget" {"title" "Widget" "paths" [dir] "scope" "ego"}} reg)))))

(deftest plugin-manifest-shape
  (do
    (load-file "plugins/default/project.clj")
    (let [m @(resolve 'envrc.plugin.project/plugin)]
      (is (= "project" (:id m)))
      (is (nil? (:capability m)))
      (is (= #{:register :list :check :prune :worktrees} (set (keys (:cli m)))))
      (is (= #{:project-new} (:events m))))))

(deftest read-registry-coerces-non-map
  (let [path (str (fs/create-temp-dir) "/projects.json")]
    (testing "top-level array → empty registry"
      (spit path "[]")
      (is (= {} (project/read-registry path))))
    (testing "top-level scalar → empty registry"
      (spit path "42")
      (is (= {} (project/read-registry path))))))

(def porcelain-sample
  (str "worktree /home/ay/Code/app\nHEAD abc\nbranch refs/heads/main\n\n"
       "worktree /home/ay/.local/state/worktrees/app/feat\nHEAD def\nbranch refs/heads/feat\n\n"))

(deftest parse-worktree-porcelain-extracts-path-and-branch
  (is (= [{:path "/home/ay/Code/app" :branch "main"}
          {:path "/home/ay/.local/state/worktrees/app/feat" :branch "feat"}]
         (project/parse-worktree-porcelain porcelain-sample))))

(deftest parse-worktree-handles-detached
  (is (= [{:path "/x" :branch "detached"}]
         (project/parse-worktree-porcelain "worktree /x\nHEAD abc\ndetached\n\n"))))

(deftest worktree-lines-attaches-project-context
  (let [registry {"app" {"title" "My App" "paths" ["/home/ay/Code/app"] "scope" "ego"}}
        git-fn (fn [p] (when (= p "/home/ay/Code/app")
                         [{:path "/home/ay/Code/app" :branch "main"}
                          {:path "/home/ay/.local/state/worktrees/app/feat" :branch "feat"}]))]
    (is (= ["My App\tmain\tego\t/home/ay/Code/app"
            "My App\tfeat\tego\t/home/ay/.local/state/worktrees/app/feat"]
           (project/worktree-lines registry git-fn)))))

(deftest worktree-lines-dedupes-by-path
  (let [registry {"a" {"title" "A" "paths" ["/p1" "/p2"] "scope" "ego"}}
        git-fn (fn [_] [{:path "/wt" :branch "b"}])]
    (is (= 1 (count (project/worktree-lines registry git-fn))))))

(deftest resolve-entry-sanitizes-title-whitespace
  (let [dir (str (fs/create-temp-dir))]
    (spit (str dir "/flake.nix") "{ description = \"Tabbed\tName\"; }")
    (is (= "Tabbed Name"
           (:title (project/resolve-entry dir {:cfg-title nil :slug-arg "x" :scopes scopes-fix}))))))

(deftest registry-path-uses-envrc-subdir
  (let [expected (str (or (System/getenv "XDG_STATE_HOME")
                          (str (System/getenv "HOME") "/.local/state"))
                      "/envrc/projects.json")]
    (is (= expected (project/registry-path)))))
