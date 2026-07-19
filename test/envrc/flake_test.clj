(ns envrc.flake-test
  (:require [clojure.test :refer [deftest is testing]]
            [babashka.fs :as fs]))

;; Load the migrated flake plugin
(load-file "plugins/default/flake.clj")

(def ^:private menu-test-template
  "{ shellHooks = [\n# @@shellHooks@@\n# @@menu@@\n]; }")

(deftest render-package-string
  (let [render-package (resolve 'envrc.plugin.flake/render-package)]
    (is (= "\"biome\"" (render-package 'biome)))))

(deftest render-package-vector-uses-source-name-directly
  (let [render-package (resolve 'envrc.plugin.flake/render-package)]
    (is (= "[ \"unstable\" \"firefox\" ]"
           (render-package [:unstable 'firefox])))))

(deftest render-flake-drops-status-hook-when-no-commands
  (let [render (resolve 'envrc.plugin.flake/render)
        out (render {:packages []} menu-test-template)]
    (is (not (re-find #"envrc status" out)))))

(deftest render-flake-emits-status-hook-when-commands-present
  (let [render (resolve 'envrc.plugin.flake/render)
        out (render {:packages []
                     :tasks {:dev {:run ["x"]}}}
                    menu-test-template)]
    (is (re-find #"''\s*\n\s*envrc status\s*\n\s*''" out))))

(deftest url-parse-github-bare
  (let [parse (resolve 'envrc.plugin.flake/url->github-parts)]
    (is (= {:type "github" :owner "numtide" :repo "llm-agents.nix"}
           (parse "github:numtide/llm-agents.nix")))))

(deftest plugin-declares-flake-emitter
  (let [plugin @(resolve 'envrc.plugin.flake/plugin)
        emitter (get-in plugin [:extends :emitters :flake])]
    (is (= :cfg (:input emitter)))
    (is (= :raw (:encode emitter)))
    (is (fn? (:transform emitter)))))
