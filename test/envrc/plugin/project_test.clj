(ns envrc.plugin.project-test
  (:require [clojure.test :refer [deftest is testing]]))

(load-file "plugins/default/project.clj")

(deftest plugin-declares-project-emitter
  (let [plugin @(resolve 'envrc.plugin.project/plugin)
        emitter (get-in plugin [:extends :emitters :project])]
    (is (= :cfg (:input emitter)))
    (is (= :raw (:encode emitter)))
    (is (fn? (:transform emitter)))))

(deftest project-shell-emits-title-and-slug-exports
  (let [transform (-> @(resolve 'envrc.plugin.project/plugin)
                      (get-in [:extends :emitters :project :transform]))
        out (transform {:title "My App"
                        :project {:slug "my-app" :title "My App"}})]
    (is (re-find #"export ENVRC_PROJECT=\"My App\"" out))
    (is (re-find #"export ENVRC_PROJECT_SLUG=my-app" out))))

(deftest project-shell-escapes-quotes-and-backslashes-in-title
  (let [transform (-> @(resolve 'envrc.plugin.project/plugin)
                      (get-in [:extends :emitters :project :transform]))
        out (transform {:project {:slug "x" :title "He said \"hi\" \\o/"}})]
    ;; String-contains avoids regex-escape ambiguity. The expected substring
    ;; here is the literal bash:  export ENVRC_PROJECT="He said \"hi\" \\o/"
    (is (clojure.string/includes?
          out
          "export ENVRC_PROJECT=\"He said \\\"hi\\\" \\\\o/\""))))

(deftest project-shell-falls-back-to-humanized-slug-when-title-blank
  (let [transform (-> @(resolve 'envrc.plugin.project/plugin)
                      (get-in [:extends :emitters :project :transform]))
        out (transform {:project {:slug "my-cool-app" :title ""}})]
    (is (re-find #"export ENVRC_PROJECT=\"My Cool App\"" out))))

(deftest project-shell-exports-all-four
  (let [transform (-> @(resolve 'envrc.plugin.project/plugin)
                      (get-in [:extends :emitters :project :transform]))
        out (transform {:project {:slug "foo" :title "Foo Project"
                                  :scope :ego :workspace "main"}})]
    (is (re-find #"export ENVRC_PROJECT=\"Foo Project\"" out))
    (is (re-find #"export ENVRC_PROJECT_SLUG=foo" out))
    (is (re-find #"export ENVRC_PROJECT_SCOPE=ego" out))
    (is (re-find #"export ENVRC_WORKSPACE=main" out))))
