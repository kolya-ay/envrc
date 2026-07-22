(ns envrc.gen-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string]
            [envrc.gen :refer [user-invokable?]]
            [envrc.gen]))

;; Load both emitter plugins (mimicking envrc plugin discovery)
(load-file "plugins/default/zed-emit.clj")
(load-file "plugins/default/vscode-emit.clj")

(deftest zed-transform-string-run
  (let [transform (-> @(resolve 'envrc.plugin.zed-emit/plugin)
                      (get-in [:extends :emitters :zed :transform]))
        out (transform {} :my-task :my-task {:label "Dev" :run "bun dev" :group "general"})]
    (is (= "Dev" (:label out)))
    (is (= "bun dev" (:command out)))
    (is (= "system" (:shell out)))))

(deftest zed-transform-vector-run
  (let [transform (-> @(resolve 'envrc.plugin.zed-emit/plugin)
                      (get-in [:extends :emitters :zed :transform]))
        out (transform {} :my-task :my-task {:label "Dev" :run ["bun" "dev"]})]
    (is (= "bun" (:command out)))
    (is (= ["dev"] (:args out)))))

(deftest vscode-transform-string-run
  (let [transform (-> @(resolve 'envrc.plugin.vscode-emit/plugin)
                      (get-in [:extends :emitters :vscode :transform]))
        out (transform {} :my-task :my-task {:label "Dev" :run "bun dev"})]
    (is (= "Dev" (:label out)))
    (is (= "shell" (:type out)))
    (is (= "bun dev" (:command out)))))

(deftest vscode-transform-vector-run
  (let [transform (-> @(resolve 'envrc.plugin.vscode-emit/plugin)
                      (get-in [:extends :emitters :vscode :transform]))
        out (transform {} :my-task :my-task {:label "Dev" :run ["bun" "dev"]})]
    (is (= "process" (:type out)))
    (is (= "bun" (:command out)))
    (is (= ["dev"] (:args out)))))

(deftest zed-transform-unsets-inherited-env
  (let [transform (-> @(resolve 'envrc.plugin.zed-emit/plugin)
                      (get-in [:extends :emitters :zed :transform]))
        out (transform {:env {:SECRET "secret"}}
                       :my-task :my-task
                       {:label "Dev" :run "echo $SECRET" :env {:SECRET nil}})]
    (is (= {"SECRET" nil} (:env out)))
    (is (= "env -u SECRET -- sh -c 'echo $SECRET'" (:command out)))
    (is (= "system" (:shell out)))))

(deftest vscode-transform-unsets-inherited-env
  (let [transform (-> @(resolve 'envrc.plugin.vscode-emit/plugin)
                      (get-in [:extends :emitters :vscode :transform]))
        out (transform {:env {:SECRET "secret"}}
                       :my-task :my-task
                       {:label "Dev" :run "echo $SECRET" :env {:SECRET nil}})]
    (is (= {"SECRET" nil} (get-in out [:options :env])))
    (is (= "env -u SECRET -- sh -c 'echo $SECRET'" (:command out)))
    (is (= "shell" (:type out)))))

(deftest user-invokable-filter
  (is (false? (user-invokable? {:on :shell})))
  (is (false? (user-invokable? {:on :gen})))
  (is (true?  (user-invokable? {:run "x"})))
  (is (true?  (user-invokable? {:run "x" :show {:pane :a}})))
  (is (false? (user-invokable? {:on [:shell]})))
  (is (true?  (user-invokable? {:on []}))))

(deftest gen-cfg-mode-raw-passes-transform-output-through
  (let [emitter {:input :cfg :encode :raw
                 :transform (fn [cfg] (str "PKGS=" (count (:packages cfg))))}
        cfg {:packages ["a" "b" "c"]
             :plugins {"x" {:id "x" :extends {:emitters {:x emitter}}}}}
        out (with-out-str (envrc.gen/gen! cfg :x {:stdout true}))]
    (is (= "PKGS=3\n" out))))

(deftest gen-tasks-raw-joins-with-blank-line
  (let [emitter {:input :tasks :encode :raw
                 :filter (constantly true)
                 :transform (fn [_cfg _name t] (:run t))}
        cfg {:tasks {:b {:run "echo b"} :a {:run "echo a"}}
             :plugins {"x" {:id "x" :extends {:emitters {:x emitter}}}}}
        out (with-out-str (envrc.gen/gen! cfg :x {:stdout true}))]
    ;; sorted by task name → a before b; :raw joins with a blank line
    (is (= "echo a\n\necho b\n" out))))

(deftest gen-tasks-raw-filter-selects
  (let [emitter {:input :tasks :encode :raw
                 :filter (fn [t] (= :enter (:on t)))
                 :transform (fn [_cfg _n t] (:run t))}
        cfg {:tasks {:a {:run "skip" :on :shell}
                     :b {:run "keep" :on :enter}}
             :plugins {"x" {:id "x" :extends {:emitters {:x emitter}}}}}
        out (with-out-str (envrc.gen/gen! cfg :x {:stdout true}))]
    (is (= "keep\n" out))))

(deftest gen-tasks-json-uses-aliases-for-editor-surface
  (let [cfg {:tasks {:keep {:run "echo keep" :label "Keep"}
                     :skip {:run "echo skip"}}
             :use {:aliases {:k :keep}}
             :plugins {"zed-emit" @(resolve 'envrc.plugin.zed-emit/plugin)}}
        out (with-out-str (envrc.gen/gen! cfg :zed {:stdout true}))]
    (is (clojure.string/includes? out "Keep"))
    (is (not (clojure.string/includes? out "skip")))))

(deftest gen-implicit-event-emitter-walks-on-tasks
  (let [cfg {:tasks {:a {:run "export A=1" :on :enter}
                     :b {:run "export B=2" :on :enter}
                     :c {:run "skip"       :on :shell}}
             :plugins {}}
        out (with-out-str (envrc.gen/gen! cfg :enter {:stdout true}))]
    (is (= "export A=1\n\nexport B=2\n" out))))

(deftest gen-implicit-event-also-matches-vector-on
  (let [cfg {:tasks {:a {:run "x" :on [:enter :shell]}}
             :plugins {}}
        out (with-out-str (envrc.gen/gen! cfg :enter {:stdout true}))]
    (is (= "x\n" out))))

(deftest gen-implicit-event-emitter-evaluates-functions
  (let [cfg {:tasks {:a {:run (fn [_] "export A=1") :on :enter}}
             :plugins {}}
        out (with-out-str (envrc.gen/gen! cfg :enter {:stdout true}))]
    (is (= "export A=1\n" out))))

(deftest gen-unknown-fmt-suggests-event-name
  (let [cfg {:tasks {} :plugins {}}]
    (is (thrown-with-msg? Exception #"did you mean `enter`"
          (envrc.gen/gen! cfg :enterr {:stdout true})))))

(deftest gen-explicit-emitter-shadows-implicit-event
  (let [explicit {:input :cfg :encode :raw
                  :transform (fn [_] "EXPLICIT")}
        cfg {:tasks {:a {:run "should-not-appear" :on :enter}
                     }
             :plugins {"x" {:id "x" :events #{:enter}
                            :extends {:emitters {:enter explicit}}}}}
        out (with-out-str (envrc.gen/gen! cfg :enter {:stdout true}))]
    (is (= "EXPLICIT\n" out))))

(deftest user-invokable-filters-workspace-events
  (is (false? (user-invokable? {:on :workspace-new})))
  (is (false? (user-invokable? {:on :workspace-removed})))
  (is (false? (user-invokable? {:on :enter})))
  (is (true?  (user-invokable? {:on :my-custom-event})))
  (is (true?  (user-invokable? {}))))
