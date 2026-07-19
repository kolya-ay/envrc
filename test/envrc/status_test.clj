(ns envrc.status-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [envrc.status :as status]))

(deftest overview-default
  (let [cfg {:tasks {:t {:run ["x"] :label "the t task"}
                     :hidden {:run ["y"] :label "hidden task"}}
             :use {:aliases {:t :t}}
             :plugins {"konsole" {:id "konsole" :capability :pane}}}
        out (with-out-str (status/overview cfg {}))]
    (is (re-find #"Welcome to" out))
    (is (re-find #"the t task" out))
    (is (not (re-find #"hidden task" out)))))

(deftest overview-verbose-includes-counts
  (let [cfg {:tasks {:t {:run ["x"]}}
             :plugins {"konsole" {:id "konsole" :capability :pane}}}
        out (with-out-str (status/overview cfg {:v true}))]
    (is (re-find #"tasks: 1" out))
    (is (re-find #"plugins: 1" out))))

(deftest overview-json-has-counts
  (let [cfg {:tasks {:t {:run ["x"]}}
             :plugins {"konsole" {:id "konsole" :capability :pane}}}
        out (with-out-str (status/overview cfg {:json true}))]
    (is (re-find #"\"tasks\":1" out))
    (is (re-find #"\"plugins\":1" out))))

(deftest tasks-view
  (let [cfg {:tasks {:t1 {:label "L1" :group "dev" :run ["x"]}
                     :t2 {:run ["y"]}}}]
    (is (re-find #"t1" (with-out-str (status/tasks cfg {}))))
    (is (re-find #"t2" (with-out-str (status/tasks cfg {}))))))

(deftest plugins-view
  (let [cfg {:plugins {"konsole" {:id "konsole" :description "Pane plugin"}}}]
    (is (re-find #"konsole" (with-out-str (status/plugins cfg {}))))))

(deftest dispatch-routes-to-view
  (let [cfg {:tasks {} :plugins {}}]
    (is (re-find #"Welcome to" (with-out-str (status/dispatch cfg nil {}))))
    (is (re-find #"unknown view"
                 (try (with-out-str (status/dispatch cfg "nonexistent" {}))
                      (catch Exception e (.getMessage e)))))))

(deftest plugin-status-sections-included-at-vv
  (let [section-fn (fn [_cfg _ctx] (println "PLUGIN CONTENT"))
        cfg {:plugins {"konsole" {:id "konsole" :description ""
                               :status {:panes {:human section-fn :level :vv}}}}
             :tasks {} :dispatch {}}]
    (is (re-find #"PLUGIN CONTENT"
                 (with-out-str (status/overview cfg {:vv true}))))
    (is (not (re-find #"PLUGIN CONTENT"
                      (with-out-str (status/overview cfg {})))))))

(deftest plugin-status-sections-at-v-level
  (let [section-fn (fn [_cfg _ctx] (println "V-LEVEL CONTENT"))
        cfg {:plugins {"konsole" {:id "konsole" :description ""
                               :status {:s {:human section-fn :level :v}}}}
             :tasks {} :dispatch {}}]
    (is (re-find #"V-LEVEL CONTENT"
                 (with-out-str (status/overview cfg {:v true}))))
    (is (re-find #"V-LEVEL CONTENT"
                 (with-out-str (status/overview cfg {:vv true})))
        "vv subsumes v")
    (is (not (re-find #"V-LEVEL CONTENT"
                      (with-out-str (status/overview cfg {})))))))

(deftest plugin-status-default-level-always-shown
  (let [section-fn (fn [_cfg _ctx] (println "DEFAULT CONTENT"))
        cfg {:plugins {"konsole" {:id "konsole" :description ""
                               :status {:s {:human section-fn :level :default}}}}
             :tasks {} :dispatch {}}]
    (is (re-find #"DEFAULT CONTENT"
                 (with-out-str (status/overview cfg {}))))))

(deftest events-view-groups-tasks-by-on
  (let [cfg {:tasks {:a {:run "x" :on :enter}
                     :b {:run "y" :on :shell}
                     :c {:run "z" :on :enter}}
             :plugins {}}
        out (with-out-str (envrc.status/dispatch cfg "events" {}))]
    (is (re-find #":enter" out))
    (is (re-find #":shell" out))
    (is (re-find #"a, c|c, a" out))))

(deftest events-view-json
  (let [cfg {:tasks {:a {:run "x" :on :enter}}
             :plugins {}}
        out (with-out-str (envrc.status/dispatch cfg "events" {:json true}))]
    (is (re-find #"\"enter\"" out))
    (is (re-find #"\"a\"" out))))

(deftest status-label-dispatches-to-cli-status
  (let [called (atom nil)
        cfg    {:plugins {"r" {:id "r" :handles #{:ref}
                               :cli {:status (fn [_ a] (reset! called a))}}}}]
    (envrc.status/dispatch-label-or-view cfg "ref" {})
    (is (= {:label :ref :brief? false} @called))))

(deftest status-overview-iterates-handles-brief
  (let [seen (atom [])
        cfg  {:plugins {"a" {:id "a" :handles #{:l1 :l2}
                             :cli {:status (fn [_ a] (swap! seen conj a))}}}}]
    (envrc.status/overview cfg {})
    (is (= #{{:label :l1 :brief? true} {:label :l2 :brief? true}}
           (set @seen)))))

(deftest status-built-in-view-takes-precedence
  ;; "tasks" arg routes to existing :tasks view, not a hypothetical :tasks label
  (let [called (atom false)
        cfg    {:plugins {"p" {:handles #{:tasks}
                               :cli {:status (fn [_ _] (reset! called true))}}}
                :tasks {}}]
    (with-out-str (envrc.status/dispatch-label-or-view cfg "tasks" {}))
    (is (false? @called))))

(deftest mask-secret-masks-sensitive-keys
  (is (= "cpk_***" (status/mask-secret :CHUTES_API_KEY "cpk_longvalue" false)))
  (is (= "tok_***" (status/mask-secret :GH_TOKEN "tok_xyz" false)))
  (is (= "2001"    (status/mask-secret :PI_ROUTE_PORT "2001" false))))

(deftest mask-secret-show-secrets-bypasses
  (is (= "cpk_longvalue" (status/mask-secret :CHUTES_API_KEY "cpk_longvalue" true))))

(deftest env-view-prints-top-level-env-masked
  (let [out (with-out-str
              (status/env {:project {:slug "pi-route"}
                           :env {:CHUTES_API_KEY "cpk_secret" :PI_ROUTE_PORT "2001"}}
                          {}))]
    (is (str/includes? out "Env for pi-route"))
    (is (str/includes? out "CHUTES_API_KEY"))
    (is (str/includes? out "cpk_***"))
    (is (not (str/includes? out "cpk_secret")))
    (is (str/includes? out "2001"))))

(deftest env-view-show-secrets
  (let [out (with-out-str
              (status/env {:project {:slug "p"} :env {:API_KEY "abc123"}}
                          {:show-secrets true}))]
    (is (str/includes? out "abc123"))))

(deftest tasks-view-shows-per-task-env-masked
  (let [out (with-out-str
              (status/tasks {:tasks {:db {:label "db" :env {:PG_PASSWORD "hunter2"}}}}
                            {}))]
    (is (str/includes? out "db"))
    (is (str/includes? out "PG_PASSWORD"))
    (is (not (str/includes? out "hunter2")))))
