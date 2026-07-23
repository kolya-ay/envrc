(ns envrc.run
  (:require [babashka.process :as p]
            [clojure.string :as str]
            [envrc.env :as envrc.env]))

(defn- argv? [x]
  (and (sequential? x)
       (seq x)
       (every? string? x)))

(defn- invalid-run [task-name msg data]
  (throw (ex-info (str "envrc: task " task-name " " msg)
                  (merge {:task task-name} data))))

(defn- validate-result [task-name value]
  (cond
    (nil? value) nil
    (string? value) value
    (argv? value) (vec value)
    :else (invalid-run task-name "returned invalid :run value" {:value value})))

(defn task-env-resolution
  [cfg task]
  (let [{:keys [set unset]} (envrc.env/task-env cfg task)]
    {:set (into (sorted-map)
                (map (fn [[k v]] [(name k) (str v)]))
                set)
     :unset (->> unset (map name) sort vec)}))

(defn- context [cfg task-name task {:keys [root event payload] :as _ctx}]
  (let [{:keys [set]} (task-env-resolution cfg task)]
    {:cfg cfg
     :task-name task-name
     :task task
     :root root
     :env set
     :event event
     :payload payload}))

(defn evaluate
  [cfg task-name task ctx]
  (let [run (:run task)]
    (cond
      (nil? run)
      (invalid-run task-name "has no :run" {})

      (string? run)
      run

      (argv? run)
      (vec run)

      (fn? run)
      (validate-result task-name (run (context cfg task-name task ctx)))

      :else
      (invalid-run task-name "has invalid :run" {:run run}))))

(defn- shell-quote [s]
  (str "'" (str/replace s "'" "'\\''") "'"))

(defn- wrap-unset-argv
  [argv unset]
  (if (seq unset)
    (into ["env"]
          (concat (mapcat (fn [name] ["-u" name]) unset)
                  ["--"]
                  argv))
    argv))

(defn wrap-unset-shell-command
  [body unset]
  (if (seq unset)
    (str/join " " (wrap-unset-argv ["sh" "-c" (shell-quote body)] unset))
    body))

(defn execute
  [cfg task-name task ctx]
  (let [result (evaluate cfg task-name task ctx)
        {:keys [set unset]} (task-env-resolution cfg task)
        opts (cond-> {:inherit true :continue true}
               (seq set) (assoc :extra-env set))]
    (cond
      (nil? result) {:exit 0}
      (string? result) (if (seq unset)
                         (-> (apply p/process opts (wrap-unset-argv ["sh" "-c" result] unset)) deref)
                         (p/shell opts result))
      :else (apply p/process opts (wrap-unset-argv result unset)))))

(defn shell-body
  ([cfg task-name task ctx]
   (shell-body cfg task-name task ctx {}))
  ([cfg task-name task ctx {:keys [require-command surface]}]
   (let [result (evaluate cfg task-name task ctx)]
     (cond
       (nil? result)
       (if require-command
         (throw (ex-info (str "envrc: " (name (or surface :task))
                              " task " task-name " must produce a command")
                         {:task task-name :surface surface}))
         nil)

       (string? result)
       result

       :else
       (str/join " " (map shell-quote result))))))

(defn editor-entry
  [cfg alias task-name task ctx editor]
  (let [result (evaluate cfg task-name task ctx)
        label (or (:label task) (some-> alias name) (name task-name))
        {:keys [set unset]} (task-env-resolution cfg task)
        env (merge set (zipmap unset (repeat nil)))]
    (when result
      (case editor
        :vscode
        (let [base {:label label :options {:env (merge set (zipmap unset (repeat nil)))}}]
          (if (string? result)
            (assoc base :type "shell" :command (wrap-unset-shell-command result unset))
            (assoc base :type "process"
                        :command (first (wrap-unset-argv result unset))
                        :args (vec (rest (wrap-unset-argv result unset))))))

        :zed
        (let [base {:label label :env (merge set (zipmap unset (repeat nil))) }]
          (if (string? result)
            (assoc base :command (wrap-unset-shell-command result unset) :shell "system")
            (assoc base :command (first result)
                        :args (vec (rest result))))))))
)
