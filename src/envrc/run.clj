(ns envrc.run
  (:require [babashka.process :as p]
            [clojure.string :as str]))

(defn- env-map [task]
  (into {}
        (map (fn [[k v]] [(name k) v]))
        (:env task)))

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

(defn- context [cfg task-name task {:keys [root event payload] :as _ctx}]
  {:cfg cfg
   :task-name task-name
   :task task
   :root root
   :env (env-map task)
   :event event
   :payload payload})

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

(defn execute
  [cfg task-name task ctx]
  (let [result (evaluate cfg task-name task ctx)
        env (env-map task)
        opts (cond-> {:inherit true :continue true}
               (seq env) (assoc :extra-env env))]
    (cond
      (nil? result) {:exit 0}
      (string? result) (p/shell opts result)
      :else (apply p/process opts result))))

(defn- shell-quote [s]
  (str "'" (str/replace s "'" "'\\''") "'"))

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
        env (or (:env task) {})]
    (when result
      (case editor
        :vscode
        (let [base {:label label :options {:env env}}]
          (if (string? result)
            (assoc base :type "shell" :command result)
            (assoc base :type "process"
                        :command (first result)
                        :args (vec (rest result)))))

        :zed
        (let [base {:label label :env env}]
          (if (string? result)
            (assoc base :command result :shell "system")
            (assoc base :command (first result)
                        :args (vec (rest result)))))))))
