(ns envrc.runner
  (:require [envrc.run :as run]))

(defn run-task
  ([task-name task cfg] (run-task task-name task cfg {}))
  ([task-name task cfg ctx]
   (run/execute cfg task-name task ctx)))

(defn- task-fn [task-name spec cfg event]
  (with-meta
    (fn [payload]
      (let [{:keys [exit]} (run-task task-name spec cfg {:event event :payload payload})]
        (when (and (some? exit) (not (zero? exit)))
          (throw (ex-info (str "task " (name task-name) " failed with exit " exit)
                          {:task task-name :exit exit})))))
    {:task task-name :tolerant (boolean (:tolerant spec))}))

(defn event-subscribers
  "Returns {event [fn ...]} built from each task's :on field. Each subscriber fn
   has metadata {:task task-name :tolerant bool}."
  [cfg]
  (reduce
    (fn [acc [task-name spec]]
      (let [on (:on spec)
            events (cond
                     (keyword? on) [on]
                     (vector? on)  on
                     :else         [])]
        (reduce (fn [a ev]
                  (update a ev (fnil conj []) (task-fn task-name spec cfg ev)))
                acc events)))
    {} (:tasks cfg)))
