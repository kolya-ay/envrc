(ns envrc.plugin.zed-emit
  "Emit .zed/tasks.json from envrc aliases."
  (:require [envrc.run :as run]))

(defn- task->zed-entry [cfg alias task-name task]
  (run/editor-entry cfg alias task-name task {:root (System/getProperty "user.dir")} :zed))

(def plugin
  {:id "zed-emit"
   :description "Emit .zed/tasks.json"
   :extends {:emitters {:zed {:path      ".zed/tasks.json"
                              :input     :aliases
                              :encode    :json
                              :transform task->zed-entry}}}})
