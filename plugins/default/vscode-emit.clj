(ns envrc.plugin.vscode-emit
  "Emit .vscode/tasks.json from envrc aliases."
  (:require [envrc.run :as run]))

(defn- task->vscode-entry [cfg alias task-name task]
  (run/editor-entry cfg alias task-name task {:root (System/getProperty "user.dir")} :vscode))

(def plugin
  {:id "vscode-emit"
   :description "Emit .vscode/tasks.json"
   :extends {:emitters {:vscode {:path ".vscode/tasks.json"
                                  :input :aliases
                                  :encode :json
                                  :transform task->vscode-entry}}}})
