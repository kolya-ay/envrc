(ns envrc.plugin.my-plugin)

(defn status [_cfg _ctx]
  (println "my-plugin ok"))

(def plugin
  {:id "my-plugin"
   :description "Example envrc plugin"
   :cli {:status status}})
