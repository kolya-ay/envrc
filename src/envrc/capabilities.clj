(ns envrc.capabilities
  "Capability → verb-set contract. See `contract` map below; load-time
   check throws on plugins missing required verbs."
  (:require [clojure.set :as set]))

(def contract
  {:pane      {:required #{:spawn :list :focus :kill :send}
               :optional #{:signal :logs :resize :title}}
   :workspace {:required #{:new :rm :go}
               :optional #{:branch :apply :status :list}}
   :service   {:required #{:up :down :status :logs :attach}
               :optional #{:list :restart}}})

(defn check-contract!
  "Validates a plugin manifest's :cli against the verb set required for its
   :capability. Throws on missing required verbs. No-op for plugins without
   :capability."
  [plugin]
  (when-let [cap (:capability plugin)]
    (let [implemented (set (keys (:cli plugin {})))
          required (get-in contract [cap :required] #{})
          missing (set/difference required implemented)]
      (when (seq missing)
        (throw (ex-info (str "plugin " (:id plugin) " claims :capability " cap
                             " but missing required verbs: " missing)
                        {:plugin (:id plugin) :capability cap :missing missing}))))))
