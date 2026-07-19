(ns envrc.plugin.test-a)
(def plugin
  {:id "test_a"
   :description "Test plugin A"
   :capability :pane
   :cli {:spawn (fn [_ _] (println "spawn from test_a"))
         :list  (fn [_ _] nil)
         :focus (fn [_ _] nil)
         :kill  (fn [_ _] nil)
         :send  (fn [_ _] nil)
         :hi    (fn [_ _] (println "hi from test_a"))}})
