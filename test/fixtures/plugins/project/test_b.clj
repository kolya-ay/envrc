(ns envrc.plugin.test-b)
(def plugin
  {:id "test_b"
   :description "Test plugin B"
   :cli {:hi (fn [_ _] (println "hi from test_b"))}})
