(ns runner
  (:require [clojure.test :as t]))

(defn -main [& _]
  (let [nss '[envrc.core-test
              envrc.templates-test
              envrc.ports-status-test]]
    (doseq [n nss] (require n))
    (let [{:keys [fail error]} (apply t/run-tests nss)]
      (System/exit (if (zero? (+ fail error)) 0 1)))))
