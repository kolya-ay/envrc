(ns runner
  (:require [clojure.test :as t]))

(defn -main [& _]
  (let [nss '[envrc.core-test
              envrc.templates-test
              envrc.ports-status-test
              envrc.worktree-test
              envrc.files-test
              envrc.schemas-test
              envrc.pane-test
              ;; --- moved: pure-require core tests ---
              envrc-test
              envrc.aliases-test envrc.api-test envrc.capabilities-test
              envrc.cli-test envrc.config-test envrc.config-multiformat-test
              envrc.data-test envrc.dirs-test envrc.events-test
              envrc.find-by-capability-test envrc.git-test envrc.migrate-test
              envrc.notify-test envrc.plugin-test
              envrc.ports-test envrc.proc-test envrc.run-test
              envrc.runner-test envrc.services-test envrc.status-test
              envrc.validate-test
              ;; --- moved: plugin-loader core tests ---
              envrc.flake-test envrc.gen-test envrc.local-test
              envrc.project-test envrc.ref-test envrc.skills-test
              envrc.plugin.ports-test envrc.plugin.process-compose-test
              envrc.plugin.project-test envrc.plugin.worktree-test]]
    (doseq [n nss] (require n))
    (let [{:keys [fail error]} (apply t/run-tests nss)]
      (System/exit (if (zero? (+ fail error)) 0 1)))))
