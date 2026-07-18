(ns envrc.notify
  "Desktop-notification shell-out shared by notifier plugins.
   Tolerates absent notify-send binary and a dead notification daemon."
  (:require [babashka.process :as p]))

(defn send!
  "Shell out to notify-send. `opts`:
   - :summary  required string
   - :body     optional string (omitted from argv when nil)
   - :urgency  \"low\" | \"normal\" | \"critical\" (default \"normal\")
   - :icon     icon name or path (default \"dialog-information\")
   - :hint     dedup key for synchronous hint; omitted when nil.
   Returns nil on success or on tolerated failure."
  [{:keys [summary body urgency icon hint]
    :or   {urgency "normal" icon "dialog-information"}}]
  (try
    (let [argv (cond-> ["notify-send" "--app-name=envrc"
                        (str "--urgency=" urgency)
                        (str "--icon=" icon)]
                 hint        (conj (str "--hint=string:x-canonical-private-synchronous:" hint))
                 true        (conj summary)
                 (some? body) (conj body))]
      (apply p/shell {:continue true :out :string :err :string} argv)
      nil)
    (catch java.io.IOException _ nil)))
