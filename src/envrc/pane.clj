(ns envrc.pane
  (:require [clojure.string :as str]
            [konsole.dbus :as dbus]
            [konsole.session :as konsole]
            [konsole.discovery :as disc]
            [konsole.pane :as kpane]))

(def ^:private skip-prefixes
  #{"#" "cd" "export" "set" "unset" "local" "readonly" "PATH="})

(defn- skip-line? [line]
  (let [t (str/trim line)]
    (or (str/blank? t)
        (some #(str/starts-with? t %) skip-prefixes))))

(defn- first-token [script]
  (let [line (->> (str/split-lines script)
                  (drop-while skip-line?)
                  first)]
    (when line
      (first (str/split (str/trim line) #"\s+")))))

(defn- basename [path]
  (when path (last (str/split path #"/"))))

(defn- matches? [token fg-cmd]
  (and (some? fg-cmd)
       (some? token)
       (= (basename fg-cmd) (basename token))))

(defn- decide [fg-cmd token]
  (cond
    (nil? fg-cmd)           :spawn
    (matches? token fg-cmd) :focus
    :else                   :restart))

(defn list-panes [conn]
  (if-let [{:keys [svc]} (konsole/current-session-from-env)]
    (vec (disc/list-panes-in-window conn svc))
    []))

(defn find-pane [pname panes]
  (some #(when (= (:name %) pname) %) panes))

(defn spawn! [conn pname body pane-spec]
  (kpane/spawn conn pname ["bash" "-c" body]
               :vertical (= (:split pane-spec) "vertical")
               :size     (:size pane-spec)))

(defn send! [conn pname body]
  (kpane/send conn pname body))

(defn focus! [conn pname]
  (kpane/focus conn pname))

(defn signal! [conn pname sig]
  (kpane/send-signal conn pname sig))

(defn dispatch
  "pname = pane-name (str), body = expanded bash script, pane-spec = the :panes entry.
   Adaptive resume:
   - pane absent           → spawn + send body
   - pane present, matches → focus
   - pane present, differs → SIGINT, send body, focus"
  [pname body pane-spec]
  (dbus/with-conn [c]
    (let [panes  (list-panes c)
          pane   (find-pane pname panes)
          fg     (:fg-cmd pane)
          action (decide fg (first-token body))]
      (case action
        :spawn   (spawn! c pname body pane-spec)
        :focus   (focus! c pname)
        :restart (do (binding [*out* *err*]
                       (println (str "re-running " pname " in pane :" pname)))
                     (signal! c pname "INT")
                     ;; brief pause so SIGINT lands before the new body is sent
                     (Thread/sleep 200)
                     (send! c pname body)
                     (focus! c pname)))
      nil)))
