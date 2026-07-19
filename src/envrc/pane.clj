(ns envrc.pane
  "Pure adaptive-resume decision logic for the :pane capability.
   Given a pane's foreground command and a task's script, decide whether to
   spawn, focus, or restart. Backend I/O (konsole, tmux, …) lives in a
   machine-side :pane backend."
  (:require [clojure.string :as str]))

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

(defn decide
  "Given the pane's foreground command (nil if pane absent) and the task's
   first token, return :spawn, :focus, or :restart."
  [fg-cmd token]
  (cond
    (nil? fg-cmd)           :spawn
    (matches? token fg-cmd) :focus
    :else                   :restart))
