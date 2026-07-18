(ns core
  (:require [babashka.cli :as cli]
            [babashka.process :as p]
            [clj-commons.ansi :refer [compose]]
            [clojure.string :as str]
            [envrc.config :as econf]))

(defn tv-select [choices]
  (when (seq choices)
    (let [{:keys [out exit]} (p/shell {:in (str/join "\n" choices)
                                       :out :string :continue true} "tv")]
      (when (zero? exit) (not-empty (str/trim out))))))

(def ^:private config-cache (atom nil))

(defn load-config
  "Read ~/.config/envrc.{edn,yaml,yml,json} once, memoized. Returns the merged attrset.
   Callers extract keys with get/get-in."
  []
  (or @config-cache
      (let [dir         (econf/xdg-config-dir)
            [layer _]   (econf/load-layer dir)
            merged-cfg  (econf/collapse-layer layer)]
        (reset! config-cache merged-cfg))))

(defn run [description cli-spec f]
  (let [spec (assoc cli-spec :help {:coerce :boolean :desc "Show this help"})
        opts (cli/parse-opts *command-line-args* {:spec spec})]
    (if (:help opts)
      (do (println description) (println) (println (cli/format-opts {:spec spec})))
      (f opts))))

(defn dispatch-error [{:keys [cause dispatch all-commands wrong-input]}]
  (case cause
    :input-exhausted
    (let [prefix (when (seq dispatch) (str (str/join " " dispatch) " "))]
      (println (str "Usage: " prefix "<command>"))
      (println)
      (doseq [c (sort all-commands)]
        (println (str "  " c)))
      (System/exit 1))
    :no-match
    (do (println (compose [:bold.red "ERROR: "] "Unknown command: " (or wrong-input (str/join " " dispatch))))
        (System/exit 1))
    (do (println (compose [:bold.red "ERROR: "] (str cause)))
        (System/exit 1))))
