(ns envrc.plugin.local
  "Adds :local-labeled paths to .git/info/exclude on shell enter.
   Replaces the V1 `exclude` plugin (which had its own :categories/:registry
   surface). Labels live in top-level :files; this plugin just resolves the
   :local label and appends to git-exclude."
  (:require [envrc.api :as e]
            [envrc.files :as files]
            [clojure.string :as str]))

(defn- resolved-paths [cfg]
  (files/resolve (get-in cfg [:files :local]) (:files cfg)))

(defn apply-impl
  "Append :local paths to <root>/.git/info/exclude. Returns the vector of
   newly-appended lines (empty if nothing added; nil if no .git/)."
  [cfg {:keys [label args root]}]
  (let [root  (or root (e/root))
        paths (resolved-paths cfg)]
    (when (seq paths)
      (files/append-git-exclude root paths))))

(defn status-impl
  "Brief: one-liner counts. Verbose: list missing entries."
  [cfg {:keys [label brief?]}]
  (let [paths   (resolved-paths cfg)
        present (set (files/read-git-exclude-lines (e/root)))
        missing (vec (remove present paths))
        ok      (- (count paths) (count missing))]
    (println (str "  local — " ok "/" (count paths) " in .git/info/exclude"
                  (when (seq missing) (str "; " (count missing) " missing"))))
    (when (and (not brief?) (seq missing))
      (doseq [p missing] (println "    missing:" p))
      (println "    run `envrc apply local` to add"))
    nil))

(def plugin
  {:id "local"
   :description "Adds :local-labeled paths to .git/info/exclude on workspace creation"
   :requires    ["git"]
   :handles     #{:local}
   :cli         {:apply apply-impl :status status-impl}
   :provides
   {:tasks
    {:local-on-workspace-new
     {:label    "Apply :local to .git/info/exclude in new worktree"
      :group    "internal"
      :on       :workspace-new
      :tolerant true
      :run      (fn [{:keys [cfg payload]}]
                  (doseq [line (or (apply-impl cfg {:root (:dst payload)}) [])]
                    (files/print-action :local line)))}}}})
