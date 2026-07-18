(ns envrc.plugin.ref
  "Project-shared externalized <root> dir. Lazy lifecycle — `envrc status ref`
   warns on misconfig; `envrc apply ref` is the only mutation verb (idempotent
   in absent/symlink cases; destructive on real directory)."
  (:require [envrc.api :as e]
            [envrc.dirs :as dirs]
            [envrc.files :as files]
            [envrc.git :as git]
            [clojure.string :as str]
            [babashka.fs :as fs]))

(defn- warn [& xs] (binding [*out* *err*] (apply println xs)))

(defn- ref-state [local]
  (cond (not (fs/exists? local)) :absent
        (fs/sym-link? local)     :symlink
        (fs/directory? local)    :directory
        :else                    :unknown))

(defn- name-from-use [cfg]
  (or (get-in cfg [:use :ref :root]) ".ref"))

(defn- ctx-from-cfg [cfg]
  (select-keys (:project cfg) [:scope :slug :workspace]))

(defn- mirror-base [cfg]
  (or (get-in cfg [:use :dirs :ref :mirror]) "."))

(defn- collisions
  "Resolved paths grouped by mirror basename, keeping only basenames claimed
   by 2+ paths."
  [paths]
  (->> paths
       (group-by #(fs/file-name (str/replace % #"/$" "")))
       (filter (fn [[_ ps]] (> (count ps) 1)))))

(defn- mirror-paths [cfg name root paths]
  (let [dest (str (fs/normalize (str root "/" name "/" (mirror-base cfg))))]
    (fs/create-dirs dest)
    (doseq [[base ps] (collisions paths)]
      (warn (str "  ref: " (str/join ", " ps)
                 " all mirror to " base " (last wins)")))
    (doseq [p paths] (files/mirror-into dest root p))))

(defn ensure-symlink!
  "Idempotent: if local is absent or a symlink, make it point at target.
   Does NOT create target. Returns :linked on create/replace, :ok if already
   correct, nil if state is unsuited (real directory / unknown)."
  [{:keys [local target]}]
  (case (ref-state local)
    :absent    (do (fs/create-sym-link local target) :linked)
    :symlink   (let [actual (str (fs/read-link local))]
                 (if (= actual target)
                   :ok
                   (do (fs/delete local)
                       (fs/create-sym-link local target)
                       :linked)))
    :directory nil
    :unknown   nil))

(defn apply-impl
  [cfg {:keys [label args root]}]
  (let [name   (name-from-use cfg)
        root   (or root (e/root))
        target (dirs/categorical-dir (ctx-from-cfg cfg) "ref")
        paths  (files/resolve (get-in cfg [:files :ref]) (:files cfg))
        local  (str root "/" name)]
    (fs/create-dirs target)
    (case (ref-state local)
      (:absent :symlink)
      (when (= :linked (ensure-symlink! {:local local :target target}))
        (files/print-action :ref (str "→ " target)))

      :directory
      (do (warn (str "moving " local " → " target " (destructive)"))
          (doseq [child (fs/list-dir local)]
            (fs/move (str child) (str target "/" (fs/file-name child))))
          (fs/delete local)
          (fs/create-sym-link local target)
          (files/print-action :ref (str "→ " target)))

      :unknown
      (throw (ex-info (str "ref state unclear for " local "; refusing to act")
                      {:local local :state :unknown})))
    (when (and (seq paths) (git/main-checkout? root))
      (mirror-paths cfg name root paths)
      (doseq [p paths] (files/print-action :mirrored p)))
    nil))

(defn status-impl
  [cfg {:keys [label brief?]}]
  (let [name   (name-from-use cfg)
        root   (e/root)
        target (dirs/categorical-dir (ctx-from-cfg cfg) "ref")
        paths  (files/resolve (get-in cfg [:files :ref]) (:files cfg))
        local  (str root "/" name)
        state  (ref-state local)]
    (let [colls (count (collisions paths))]
      (println (str "  ref — " (clojure.core/name state)
                    (when (seq paths) (str "; " (count paths) " mirror paths"))
                    (when (pos? colls) (str ", " colls " basename " (if (= 1 colls) "collision" "collisions"))))))
    (when-not brief?
      (case state
        :absent    (when (seq paths)
                     (println (str "    " name " absent, would symlink to " target))
                     (println "    run `envrc apply ref` to create"))
        :symlink   (let [actual (str (fs/read-link local))]
                     (when-not (= actual target)
                       (println (str "    " name " → " actual ", expected " target))
                       (println "    run `envrc apply ref` to relink")))
        :directory (do (println (str "    " name " is a real directory"))
                       (println (str "    run `envrc apply ref` to MOVE contents into " target " (destructive)")))
        :unknown   (println (str "    " name " state unclear")))
      (when-not (git/main-checkout? root)
        (println "    mirror managed from main checkout")))
    nil))

(def plugin
  {:id "ref"
   :description "Project-shared externalized <root> dir; mirror :ref-labeled paths into <root>/, or a subdir set by :mirror"
   :events #{}
   :handles #{:ref}
   :extends {:use [:map [:root {:optional true} string?]]}
   :cli {:apply apply-impl :status status-impl}
   :provides
   {:tasks
    {:ref-on-workspace-new
     {:label    "Set up <root>/.ref symlink in new worktree if target exists"
      :group    "internal"
      :on       :workspace-new
      :tolerant true
      :run      (fn [{:keys [cfg payload]}]
                  (let [{:keys [dst ctx]} payload
                        name   (name-from-use cfg)
                        target (dirs/categorical-dir ctx "ref")]
                    (if (fs/exists? target)
                      (let [status (ensure-symlink! {:local  (str dst "/" name)
                                                     :target target})]
                        (when (= :linked status)
                          (files/print-action :ref (str "→ " target))))
                      (println (format "  %-7s %s absent, skipping" "ref" target)))))}}}})
