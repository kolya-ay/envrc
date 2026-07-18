(ns envrc.dirs
  "Per-project dir substrate. Returns absolute paths under
   <base>/envrc/<scope>/<slug>/... using the layout shapes defined in
   .ref/specs/2026-06-07-dirs-and-hooks-design.md."
  (:require [clojure.string :as str]
            [envrc.api :as api]))

(def ^:dynamic *env-overrides*
  "Rebind in tests to override specific env vars without touching the process env."
  nil)

(defn- env [k]
  (if-let [override (find *env-overrides* k)]
    (val override)
    (System/getenv k)))

(defn- anchor-override
  "String override for an XDG anchor under {:use :dirs <anchor>}.
   Only strings count (categories are maps and are resolved elsewhere)."
  [anchor]
  (let [v (get-in api/*context* [:cfg :use :dirs anchor])]
    (when (string? v) v)))

(defn sha1-hex [s]
  (let [md (java.security.MessageDigest/getInstance "SHA-1")
        bs (.digest md (.getBytes (str s) "UTF-8"))]
    (apply str (map #(format "%02x" %) bs))))

(defn state-base []
  (or (anchor-override :state)
      (env "XDG_STATE_HOME")
      (str (env "HOME") "/.local/state")))

(defn cache-base []
  (or (anchor-override :cache)
      (env "XDG_CACHE_HOME")
      (str (env "HOME") "/.cache")))

(defn runtime-base []
  (or (anchor-override :runtime)
      (env "XDG_RUNTIME_DIR")
      (throw (ex-info "runtime-dir requires XDG_RUNTIME_DIR" {}))))

(defn direnv-layout-base
  "Reproduces assets/direnv/stdlib.sh direnv_layout_dir():
   <cache>/direnv/layouts/<sha1(PWD)[0:10]><PWD with / → ->."
  []
  (let [pwd (env "PWD")]
    (str (cache-base) "/direnv/layouts/"
         (subs (sha1-hex pwd) 0 10)
         (str/replace pwd "/" "-"))))

(defn services-dir
  "process-compose state dir: the direnv layout dir, or a {:use :dirs :services :base} override.
   The layout dir is already project-unique, so service state files live flat inside it."
  []
  (or (get-in api/*context* [:cfg :use :dirs :services :base])
      (direnv-layout-base)))

(defn- scope-slug [{:keys [scope slug]}]
  (str (name scope) "/" slug))

(def ^:private dir-registry
  {:project    {:anchor :state         :layout (fn [ctx name _] (str "envrc/" (scope-slug ctx) "/" name))}
   :workspace  {:anchor :state         :layout (fn [ctx name _] (str "envrc/" (scope-slug ctx) "/" (:workspace ctx) "/" name))}
   ;; categorical: a :base override points at the category root, so it absorbs
   ;; the envrc/<category> prefix and only <scope>/<slug> is appended.
   :worktrees  {:anchor :state         :layout (fn [ctx _ {:keys [base-overridden?]}]
                                                 (str (when-not base-overridden? "envrc/worktrees/") (scope-slug ctx)))}
   :ref        {:anchor :state         :layout (fn [ctx _ {:keys [base-overridden?]}]
                                                 (str (when-not base-overridden? "envrc/ref/") (scope-slug ctx)))}
   :cache      {:anchor :cache         :layout (fn [ctx name opts]
                                                 (str "envrc" (when (= :direnv (:lifecycle opts)) "/direnv")
                                                      "/" (scope-slug ctx) "/" (:workspace ctx) "/" name))}
   :runtime    {:anchor :runtime       :layout (fn [ctx name _] (str "envrc/" (scope-slug ctx) "/" (:workspace ctx) "/" name))}
   :socket     {:anchor :runtime       :layout (fn [ctx _ _]    (str "envrc/" (scope-slug ctx) "/" (:workspace ctx) "/process-compose.sock"))}})

(defn- anchor-base [anchor]
  (case anchor
    :state         (state-base)
    :cache         (cache-base)
    :runtime       (runtime-base)))

(defn resolve-dir
  "Resolve a dir category to an absolute path. A category map override's :base
   short-circuits the anchor; otherwise <anchor-base>/<layout>."
  ([category ctx name] (resolve-dir category ctx name {}))
  ([category ctx name opts]
   (let [{:keys [anchor layout]} (dir-registry category)
         ov         (get-in api/*context* [:cfg :use :dirs category])
         ov-base    (when (map? ov) (:base ov))
         base       (or ov-base (anchor-base anchor))]
     (str base "/" (layout ctx name (assoc (or opts {}) :base-overridden? (some? ov-base)))))))

(defn project-dir
  "<state-base>/envrc/<scope>/<slug>/<name>"
  [ctx name]
  (resolve-dir :project ctx name))

(defn categorical-dir
  "Resolve a named category (:worktrees, :ref). Override is a map carrying :base."
  [ctx category]
  (resolve-dir (keyword category) ctx nil))

(defn sanitize-branch [s]
  (str/replace s "/" "--"))

(defn workspace-name
  "Resolves the current workspace name. Order: ENVRC_WORKSPACE env var,
   then the supplied branch-fn, then main-branch-fn fallback. Sanitizes via sanitize-branch."
  [{:keys [branch-fn main-branch-fn]}]
  (sanitize-branch
    (or (env "ENVRC_WORKSPACE")
        (branch-fn)
        (main-branch-fn))))

(defn workspace-dir
  "<state-base>/envrc/<scope>/<slug>/<workspace>/<name>"
  [ctx name]
  (resolve-dir :workspace ctx name))

(defn cache-dir
  "<cache-base>/envrc[/direnv]/<scope>/<slug>/<workspace>/<name>"
  ([ctx name] (cache-dir ctx name {}))
  ([ctx name opts] (resolve-dir :cache ctx name opts)))

(defn runtime-dir
  "<runtime-base>/envrc/<scope>/<slug>/<workspace>/<name>"
  [ctx name]
  (resolve-dir :runtime ctx name))
