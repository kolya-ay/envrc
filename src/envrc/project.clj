(ns envrc.project
  "Project registry: resolve + record rc-marked projects into
   ~/.local/state/projects.json. Pure logic lives here; the discovered
   plugin envrc.plugin.project is a thin CLI shell over it."
  (:require [babashka.fs :as fs]
            [cheshire.core :as json]
            [clojure.string :as str]
            [core :as c]
            [envrc.data :as data]))

(defn slugify [s]
  (-> (str s)
      str/lower-case
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-+|-+$" "")))

(defn humanize [slug]
  (->> (str/split (str slug) #"-")
       (remove str/blank?)
       (map str/capitalize)
       (str/join " ")))

(defn registry-path []
  (str (or (System/getenv "XDG_STATE_HOME")
           (str (System/getenv "HOME") "/.local/state"))
       "/envrc/projects.json"))

(defn read-registry
  "Parse the string-keyed registry JSON at `path`. Missing file or non-object
   top-level → {}. Throws on malformed JSON; callers decide how to recover."
  [path]
  (if (fs/exists? path)
    (let [parsed (json/parse-string (slurp path))]
      (if (map? parsed) parsed {}))
    {}))

(defn registry-json [registry]
  (json/generate-string (into (sorted-map) registry) {:pretty true}))

(defn write-registry!
  "Atomically write the registry to `path` (temp file in the same dir + rename)."
  [path registry]
  (fs/create-dirs (fs/parent path))
  (let [tmp (str path ".tmp." (System/currentTimeMillis))]
    (spit tmp (registry-json registry))
    (fs/move tmp path {:replace-existing true :atomic-move true})))

(defn- flake-description [dir]
  (let [f (str dir "/flake.nix")]
    (when (fs/exists? f)
      (some-> (re-find #"description\s*=\s*\"((?:[^\"\\]|\\.)*)\"" (slurp f))
              second))))

(defn- package-json-name [dir]
  (let [f (str dir "/package.json")]
    (when (fs/exists? f)
      (try (get (json/parse-string (slurp f)) "name")
           (catch Exception _ nil)))))

(defn- pyproject-name [dir]
  (let [f (str dir "/pyproject.toml")]
    (when (fs/exists? f)
      (try (some-> (re-find #"(?m)^\s*name\s*=\s*\"([^\"]+)\"" (slurp f)) second)
           (catch Exception _ nil)))))

(defn infer-title
  "First non-blank of: cfg :title, flake description, package.json name,
   pyproject name. nil when none."
  [dir cfg-title]
  (or (when-not (str/blank? (str cfg-title)) (str cfg-title))
      (flake-description dir)
      (package-json-name dir)
      (pyproject-name dir)))

(defn infer-slug
  "Pinned arg → slugify(title) → slugify(basename)."
  [dir title slug-arg]
  (cond
    (not (str/blank? (str slug-arg))) (slugify slug-arg)
    (not (str/blank? (str title)))    (slugify title)
    :else                             (slugify (fs/file-name dir))))

(defn derive-scope
  "Longest-prefix match of `dir` against scopes :entries :dir; else :default."
  [dir {:keys [default entries]}]
  (let [dir* (str dir)
        match (->> entries
                   (filter (fn [[_ {d :dir}]]
                             (and d (or (= dir* d)
                                        (str/starts-with? dir* (str d "/"))))))
                   (sort-by (fn [[_ {d :dir}]] (- (count (str d))))) ; longest first
                   first)]
    (name (or (some-> match key) default :ego))))

(defn resolve-entry
  "Pure resolution of the registry entry for a project at `dir`.
   opts: {:cfg-title <str|nil> :slug-arg <str|nil> :scopes <scopes-cfg>}."
  [dir {:keys [cfg-title slug-arg scopes]}]
  (let [path  (str dir)
        title (infer-title dir cfg-title)
        slug  (let [s (infer-slug dir title slug-arg)]
                (if (str/blank? s) "project" s))
        title (-> (or title (humanize slug))
                  (str/replace #"[\t\r\n]+" " "))
        scope (derive-scope dir scopes)]
    {:slug slug :title title :scope scope :path path}))

(defn entry-status
  "Classify what recording `entry` does to the slug-keyed `registry`:
   :created (slug unseen), :updated (new path or retitled known path),
   :conflict (new path under an existing slug whose title differs), or
   :unchanged (same path, same title)."
  [registry {:keys [slug title path]}]
  (if-let [existing (get registry slug)]
    (let [known-path (some #{path} (get existing "paths"))
          same-title (= (get existing "title") title)]
      (cond
        (and known-path same-title)             :unchanged
        (and (not known-path) (not same-title)) :conflict
        :else                                   :updated))
    :created))

(defn merge-entry
  "Fold a resolved entry into the string-keyed registry.
   Returns [registry' warnings]. Warnings are derived from `entry-status`,
   so classification and warnings can't drift apart."
  [registry {:keys [slug title scope path] :as entry}]
  (let [existing (get registry slug)
        warnings (case (entry-status registry entry)
                   :conflict [(str "slug '" slug "' already registered as '"
                                   (get existing "title") "'; '" path
                                   "' may be a different project")]
                   :updated  (if (= (get existing "title") title)
                               []
                               [(str "title for '" slug "' changed: '"
                                     (get existing "title") "' -> '" title "'")])
                   [])
        paths (vec (sort (distinct (conj (vec (get existing "paths")) path))))]
    [(assoc registry slug {"title" title "paths" paths "scope" scope}) warnings]))

(defn notification-for
  "Desktop-notification spec for a registration `status`, or nil to stay silent.
   Returns {:summary :body :urgency} | nil."
  [status {:keys [title scope slug]}]
  (case status
    :created  {:summary "Project registered"   :body (str title " · " scope)     :urgency "normal"}
    :updated  {:summary "Project updated"       :body title                       :urgency "low"}
    :conflict {:summary "Project slug conflict" :body (str "'" slug "' → " title) :urgency "critical"}
    nil))

(defn check-registry
  "Warnings for registered paths failing `exists?` (a path->bool fn)."
  [registry exists?]
  (vec (for [[slug entry] registry
             path (get entry "paths")
             :when (not (exists? path))]
         (str "project '" slug "' path missing: " path))))

(defn prune-registry
  "Drop paths failing `exists?` and any slug left empty.
   Returns [registry' removed-paths]."
  [registry exists?]
  (reduce
    (fn [[reg removed] [slug entry]]
      (let [keep (vec (filter exists? (get entry "paths")))
            gone (remove (set keep) (get entry "paths"))]
        [(if (seq keep) (assoc reg slug (assoc entry "paths" keep)) reg)
         (into removed gone)]))
    [{} []]
    registry))

(defn list-lines
  "One tab-delimited line per (project,path): title<TAB>slug<TAB>scope<TAB>path.
   Sorted by title then path."
  [registry]
  (->> (for [[slug entry] registry
             path (get entry "paths")]
         [(get entry "title") slug (get entry "scope") path])
       (sort-by (juxt first last))
       (map #(str/join "\t" %))))

(defn apply-register
  "Pure registry update. Returns [registry' entry warnings]."
  [registry dir opts]
  (let [entry (resolve-entry dir opts)
        [reg' warnings] (merge-entry registry entry)]
    [reg' entry warnings]))

(defn- read-registry-safe [path]
  (try (read-registry path)
       (catch Exception _
         (binding [*out* *err*]
           (println "envrc project: registry at" path "is corrupt; starting fresh"))
         {})))

(defn register!
  "Resolve + record the project at `dir` into projects.json.
   Returns {:entry <resolved-entry> :status <entry-status>}."
  [{:keys [dir slug]}]
  (let [dir       (str (fs/canonicalize dir))
        cfg-title (try (-> (data/load-project dir) second :title)
                       (catch Exception _ nil))
        scopes    (get-in (c/load-config) [:use :scopes])
        path      (registry-path)
        reg0      (read-registry-safe path)
        [registry entry warnings]
        (apply-register reg0 dir
                        {:cfg-title cfg-title :slug-arg slug :scopes scopes})
        status    (entry-status reg0 entry)]
    (write-registry! path registry)
    (doseq [w warnings] (binding [*out* *err*] (println "envrc project:" w)))
    {:entry entry :status status}))

(defn parse-worktree-porcelain
  "Parse `git worktree list --porcelain` output into [{:path :branch} ...].
   :branch is the short name, or \"detached\"/\"bare\" when there's no branch."
  [out]
  (->> (str/split (str out) #"\n\n")
       (keep (fn [block]
               (let [lines (str/split-lines block)
                     path  (some (fn [l] (when (str/starts-with? l "worktree ")
                                           (subs l (count "worktree ")))) lines)
                     bline (some (fn [l] (when (str/starts-with? l "branch ") l)) lines)
                     branch (cond
                              bline (str/replace (subs bline (count "branch ")) #"^refs/heads/" "")
                              (some #{"detached"} lines) "detached"
                              (some #{"bare"} lines) "bare"
                              :else "")]
                 (when path {:path path :branch branch}))))
       vec))

(defn worktree-entries
  "For each registered project path, list its git worktrees via `git-fn`
   (path -> seq of {:path :branch}), attaching project title/scope.
   Deduped by worktree path, first occurrence wins."
  [registry git-fn]
  (->> (for [[_ entry] registry
             proj-path (get entry "paths")
             wt        (git-fn proj-path)]
         {:title (get entry "title") :branch (:branch wt)
          :scope (get entry "scope") :path (:path wt)})
       (reduce (fn [[seen acc] e]
                 (if (seen (:path e))
                   [seen acc]
                   [(conj seen (:path e)) (conj acc e)]))
               [#{} []])
       second))

(defn worktree-lines
  "title<TAB>branch<TAB>scope<TAB>path per worktree. Stable-sorted by title so a
   project's worktrees group together; git order (main first) preserved within."
  [registry git-fn]
  (->> (worktree-entries registry git-fn)
       (sort-by :title)
       (map (fn [{:keys [title branch scope path]}]
              (str/join "\t" [title branch scope path])))))

(defn lookup-by-basename
  "Search the registry for an entry whose :path basename matches `basename`.
   Returns the entry map (with string keys) or nil."
  [basename]
  (let [registry (read-registry (registry-path))]
    (->> (vals registry)
         (filter (fn [entry]
                   (some (fn [path]
                           (= basename (-> path (clojure.string/split #"/") last)))
                         (get entry "paths"))))
         first)))
