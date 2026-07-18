(ns envrc.schemas
  "Schemas as data + closest-match helper. No validation logic — see envrc.validate.
   After the system-wide simplification, top-level surface is the V2 reserved 6
   (:tasks :files :env :packages :use :config) plus envrc-core built-in :title.
   :panes / :default-agent / :agents / :exclude / :ref / :inputs moved to plugin
   :use slots — :inputs lives under :use {:flake {:inputs ...}} via envrc/flake.clj."
  (:require [clojure.string :as str]
            [malli.core :as m]))

(def reserved-task-names
  "Task names that collide with envrc CLI verbs and cannot be user-defined."
  #{:status :gen :config :pane :ws :services})

(def core-events
  #{:shell :gen :enter})

(def aliases
  "Manual semantic-synonym map. Distance-based closest-match (Levenshtein)
   catches typos like `taks → tasks`, but misses semantic synonyms like
   `cmds → tasks` or `deps → packages` because edit-distance exceeds the
   80%-of-longer-length bound. Keep this table small and intentional —
   four entries covers common cross-tool muscle memory."
  {"cmds"    "tasks"
   "scripts" "tasks"
   "deps"    "packages"
   "imports" "use"})

(defn- levenshtein
  "Edit distance — iterative DP, O(m*n)."
  [a b]
  (let [a (vec a) b (vec b)
        m (count a) n (count b)
        ^"[[I" dp (make-array Integer/TYPE (inc m) (inc n))]
    (dotimes [i (inc m)] (aset dp i 0 (int i)))
    (dotimes [j (inc n)] (aset (aget dp 0) j (int j)))
    (dotimes [i m]
      (dotimes [j n]
        (let [cost (if (= (nth a i) (nth b j)) 0 1)]
          (aset (aget dp (inc i)) (inc j)
                (int (min (inc (aget dp i (inc j)))
                          (inc (aget dp (inc i) j))
                          (+ cost (aget dp i j))))))))
    (aget dp m n)))

(defn closest-match
  "Returns the closest candidate string. Consults `aliases` first; on miss
   falls back to Levenshtein bounded by 80% of the longer string's length."
  [s candidates]
  (let [s' (name s)
        candidate-set (set (map name candidates))
        aliased (get aliases s')]
    (or (when (contains? candidate-set aliased) aliased)
        (let [scored (->> candidates
                          (map (fn [c]
                                 (let [c' (name c)]
                                   [c (levenshtein s' c') (max (count s') (count c'))])))
                          (filter (fn [[_ d longer]] (<= d (* 0.8 longer))))
                          (sort-by second))]
          (when (seq scored) (first (first scored)))))))

;; ============================================================
;; Malli schemas — base shapes (plugin extension folded in by
;; envrc.validate/build-effective-schemas, not here).
;; ============================================================

(def TaskName
  [:and keyword?
   [:fn {:error/message "task name collides with reserved verb"}
        (fn [k] (not (contains? reserved-task-names k)))]
   [:fn {:error/message "task name cannot start with `-`"}
        (fn [k] (not (str/starts-with? (name k) "-")))]])

(def Event
  ;; Base schema accepts any keyword; envrc.validate/build-effective-schemas
  ;; narrows this to the effective enum (core-events ∪ plugin :events) at load
  ;; time. Keeping base permissive lets plugin-contributed events validate
  ;; against the base Task shape without requiring the full effective schema.
  keyword?)

(def Run
  [:or string? [:sequential {:min 1} string?] fn?])


(def Task
  [:map {:closed true}
   [:label    {:optional true} string?]
   [:group    {:optional true} string?]
   [:run      {:optional true} Run]
   [:env      {:optional true} [:map-of keyword? [:maybe string?]]]
   [:on       {:optional true} [:or Event [:sequential Event]]]
   [:show     {:optional true} [:map [:pane keyword?]]]
   [:service  {:optional true} boolean?]
   [:tolerant {:optional true} boolean?]])

(def Tasks
  [:map-of TaskName Task])

(def FileRef [:or string? keyword?])

(def Files
  [:map-of keyword? [:or FileRef [:vector FileRef]]])

(def Dirs
  "Per-installation override of dir roots; consumed by envrc.dirs/resolve-dir
   via envrc.api/*context*. Anchors (:state/:cache/:runtime) are string paths;
   categories (:worktrees/:ref/:services/:socket) are maps carrying :base plus
   category-specific extras (:ref also takes :mirror)."
  (let [Category [:map {:closed false}
                  [:base {:optional true} string?]]]
    [:map {:closed false}
     [:state     {:optional true} string?]
     [:cache     {:optional true} string?]
     [:runtime   {:optional true} string?]
     [:worktrees {:optional true} Category]
     [:services  {:optional true} Category]
     [:socket    {:optional true} Category]
     [:ref       {:optional true}
      [:map {:closed false}
       [:base   {:optional true} string?]
       [:mirror {:optional true} string?]]]]))

(def Use
  [:map {:closed false}
   [:aliases {:optional true} [:map-of keyword? keyword?]]
   [:dirs    {:optional true} Dirs]])


(def EnvrcEdn-base
  [:map {:closed true}
   [:tasks    {:optional true} Tasks]
   [:files    {:optional true} Files]
   [:env      {:optional true} [:map-of keyword? [:maybe string?]]]
   [:packages {:optional true} [:sequential :any]]
   [:use      {:optional true} Use]
   [:config   {:optional true} :any]
   ;; envrc-core built-ins
   [:title    {:optional true} string?]])

(def DirsOverride
  "Per-plugin dirs override surface; plugins declare under :use {<id> {:dirs DirsOverride}}."
  [:map {:closed false}
   [:state   {:optional true} string?]
   [:cache   {:optional true} string?]
   [:runtime {:optional true} string?]])

(def PluginManifest
  [:map {:closed true}
   [:id          string?]
   [:description string?]
   [:category    {:optional true} [:or keyword? string?]]
   [:requires    {:optional true} [:vector string?]]
   [:capability  {:optional true} [:enum :pane :workspace :service :notifier]]
   [:handles     {:optional true} [:set keyword?]]
   [:events      {:optional true} [:set keyword?]]
   [:cli         {:optional true} [:map-of keyword? fn?]]
   [:extends     {:optional true}
    [:map {:closed true}
     [:use       {:optional true} :any]
     [:tasks     {:optional true} :any]
     [:files     {:optional true} :any]
     [:events    {:optional true} [:set keyword?]]
     [:emitters  {:optional true} [:map-of keyword? :any]]]]
   [:provides    {:optional true}
    [:or fn? [:map {:closed true}
              [:tasks {:optional true} :any]
              [:aliases {:optional true} [:map-of keyword? keyword?]]
              [:files {:optional true} Files]]]]
   [:status      {:optional true}
    [:map-of keyword?
     [:map [:human {:optional true} fn?]
           [:json  {:optional true} fn?]
           [:level {:optional true} [:enum :default :v :vv]]]]]
   [:source      {:optional true} string?]
   [:file        {:optional true} string?]])

(defn reserved-top-level-keys
  "Derived from EnvrcEdn-base — single source of truth for top-level field names."
  []
  (->> (m/children EnvrcEdn-base) (map first) set))

(defn reserved-task-fields
  "Derived from Task Malli schema — a flat [:map {:closed true} ...] after T3."
  []
  (->> (m/children Task) (map first) set))
