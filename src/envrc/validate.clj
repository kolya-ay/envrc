(ns envrc.validate
  "Single validate! entry point. Wraps Malli explain, translates errors into
   envrc's ex-info shape with caller-supplied :reason codes and closest-match
   :suggestion (consulting envrc.schemas/aliases for synonyms then Levenshtein)."
  (:require [malli.core :as m]
            [envrc.schemas :as schemas]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn- first-disallowed-error
  "Walks Malli explain errors; returns the first error that violated a closed-map
   constraint, or nil. Handles both :malli.core/extra-key and ::m/extra-key tags
   defensively across Malli versions."
  [{:keys [errors]}]
  (some (fn [err]
          (let [t (:type err)
                in (:in err)]
            (when (and (qualified-keyword? t)
                       (or (= t :malli.core/extra-key)
                           (= t ::m/extra-key)
                           (= (namespace t) "malli.core"))
                       (seq in))
              err)))
        errors))

(defn- format-error-line [err]
  (let [path (str/join "/" (map (fn [p]
                                  (cond
                                    (keyword? p) (name p)
                                    :else        (str p)))
                                (:in err)))
        msg  (or (get-in err [:schema 1 :error/message])
                 (some-> err :type str)
                 (pr-str (:schema err)))]
    (str "  " path " — " msg)))

(defn validate!
  "Run Malli explain on schema + value. Returns nil on success; on failure throws
   ex-info with:
     :reason     <code>      caller-supplied (default :validation-error)
     :errors     <Malli explain output>   ALL errors (not just the first)
     :suggestion <string|nil> closest-match for first disallowed key
     :key        <keyword|nil> the key that triggered the error

   The thrown message accumulates every error from `m/explain` into a single
   report with a count header and one indented line per error — letting users
   fix a batch in one pass and giving forthcoming status views a comprehensive
   surface to render.

  Optional kwargs:
     :reason       — ex-info :reason code
     :allowed-keys — either a vector of accepted keys, or a function from
                     the error's :in path to a vector of candidates for that
                     nesting depth (enables path-aware suggestions, e.g.
                     task-field candidates when the bad key sits under :tasks)."
  [schema value & {:keys [reason allowed-keys]
                   :or {reason :validation-error}}]
  (when-let [explained (m/explain schema value)]
    (let [errors     (:errors explained)
          n          (count errors)
          header     (str "envrc: " n " error" (when (not= n 1) "s"))
          lines      (map format-error-line errors)
          msg        (str/join "\n" (cons header lines))
          bad-err    (first-disallowed-error explained)
          bad-key    (some-> bad-err :in last)
          candidates (cond
                       (fn? allowed-keys)     (allowed-keys (:in bad-err))
                       (vector? allowed-keys) allowed-keys
                       :else                  nil)
          suggestion (when (and bad-key (seq candidates))
                       (schemas/closest-match (name bad-key)
                                              (map name candidates)))]
      (throw (ex-info msg
                      {:reason reason
                       :errors errors
                       :suggestion suggestion
                       :key bad-key})))))

(defn- collect-plugin-events [plugins]
  (apply set/union #{}
         (map (fn [[_ m]] (or (get-in m [:extends :events]) #{}))
              plugins)))

(defn- build-event-schema [plugins]
  (let [all (set/union schemas/core-events (collect-plugin-events plugins))]
    (into [:enum] (sort all))))

(defn- build-task-schema
  "Fold plugin :extends.tasks schemas under :<plugin-id> sub-maps. Each plugin
   provides a single Malli schema (typically [:map ...]) for its supervisor /
   capability-specific fields; the schema is wired into the effective Task as
   [:<plugin-id> {:optional true} <plugin-schema>] so users author
   {:my-task {:run \"...\" :process-compose {:availability ...}}}."
  [plugins event-schema]
  (let [plugin-entries (->> plugins
                            (keep (fn [[id m]]
                                    (when-let [schema (get-in m [:extends :tasks])]
                                      [(keyword id) {:optional true} schema])))
                            vec)
        base-map [:map {:closed true}
                  [:label    {:optional true} string?]
                  [:group    {:optional true} string?]
                  [:run      {:optional true} schemas/Run]
                  [:env      {:optional true} [:map-of keyword? [:maybe [:or string? number?]]]]
                  [:on       {:optional true} [:or event-schema [:sequential event-schema]]]
                  [:show     {:optional true} [:map [:pane keyword?]]]
                  [:service  {:optional true} boolean?]
                  [:tolerant {:optional true} boolean?]]]
    (into base-map plugin-entries)))

(defn build-effective-schemas
  "Returns {:Event ... :Task ... :Tasks ... :Files ... :EnvrcEdn ...}
   with plugin-contributed vocabulary folded in. Pass the discovered plugins map."
  [plugins]
  (let [Event  (build-event-schema plugins)
        Task   (build-task-schema plugins Event)
        Tasks  [:map-of schemas/TaskName Task]
        Files  schemas/Files
        EnvrcEdn
        [:map {:closed true}
         [:tasks    {:optional true} Tasks]
         [:files    {:optional true} Files]
         [:env      {:optional true} [:map-of keyword? [:maybe [:or string? number?]]]]
         [:packages {:optional true} [:sequential :any]]
         [:use      {:optional true} [:map-of keyword? :any]]
         [:config   {:optional true} :any]
         [:title    {:optional true} string?]]]
    {:Event Event :Task Task :Tasks Tasks :Files Files :EnvrcEdn EnvrcEdn}))
