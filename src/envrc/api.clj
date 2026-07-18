(ns envrc.api
  "Public API for envrc plugin authors. See `:require [envrc.api :as e]`."
  (:require [babashka.process]
            [babashka.fs]))

(def ^:dynamic *context*
  "Per-invocation context: {:cfg ... :root str :state str :capabilities {cap plugin-id}
   :events #{:fired ...} :plugins {id manifest}}."
  nil)

(defmacro with-context [ctx & body]
  `(binding [*context* ~ctx] ~@body))

(defn build-context
  "Build the standard invocation context map from cfg and root.
   Extracts :capabilities from (:dispatch cfg) so callers don't repeat the logic."
  [cfg root]
  (let [dispatch (or (:dispatch cfg) {})]
    {:cfg    cfg
     :root   root
     :plugins (:plugins cfg)
     :capabilities
     (into {}
           (for [[_ ent] dispatch
                 :let  [cap (:capability (:plugin ent))]
                 :when cap]
             [cap (:id (:plugin ent))]))}))


;; --- data accessors ---
(defn cfg     []      (:cfg *context*))
(defn root    []      (:root *context*))
(defn state   []      (:state *context*))
(defn env     [k]     (get-in *context* [:cfg :env (keyword k)]))
(defn use     [id]    (get-in *context* [:cfg :use (keyword id)]))
(defn tasks   []      (or (get-in *context* [:cfg :tasks]) {}))
(defn files   []      (or (get-in *context* [:cfg :files]) []))

;; --- pure helpers ---
(defn event-active? [ev] (contains? (:events *context* #{}) ev))
(defn plugin-of [cap]    (get-in *context* [:capabilities cap]))

;; --- effects (bang convention) ---
(defn exec! [argv & {:keys [dir env]}]
  (let [{:keys [exit out err]} (apply babashka.process/shell
                                       (cond-> {:out :string :err :string :continue true}
                                         dir (assoc :dir dir)
                                         env (assoc :extra-env env))
                                       argv)]
    {:exit exit :out out :err err}))

(defn write! [path content]
  (let [p (if (.startsWith ^String path "/") path (str (root) "/" path))]
    (babashka.fs/create-dirs (babashka.fs/parent p))
    (spit p content)))

(defn log! [level msg & {:as ks}]
  (binding [*out* *err*]
    (println (str "[" (name level) "] " msg
                  (when (seq ks) (str " " (pr-str ks)))))))

(defn fire!
  ([event]         (fire! event nil))
  ([event payload] ((requiring-resolve 'envrc.events/fire!) (cfg) event payload)))

(defn set-env! [k v]
  ;; Mutation buffer; loader flushes at end-of-script.
  (set! *context* (assoc-in *context* [:env-buffer (keyword k)] v)))
