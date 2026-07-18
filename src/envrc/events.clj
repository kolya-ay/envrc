(ns envrc.events
  (:require [clojure.set :as set]
            [envrc.schemas :as envrc.schemas]))

(def ^:dynamic *bus* {})

(defmacro with-bus [bus & body]
  `(binding [*bus* ~bus] ~@body))

(defn- known-events [cfg]
  (apply set/union
         envrc.schemas/core-events
         (map (fn [[_ m]] (or (:events m) #{})) (:plugins cfg))))

(defn fire!
  "Runs subscribers for event in declared order. Aborts on first exception
   unless the throwing subscriber's metadata has {:tolerant true}.
   Subscribers are 1-arity, receiving the optional `payload` (nil if absent).
   Throws if event is not declared (core ∪ plugin :events)."
  ([cfg event] (fire! cfg event nil))
  ([cfg event payload]
   (when-not (contains? (known-events cfg) event)
     (throw (ex-info (str "envrc: undeclared event `" (name event)
                          "`. Declared events: " (vec (sort (known-events cfg))))
                     {:event event :reason :undeclared-event})))
   (doseq [sub (get *bus* event [])]
     (try
       (sub payload)
       (catch Throwable t
         (if (:tolerant (meta sub))
           (binding [*out* *err*]
             (println (str "envrc warn: tolerant subscriber of " event " failed: " (.getMessage t))))
           (throw t)))))))

(defn subscribers-for [event]
  (get *bus* event []))
