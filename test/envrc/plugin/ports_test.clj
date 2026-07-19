(ns envrc.plugin.ports-test
  (:require [clojure.test :refer [deftest is testing]]
            [envrc.validate :as v]))

(load-file "plugins/default/ports.clj")

(defn- plugin [] @(resolve 'envrc.plugin.ports/plugin))
(defn- ports-shell [] (-> (plugin) (get-in [:extends :emitters :ports :transform])))
(defn- use-schema [] (get-in (plugin) [:extends :use]))

;; -----------------------------------------------------------------
;; Manifest shape
;; -----------------------------------------------------------------

(deftest plugin-declares-ports-emitter
  (let [emitter (get-in (plugin) [:extends :emitters :ports])]
    (is (= :cfg (:input emitter)))
    (is (= :raw (:encode emitter)))
    (is (fn? (:transform emitter)))))

(deftest plugin-declares-status-handler
  (is (= #{:ports} (:handles (plugin))))
  (is (fn? (get-in (plugin) [:cli :status]))))

(deftest plugin-extends-use-with-schema
  (is (some? (use-schema)))
  (testing "schema validates a well-formed config"
    (is (nil? (v/validate! (use-schema)
                           {:base 4000 :stride 10 :vars [:A :B]}
                           :reason :invalid-use-slot)))))

;; -----------------------------------------------------------------
;; ports-shell behavior
;; -----------------------------------------------------------------

(deftest ports-shell-empty-when-not-opted-in
  (is (= "" ((ports-shell) {:project {:workspace "main"}}))))

(deftest ports-shell-emits-lines-when-opted-in
  (let [out ((ports-shell)
             {:project {:workspace "feature--auth"}
              :use {:ports {:base 4000 :stride 10
                            :vars [:AIST_PORT :UI_PORT]
                            :derive {:SERVER_URL "http://127.0.0.1:${AIST_PORT}"}}}})]
    (is (re-find #"(?m)^export AIST_PORT='\d+'$" out))
    (is (re-find #"(?m)^export UI_PORT='\d+'$" out))
    (is (re-find #"(?m)^export SERVER_URL='http://127\.0\.0\.1:\d+'$" out))))

;; -----------------------------------------------------------------
;; Override precedence: env > config :offset > hash
;; -----------------------------------------------------------------

(defn- get-port [out var-name]
  (let [m (re-find (re-pattern (str "(?m)^export " var-name "='(\\d+)'$")) out)]
    (when m (Integer/parseInt (second m)))))

(deftest env-var-overrides-config-and-hash
  ;; Redef the env-read seam (env-offset) rather than System/getenv —
  ;; SCI's static-method redefinition is awkward.
  (let [cfg {:project {:workspace "main"}
             :use {:ports {:base 4000 :stride 10 :vars [:A] :offset 50}}}]
    (with-redefs [envrc.plugin.ports/env-offset (constantly 7)]
      (let [out ((ports-shell) cfg)]
        (is (= 4070 (get-port out "A")))))))

(deftest config-offset-overrides-hash
  (let [cfg {:project {:workspace "main"}
             :use {:ports {:base 4000 :stride 10 :vars [:A] :offset 5}}}]
    (with-redefs [envrc.plugin.ports/env-offset (constantly nil)]
      (let [out ((ports-shell) cfg)]
        (is (= 4050 (get-port out "A")))))))

(deftest hash-used-when-no-overrides
  (let [cfg {:project {:workspace "main"}
             :use {:ports {:base 4000 :stride 10 :vars [:A]}}}]
    (with-redefs [envrc.plugin.ports/env-offset (constantly nil)
                  envrc.ports/offset           (constantly 42)]
      (let [out ((ports-shell) cfg)]
        (is (= 4420 (get-port out "A")))))))

(deftest bad-env-offset-raises
  (with-redefs [envrc.plugin.ports/env-offset (fn [_] (throw (ex-info "bad" {})))]
    (is (thrown? Exception
          ((ports-shell)
           {:project {:workspace "main"}
            :use {:ports {:base 4000 :stride 10 :vars [:A]}}})))))

;; -----------------------------------------------------------------
;; Schema rejection paths (via the project's validate path)
;; -----------------------------------------------------------------

(deftest schema-rejects-bad-configs
  (doseq [[label cfg] [[:missing-base       {:vars [:A]}]
                       [:missing-vars       {:base 4000}]
                       [:empty-vars         {:base 4000 :vars []}]
                       [:offset-out-of-range {:base 4000 :vars [:A] :offset 100}]
                       [:non-int-base       {:base "4000" :vars [:A]}]
                       [:unknown-key        {:base 4000 :vars [:A] :bogus 1}]]]
    (testing (str "schema rejects " (name label))
      (is (thrown? Exception
            (v/validate! (use-schema) cfg :reason :invalid-use-slot))))))
