(ns envrc.ports-test
  (:require [clojure.test :refer [deftest is testing]]
            [envrc.ports :as ports]))

;; -----------------------------------------------------------------
;; offset: determinism + range
;; -----------------------------------------------------------------

(deftest offset-pinned-vectors
  ;; Lock the algorithm against silent drift. These values are
  ;; md5(handle)[0..4] mod 100 — recompute via `md5sum`+`bc` to verify.
  (is (= (mod (Integer/parseInt (subs "0cc175b9c0f1b6a831c399e269772661" 0 4) 16) 100)
         (ports/offset "a")))
  (testing "stable across calls"
    (is (= (ports/offset "main") (ports/offset "main")))
    (is (= (ports/offset "feature--auth") (ports/offset "feature--auth")))))

(deftest offset-range-always-0-99
  (doseq [s ["main" "feature/foo" "🦄"]]
    (let [o (ports/offset s)]
      (is (<= 0 o 99) (str "offset " o " out of range for " (pr-str s))))))

;; -----------------------------------------------------------------
;; slot-port
;; -----------------------------------------------------------------

(deftest slot-port-arithmetic
  (is (= 4731 (ports/slot-port 4000 10 73 1))))

;; -----------------------------------------------------------------
;; expand-template
;; -----------------------------------------------------------------

(deftest expand-template-substitutes-known-keys
  (is (= "http://127.0.0.1:4730"
         (ports/expand-template "http://127.0.0.1:${P}" {:P "4730"}))))

(deftest expand-template-multiple-vars
  (is (= "4730+4731"
         (ports/expand-template "${A}+${B}" {:A "4730" :B "4731"}))))

(deftest expand-template-unknown-var-raises-with-name
  (is (thrown-with-msg? Exception #"unknown \$\{MISSING\}"
        (ports/expand-template "x${MISSING}y" {:P "4730"}))))

;; -----------------------------------------------------------------
;; build-exports
;; -----------------------------------------------------------------

(deftest build-exports-vars-then-derive-in-order
  (let [out (ports/build-exports
              {:base   4000
               :stride 10
               :vars   [:A :B]
               :derive {:URL  "http://${A}"
                        :PAIR "${A}-${B}-${URL}"}}
              73)]
    (is (= [[:A "4730"] [:B "4731"]
            [:URL "http://4730"]
            [:PAIR "4730-4731-http://4730"]]
           out))))

(deftest build-exports-no-derive
  (is (= [[:AIST_PORT "4730"] [:UI_PORT "4731"]]
         (ports/build-exports
           {:base 4000 :stride 10 :vars [:AIST_PORT :UI_PORT]}
           73))))

(deftest build-exports-default-stride
  ;; Spec says default :stride 10; ensure it kicks in when omitted.
  (is (= [[:A "4730"]]
         (ports/build-exports {:base 4000 :vars [:A]} 73))))

(deftest build-exports-rejects-stride-less-than-vars
  (is (thrown-with-msg? Exception #":stride 2 < \(count :vars\) 3"
        (ports/build-exports
          {:base 4000 :stride 2 :vars [:A :B :C]} 0))))

(deftest build-exports-rejects-quote-in-derived-value
  (is (thrown-with-msg? Exception #"contains single quote"
        (ports/build-exports
          {:base 4000 :stride 10 :vars [:A]
           :derive {:BAD "ohno'"}}
          0))))

;; -----------------------------------------------------------------
;; shell-lines: quoting round-trip via bash
;; -----------------------------------------------------------------

(deftest shell-lines-format
  (is (= "export A='4730'\nexport URL='http://4730'"
         (ports/shell-lines [[:A "4730"] [:URL "http://4730"]]))))

