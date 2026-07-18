(ns envrc.plugin.my-plugin-test
  (:require [clojure.test :refer [deftest is]]
            [envrc.plugin.my-plugin :as plugin]))

(deftest manifest-has-id
  (is (= "my-plugin" (:id plugin/plugin))))
