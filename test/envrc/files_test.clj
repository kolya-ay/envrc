(ns envrc.files-test
  (:require [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]
            [envrc.files :as f]))

(deftest link-into-skips-real-directory-instead-of-crashing
  (let [root (fs/create-temp-dir {:prefix "envrc-link-"})
        src  (str root "/src")
        dst  (str root "/dst")]
    (fs/create-dirs (str src "/envrc"))
    (spit (str src "/envrc/a") "1")
    (fs/create-dirs (str dst "/envrc"))       ; pre-existing real, non-empty dir
    (spit (str dst "/envrc/keep") "2")
    (is (= :skipped (f/link-into src dst "envrc")) "returns :skipped, no throw")
    (is (fs/directory? (str dst "/envrc")) "dst dir left intact")
    (is (not (fs/sym-link? (str dst "/envrc"))))
    (is (fs/exists? (str dst "/envrc/keep")) "existing content preserved")))
