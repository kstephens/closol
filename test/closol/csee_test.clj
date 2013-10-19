(ns closol.csee-test
  (:require [clojure.test :refer :all]
            [closol.csee :refer :all]))

(deftest a-test
  (testing "OK"
    (is (csee 'x) 'x)))
