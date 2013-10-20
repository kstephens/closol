(ns closol.csee-test
  (:require [clojure.test :refer :all]
            [closol.csee :refer :all]))

(deftest a-test
  (testing "csee"
    (is (= (csee 'x)
          'x))
    (is (= (csee '(+ x y))
          '(+ x y)))
    (is (= (csee '(* (+ x 2 y) z (+ x 2 y)))
          '(clojure.core/let [%g0 (+ x 2 y)] (* %g0 z %g0))))
    ))
