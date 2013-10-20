(ns closol.random-test
  (:require
    [clojure.test :refer :all]
    [closol.random :refer :all]))

(deftest a-test
  (let [r (make-random 20130827)]
    (dosync
      (testing "(r 10)"
        (is (= (map (fn [_] (r 10)) (range 0 10))
              '(2 3 1 0 9 6 4 7 6 4)))
        )
      (testing "(random-sequence r [1 2 3 4 5])"
        (is (= (random-sequence r [0 1 2 3 4 5 6 7 8 9])
              '(6 3 7 1 2 8 9 0 4 5)))
        )
      )))
