(ns closol.random-test
  (:require [clojure.test :refer :all]
            [closol.random :refer :all]))

(deftest a-test
  (let [r (make-random 20130827)]
    (dosync
      (testing "OK"
        (is (= (map (fn [_] (r 10)) (range 0 10))
              '(2 3 1 0 9 6 4 7 6 4)))
        )
      )))
