(ns closol.random-test
  (:require
    [clojure.test :refer :all]
    [closol.random :refer :all]))

(deftest make-random-test
  (testing "(r 10)"
    (let [r (make-random 1)]
      (dosync
        (is (= (map (fn [_] (r 10)) (range 0 10))
              '(2 3 2 7 2 3 5 9 5 7)))
        ))))
(deftest random-sequence-test
  (testing "(random-sequence r [1 2 3 4 5])"
    (let [r (make-random 2)]
      (dosync
        (is (= (random-sequence r [0 1 2 3 4 5 6 7 8 9])
              '(5 8 9 3 7 1 6 2 4 0)))
        ))))
(deftest random-merge-test
  (testing "(random-merge r a b)"
    (let [r (make-random 3)]
      (dosync
        (is (= (random-merge r '[a b c d] '[1 2 3 4 5] '[x y z])
              '(1 y c d 5)))
        )))
  (testing "(random-merge r seqs)"
    (let [r (make-random 3)]
      (dosync
        (is (= (random-merge r '[[a b c d] [1 2 3 4 5] [x y z]])
              '(1 y c d 5)))
        )))
  (testing "(random-merge r [ a ])"
    (let [r (make-random 4)]
      (dosync
        (is (= (random-merge r '[[a b c d]])
              '(a b c d)))
        ))))
