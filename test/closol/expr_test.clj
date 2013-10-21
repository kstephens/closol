(ns closol.expr-test
  (:require
    [clojure.test :refer :all]
    [closol.expr :refer :all]))

(deftest subexpressions-test
  (testing "subexpressions"
    (is (= (subexpressions '1)
      '(1)))
    (is (= (subexpressions 'x)
      '(x)))
    (is (= (subexpressions '(+ 1 2))
      '((+ 1 2) 1 2)))
    (is (= (subexpressions '(* 2 (+ 1 2) (- 1 x)))
      '((* 2 (+ 1 2) (- 1 x)) 2 (+ 1 2) 1 2 (- 1 x) 1 x)))
    ))
(deftest expression-complexity-test
  (testing "expression-complexity"
    (is (= (expression-complexity '1)
      0))
    (is (= (expression-complexity 'x)
      1))
    (is (= (expression-complexity '(+ 1 2))
      2))
    (is (= (expression-complexity '(+ 1 x y))
      4))
    (is (= (expression-complexity '(* 2 (+ x 2) (- 1 x)))
      8))
    ))
(deftest constant-fold-test
  (testing "constant-fold"
    (is (= (constant-fold 'x) 'x))
    (is (= (constant-fold '2345) '2345))
    (is (= (constant-fold '(+ 1 2)) '3))
    (is (= (constant-fold '(+ 1 x (* 2 3)))) '(+ 1 x 6))
    ))