(ns closol.expr-test
  (:require [clojure.test :refer :all]
            [closol.expr :refer :all]))

(deftest a-test
  (testing "enumerate-subexpressions"
    (is (enumerate-subexpressions '1)
      '(1))
    (is (enumerate-subexpressions 'x)
      '(x))
    (is (enumerate-subexpressions '(+ 1 2))
      '((+ 1 2) + 1 2))
    (is (enumerate-subexpressions '(* 2 (+ 1 2) (- 1 x)))
      '((* 2 (+ 1 2) (- 1 x)) * 2 (+ 1 2) + 1 2 (- 1 x) - 1 x))
    )
  (testing "expression-complexity"
    (is (expression-complexity '1)
      0)
    (is (expression-complexity 'x)
      1)
    (is (expression-complexity '(+ 1 2))
      2)
    (is (expression-complexity '(+ 1 x y))
      4)
    (is (expression-complexity '(* 2 (+ x 2) (- 1 x)))
      6)
    )
  )
