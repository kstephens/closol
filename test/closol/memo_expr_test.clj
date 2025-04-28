(ns closol.memo-expr-test
  (:require
    [clojure.test :refer :all]
    [closol.memo-expr :refer :all]))

(defmacro mock-generate-variable
  [e]
  `(let [counter# (atom 0)]
     (with-redefs [generate-variable
                   (fn [] (symbol (str "G" (swap! counter# inc))))]
     ~e)))

(deftest parameters-of-constant
  (testing "(parameters 1234)"
    (is (= (parameters '1234)
           #{}))))

(deftest parameters-of-parameter
  (testing "(parameters 'x"
    (is (= (parameters 'x)
           #{'x}))))

(deftest parameters-of-constant-expression
  (testing "(parameters '(+ 1 2)"
    (is (= (parameters '(+ 1 2))
           #{}))))

(deftest parameters-of-non-constant-expression
  (testing "(parameters '(+ x 2 y)"
    (is (= (parameters '(+ x 2 y))
           (set '(x y))))))

(deftest constant-expressions-should-not-be-memoized
  (testing "(memoize-subexpressions? 123)"
    (is (= (memoize-subexpressions? 123) false)))
  (testing "(memoize-subexpressions? (+ 1 123))"
    (is (= (memoize-subexpressions? '(+ 1 123)) false))))

(deftest parameters-should-not-be-memoized
  (testing "(memoize-subexpressions? 'x)"
    (is (= (memoize-subexpressions? 'x) false))))

(deftest simple-expressions-should-not-be-memoized
  (testing "(memoize-subexpressions? '(+ x 1))"
    (is (= (memoize-subexpressions? '(+ x 1)) false)))
  (testing "(memoize-subexpressions? '(* y 0.5))"
    (is (= (memoize-subexpressions? '(* y 0.5)) false))))

(deftest complex-expressions-should-be-memoized
  (testing "(memoize-subexpressions? '(f (+ x 1) (* y 0.5))"
    (is (= (memoize-subexpressions? '(f (+ x 1) (* y 0.5))) true))))
  
(deftest memoized-subexpression-on-const-is-unbound
  (testing "(memoized-subexpression 1234)"
    (is (= (memoized-subexpression 1234)
           '(1234 false false)))))

(deftest memoized-subexpression-on-parameter-is-unbound
  (testing "(memoized-subexpression 'x)"
    (is (= (memoized-subexpression 'x)
           '(x false false)))))

(deftest memoized-subexpression-on-parameter-is-bound-to-memoized-expression
  (testing "(memoized-subexpression '(+ x 1))"
    (mock-generate-variable
     (is (= (memoized-subexpression '(+ x 1))
            '((G1 x) G1 (clojure.core/memoize (clojure.core/fn [x] (+ x 1)))))))))

(deftest memoize-expression-on-constant-is-simple
  (mock-generate-variable
   (is (= (memoize-expression '(x y) '(+ 1.2 3.4))
          '(clojure.core/let []
             (clojure.core/fn [x y] (+ 1.2 3.4)))))))

(deftest memoize-expression-on-parameter-is-simple
  (mock-generate-variable
   (is (= (memoize-expression '(x y) 'y)
          '(clojure.core/let []
             (clojure.core/fn [x y] y))))))

(deftest memoize-expression-test
  (testing "memoize-expression"
    (mock-generate-variable
     (is (= (memoize-expression '(x y) '(f 1.2 (+ x (/ 3.4 x)) (* y 5.6)))
            '(clojure.core/let
                 [G1 (clojure.core/memoize
                      (clojure.core/fn [x] (+ x (/ 3.4 x))))
                  G2 (clojure.core/memoize
                      (clojure.core/fn [y] (* y 5.6)))]
               (clojure.core/fn [x y] (f 1.2 (G1 x) (G2 y)))))))))

