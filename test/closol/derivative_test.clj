(ns closol.derivative-test
  (:require
    [clojure.test :refer :all]
    [closol.derivative :refer :all]))

(defmacro -test [expr expected]
  `(is (= ~expr ~expected)))

(defmacro -test-not [expr expected]
  `(is (not (= ~expr ~expected))))

(deftest expand-expression-test
  (testing "expand-expression"
    (-test (expand-expression `(+)) `0)
    (-test (expand-expression `(+ x)) `x)
    (-test (expand-expression `(+ 0 x)) `x)
    (-test (expand-expression `(+ x 0)) `x)
    (-test (expand-expression `(+ 3 4)) `7)
    (-test (expand-expression `(+ x 4)) `(+ 4 x))
    (-test (expand-expression `(+ 1 0 2 3 0 4)) `10)
    (-test (expand-expression `(- (- x))) `x)
    (-test (expand-expression `(+ 0 (- (- (+ 5 4))))) `9)
    ))

#_
(deftest derivative-test
  (testing "d"
    (-test (d '123 'x) 0)
    (-test (d 'x 'x) 1)
    (-test (d '(* x 2) 'x) '2)
    (-test (d '(* x x) 'x) '(* 2 x))
    (-test (d '(* 3 x x) 'x) '(* 6 x))
    ;; (set! *pattern:debug* #t)
    (-test (d '(+ (* x 3) (* x x)) 'x) '(+ 3 (* 2 x)))
   
    (-test (d '(cos u) 'u)
      '(- (sin u)))
    (-test (d '(cos (* 2 u)) 'u) 
      '(* -2 (sin (* 2 u))))   
    (-test (d '(vector 1 (expt u 3) 3) 'u)
      '(vector 0 (* 3 (expt u 2)) 0))
    (-test (d (d '(* 3 x x) 'x) 'x)
      '6)
    (-test (d '(* 3 x y) 'x)
      '(* 3 (+ (* x (d y x)) y)))
    (-test (d (d '(* 3 x y) 'x) 'x)
      '(* 3 (+ (* x (d (d y x) x)) (* 2 (d y x)))))
    (-test (d '(* x (+ 2 4 (* (expt (* x x) 3) y))) 'x)
      '(+ 6 (+ (* x (+ (* (expt x 6) (d y x)) (* 6 (* y (expt x 5))))) (* (expt x 6) y))))
   
    (-test (d '(expt f g) 'x)
      '(* (expt f g) (+ (* (d f x) (/ g f)) (* (d g x) (log f)))))
    ;;(set! *pattern:debug* #t)
    
    (let [ sphere
           (vector
             '(* r (sin (* pi v)) (cos (* 2 pi u)))
             '(* r (sin (* pi v)) (sin (* 2 pi u)))
             '(* r (cos (* pi v)))
             ) ]
      (-test (d
               (nth sphere 0)
               'u
               '(r pi v)) 
        '(* r (* (sin (* pi v)) (* (* -2 pi) (sin (* 2 (* pi u)))))))
      )
    ))
