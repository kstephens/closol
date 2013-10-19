(ns closol.vfunc)
(define v-neg -)

(define (v-real-part x)
  (set! x (real-part x))
  (if (eqv? x +nan.0)
    0
    x))

(define v-+ +)
(define v-- -)
(define v-* *)
(define (v-/ x y)
  (if (zero? y)
    x
    (/ x y)))
  
(define (v-expt x y)
  (if (zero? x)
    0
    (expt x y)))


(define (v-mod-1 x y)
  (let ((d (v-/ x y)))
    (* (- d (floor d)) y)))
  
(define (v-mod x y)
  (set! y (v-real-part y))
  (if (zero? y)
    x
    (v-mod-1 (v-real-part x) y)))
  
(define (v-floor x)
  (floor (v-real-part x)))
  

;; Trancendentals
(define v-sin sin)
(define v-cos cos)
(define (v-atan x y)
  (set! x (v-real-part x))
  (set! y (v-real-part y))
  (if (and (zero? x) (zero? y))
    0
    (atan x y)))
  
(define (v-make-rectangular a b)
  (make-rectangular (v-real-part a) (v-real-part b)))

(define (v-make-polar a b)
  (make-rectangular (v-real-part a) (v-real-part b)))
  
(define v-magnitude magnitude)

(define (v-angle a)
  (if (zero? a)
    0
    (angle a)))
  
;; Conditionals
(define (v-if a b c)
  (if (positive? (v-real-part a)) b c))
  
(define (v-clamp x a b)
  (set! x (v-real-part x))
  (set! a (v-real-part a))
  (set! b (v-real-part b))
  (if (> a b)
    (let ((t a))
      (set! a b)
      (set! b t))
    )
  (if (< x a)
    a
    (if (> x b)
      b
      x)))
  
;; Linear Interpolation:
;; x in [0, 1] => [x0, x1].
(define (v-lerp x x0 x1)
  (+ (* x0 (- 1 x)) (* x1 x)))
  
;; Inverse of lerp:
;; x in [x0, x1] => [0, 1].
(define (v-lerp-1 x x0 x1)
  (if (= x1 x0)
    0
    (/ (- x x0) (- x1 x0))))

