(ns closol.vfunc
  (:require
    [clojure.math.numeric-tower :as nt]))

;; Complex numbers are undefined in clojure.
(defn make-rectangular [a b] a)
(defn make-polar [a b] a)
(defn real-part [x] x)
(defn magnitude [x] (Math/abs x))
(defn angle [x] 0)
(defn positive? [x] (> x 0))

(defn isNaN? [x] (Double/isNaN x))
(defn v-real-part [xx]
  (let [x (real-part xx)]
    (if (isNaN? x)
      0
      x)))

(defn v-neg [x] (- x))
(defn v-add [x y] (+ x y))
(defn v-sub [x y] (- x y))
(defn v-mul [x y] (* x y))
(defn v-div [x y]
  (if (zero? y)
    x
    (/ x y)))
  
(defn v-expt [x y]
  (if (zero? x)
    0
    (nt/expt x y)))

(defn v-mod-1 [x y]
  (let [d (v-div x y)]
    (* (- d (nt/floor d)) y)))
  
(defn v-mod [x yy]
  (let [y (v-real-part yy)]
    (if (zero? y)
      x
      (v-mod-1 (v-real-part x) y))))
  
(defn v-floor [x]
  (nt/floor (v-real-part x)))

;; Trancendentals
(defn v-sin [x] (Math/sin x))
(defn v-cos [x] (Math/cos x))
(defn v-atan [xx yy]
  (let [x (v-real-part xx)
        y (v-real-part yy)]
    (if (and (zero? x) (zero? y))
      0
      (Math/atan2 x y))))

(defn v-make-rectangular [a b]
  (make-rectangular (v-real-part a) (v-real-part b)))

(defn v-make-polar [a b]
  (make-rectangular (v-real-part a) (v-real-part b)))
  
(defn v-magnitude [x] (magnitude x))

(defn v-angle [a]
  (if (zero? a)
    0
    (angle a)))
  
;; Conditionals
(defn v-if [a b c]
  (if (positive? (v-real-part a)) b c))
  
(defn v-clamp [xx aa bb]
  (let [
         x (v-real-part xx)
         a (v-real-part aa)
         b (v-real-part bb)
         ]
    (if (< a b)
      (if (< x a)
        a
        (if (> x b)
          b
          x))
      (if (< x b)
        b
        (if (> x a)
          a
          x)))))
  
;; Linear Interpolation:
;; x in [0, 1] => [x0, x1].
(defn v-lerp [x x0 x1]
  (+ (* x0 (- 1.0 x)) (* x1 x)))
  
;; Inverse of lerp:
;; x in [x0, x1] => [0, 1].
(defn v-lerp-1 [x x0 x1]
  (if (= x1 x0)
    0
    (/ (float (- x x0)) (- x1 x0))))

