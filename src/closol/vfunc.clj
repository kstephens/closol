(ns closol.vfunc
  (:require
    [clojure.math.numeric-tower :as nt]))

;; Complex numbers are undefined in clojure.
(defn make-rectangular [a b] a)
(defn make-polar [a b] a)
(defn real-part [x] x)
(defn magnitude [x] (Math/abs x))
(defn angle [x] 0.0)
(defn positive? [x] (> x 0.0))

(defn isNaN?      [x] (Double/isNaN x))
(defn isInfinite? [x] (Double/isInfinite x))
(defn float-nan?  [x] (and (float? x) (or (Double/isNaN x) (Double/isInfinite x))))

(defn v-size [x]
  (if (vector? x) (count x) 1))
(defn v-get [x i]
  (if (vector? x) (get x i) x))

(defn v-map1 [f x]
  (case (v-size x)
    (1) (f (v-get x 0))
    (2) (vector (f (v-get x 0)) (f (v-get x 1)))
    (3) (vector (f (v-get x 0)) (f (v-get x 1)) (f (v-get x 1)))))

(defn v-map2 [f x y]
  (case (max (v-size x) (v-size y))
    (1) (f (v-get x 0) (v-get y 0))
    (2) (vector (f (v-get x 0) (v-get y 0)) (f (v-get x 1) (v-get y 1)))
    (3) (vector (f (v-get x 0) (v-get y 0)) (f (v-get x 1) (v-get y 1)) (f (v-get x 2) (v-get y 2)))))

(defn v-v1 [x]     (v-get x 0))
(defn v-v2 [x y]   [ (v-get x 0) (v-get y 0) ])
(defn v-v3 [x y z] [ (v-get x 0) (v-get y 0) (v-get z 0) ])
(defn v-i0 [x]     (v-get x 0))
(defn v-i1 [x]     (v-get x 1))
(defn v-i2 [x]     (v-get x 2))
(declare v-real-part)
(defn v-bint [x]    (int (mod (v-real-part x) 256)))

(defn v-real-part [xx]
  (if (vector? xx) (v-real-part (reduce + 0.0 xx))
    (let [x (real-part xx)]
      (if (float-nan? x) 0.0 (float x)))))

(defn v-neg [x]    (v-map1 - x))
(defn v-add [x y]  (v-map2 + x y))
(defn v-sub [x y]  (v-map2 - x y))
(defn v-mul [x y]  (v-map2 * x y))
(defn v-div [x y]  (v-map2 #(if (zero? %2) %1 (/ %1 %2)) x y))
(defn v-expt [x y] (v-map2 #(if (zero? %1) 0.0 (nt/expt %1 %2)) x y))

(defn v-bit-xor [x y]  (v-map2 #(float (bit-xor (v-bint %1) (v-bint %2))) x y))
(defn v-bit-and [x y]  (v-map2 #(float (bit-and (v-bint %1) (v-bint %2))) x y))
(defn v-bit-or  [x y]  (v-map2 #(float (bit-or  (v-bint %1) (v-bint %2))) x y))
(defn v-bit-shift-left  [x y] (v-map2 #(float (bit-shift-left  (v-bint %1) (mod (v-bint %2) 64))) x y))
(defn v-bit-shift-right [x y] (v-map2 #(float (bit-shift-right (v-bint %1) (mod (v-bint %2) 64))) x y))

(defn v-mod [x yy]
  (let [y (v-real-part yy)]
    (if (zero? y) x
      (mod (v-real-part x) y))))
  
(defn v-floor [x]
  (nt/floor (v-real-part x)))

;; Trancendentals
(defn v-sin [x] (Math/sin (v-real-part x)))
(defn v-cos [x] (Math/cos (v-real-part x)))
(defn v-asin [x] (Math/asin (mod (v-real-part x) 1)))
(defn v-acos [x] (Math/acos (mod (v-real-part x) 1)))
(defn v-atan2 [xx yy]
  (let [x (v-real-part xx)
        y (v-real-part yy)]
    (if (and (zero? x) (zero? y)) 0.0
      (Math/atan2 x y))))

(defn v-make-rectangular [a b]
  (make-rectangular (v-real-part a) (v-real-part b)))
(defn v-make-polar [a b]
  (make-rectangular (v-real-part a) (v-real-part b)))
  
(defn v-magnitude [x] (magnitude x))
(defn v-angle [a]
  (if (zero? a) 0.0 (angle a)))
  
;; Conditionals
(defn v-if [a b c]
  (if (positive? (v-real-part a)) b c))
  
(defn v-clamp
  "Clamp x in [a, b] interval."
  [xx aa bb]
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
  
(defn v-lerp
  "Linear Interpolation: x in [0, 1] => [x0, x1]."
  [xx x0 x1]
  (let [ x (v-real-part xx) ]
    (+ (* x0 (- 1.0 x)) (* x1 x))))
  
(defn v-lerp-1
  "Inverse of lerp: x in [x0, x1] => [0, 1]."
  [x x0 x1]
  (let [ d (v-real-part (- x1 x0)) ]
    (if (zero? d) 0.0
      (v-real-part (/ (- x x0) d)))))

