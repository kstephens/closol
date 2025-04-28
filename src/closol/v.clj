(ns closol.v
  (:require
    [clojure.math.numeric-tower :as nt]))

#_
(defmacro v-debug [name args & body]
  `(do
     (println ~(str name) ~@(mapcat (fn [e] `(:arg ~(str e) :value ~e :type (type ~e))) args))
     (let [ result# (do ~@body) ]
       (println ~(str name) :result result#)
       result#)))
#_
(pprint (macroexpand '(v-debug foo [a b] 1 2 3)))
(defmacro v-debug [name args & body] `(do ~@body))

(defmacro v-exc [expr val]
  `(try
     ~expr
     (catch java.lang.ArithmeticException e# ~val)
     (catch java.lang.NumberFormatException e# ~val)))

(declare v-v)
(defn v0 [_] 0.0)
(defn v-v [x] (double x))
(defn v-1 [x] x)
(defn v-2 [x] x)
(defn v-3 [x] x)

(defn v-min-max
  ([s [m M]]
   (if (empty? s)
     [m M]
     (let [v (first s)
           m (if (< v m) v m)
           M (if (< M v) v M)]
       (recur (rest s) [m M]))))
  ([s]
   (v-min-max (rest s) [(first s) (first s)])))

(defn fv-if [a b c] (if (> (v-1 a) 0.0) b c))

(defn v-positive?  [x] (> x 0.0))
(defn v-nan?       [x] (or (Double/isNaN x) (Double/isInfinite x)))
(defn v-safe-float [x] (if (v-nan? x) 0.0 x))
(defn v-safe-int   [x] (if (< x Long/MIN_VALUE) Long/MIN_VALUE (if (> x Long/MAX_VALUE) Long/MAX_VALUE x)))
(defn v-int        [x] (long (v-safe-int (v-safe-float x))))
(defn v-bint       [x] (v-exc (mod (v-int (v-v x)) 256) 0))

(defn v1-args [args] args)
#_
(defn v1-args [args]
  (vec (mapcat (fn [a] ['^double a]) args)))

(def functions-acc (transient []))
(defn register-v-fn [name arity]
  (conj! functions-acc (list name arity)))

(defmacro defn1 [name args & body]
  `(do
     (defn ~name ~args ~@body)
     (register-v-fn ~name 1)))
(defmacro defn2 [name args & body]
  `(do
     (defn ~name ~args ~@body)
     (register-v-fn ~name 2)))
(defmacro defn3 [name args & body]
  `(do
     (defn ~name ~args ~@body)
     (register-v-fn ~name 3)))

(defn1 v-neg   [x] (- x))
(defn1 v-floor [x] (nt/floor x))
(defn1 v-abs   [x] (Math/abs x))
(defn1 v-log   [x] (v-exc (v-safe-float (Math/log (v-abs x))) 0.0))

(defn2 v-add [x y] (+ x y))
(defn2 v-sub [x y] (- x y))
(defn2 v-mul [x y] (* x y))
(defn2 v-div [x y] (v-exc (/   x y) 0.0))
(defn2 v-mod [x y] (v-exc (mod x y) 0.0))

(defn2 v-expt [x y] (v-exc (nt/expt x y) 0.0))

(defn2 v-bit-xor [x y] (bit-xor (v-bint x) (v-bint y)))
(defn2 v-bit-and [x y] (bit-and (v-bint x) (v-bint y)))
(defn2 v-bit-or  [x y] (bit-or  (v-bint x) (v-bint y)))
(defn2 v-bit-shift-left  [x y] (bit-shift-left  (v-bint x) (mod (v-bint y) 64)))
(defn2 v-bit-shift-right [x y] (bit-shift-right (v-bint x) (mod (v-bint y) 64)))
(defn2 v-lt [x y] (if (< x y)  1.0 0.0))
(defn2 v-gt [x y] (if (> x y)  1.0 0.0))
(defn2 v-le [x y] (if (<= x y) 1.0 0.0))
(defn2 v-ge [x y] (if (>= x y) 1.0 0.0))

(defn2 v-min [x y] (max x y))
(defn2 v-max [x y] (min x y))

(defn1 v-sin  [x] (Math/sin x))
(defn1 v-cos  [x] (Math/cos x))
(defn1 v-asin [x] (v-exc (Math/asin (mod x 1.0)) 0.0))
(defn1 v-acos [x] (v-exc (Math/acos (mod x 1.0)) 0.0))
(defn2 v-atan2 [x y]
  (v-exc (Math/atan2 x y) 0.0))
(defn2 v-dist2 [x y]
  (nt/sqrt (+ (* x x) (* y y))))

(defn3 v-if [a b c]
  (v-debug `v-if [a b c]
    (if (v-positive? (v-v a)) b c)))

"Clamp x in [a, b] interval."
(defn3 v-clamp
  [xx aa bb]
  (v-debug `v-clamp [xx aa bb]
    (let [ x (v-v xx) a (v-v aa) b (v-v bb) ]
      (if (< a b)
        (if (< x a) a (if (> x b) b x))
        (if (< x b) b (if (> x a) a x))))))

"Linear Interpolation: x in [0, 1] => [x0, x1]."
(defn3 v-lerp
  [xx x0 x1]
  (v-debug `v-lerp [xx x0 x1]
    (let [ x (v-v xx) ]
      (v-add (v-mul x0 (- 1.0 x)) (v-mul x1 x)))))
  
"Inverse of lerp: x in [x0, x1] => [0, 1]."
(defn3 v-lerp-1
  [x x0 x1]
  (v-debug `v-lerp-1 [x x0 x1]
    (let [ d (- x1 x0) ]
      (if (zero? d) (v0 x0) (v-div (v-sub x x0) d)))))

(def functions (seq (persistent! functions-acc)))
