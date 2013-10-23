(ns closol.vf2
  (:require
    [clojure.math.numeric-tower :as nt]))

(declare v-v)
(defn positive?  [x] (> x 0))
(defn float-nan? [x] (and (float? x) (or (Double/isNaN x) (Double/isInfinite x))))
(defn v-bint     [x] (int (mod (v-v x) 256)))

;; Types
(def V1 java.lang.Double)
(defrecord V2 [x y])
(defrecord V3 [x y z])

;; Destructors
(defmulti v-1 type)
(defmulti v-2 type)
(defmulti v-3 type)

(defmethod v-1 V1 [v] v)
(defmethod v-2 V1 [v] v)
(defmethod v-3 V1 [v] v)

(defmethod v-1 V2 [v] (.x v))
(defmethod v-2 V2 [v] (.y v))
(defmethod v-3 V2 [v] (.y v))

(defmethod v-1 V3 [v] (.x v))
(defmethod v-2 V3 [v] (.y v))
(defmethod v-3 V3 [v] (.z v))

;; Constructors
(defn v1 [x]     (double (v-1 x)))
(defn v2 [x y]   (V2. (v-1 x) (v-1 y)))
(defn v3 [x y z] (V3. (v-1 x) (v-1 y) (v-1 z)))

(defmulti v0 type)
(def v1-0 0.0)
(defmethod v0 V1 [_] v1-0)
(def v2-0 (v2 0.0 0.0))
(defmethod v0 V2 [_] v2-0)
(def v3-0 (v3 0.0 0.0 0.0))
(defmethod v0 V3 [_] v3-0)

(defmacro defn1 [name args & body]
  `(do
     (defmulti  ~name (fn ~args ~(vec (map #(list `type %1) args))))
     (defmethod ~name ^double [^double V1] ~args
       (v1 (do ~@body)))
     (defmethod ~name [V2] ~args
       (V2.
         (v1
           (let [ ~(args 0) (.x ~(args 0)) ]
             ~@body))
         (v1
           (let [ ~(args 0) (.y ~(args 0)) ]
             ~@body))))
     ))

(defmacro defn2 [name args & body]
  `(do
     (defmulti  ~name (fn ~args ~(vec (map #(list `type %1) args))))
     (defmethod ~name ^double [^double V1 ^double V1] ~args
       (v1 (do ~@body)))
     (defmethod ~name [V1 V2] ~args
       (V2.
         (v1
           (let [
                  ~(args 1) (.x ~(args 1)) ]
             ~@body))
         (v1
           (let [ 
                  ~(args 1) (.y ~(args 1)) ]
             ~@body))))
     (defmethod ~name [V2 ^double V1] ~args
       (V2.
         (v1
           (let [ ~(args 0) (.x ~(args 0))
                  ]
             ~@body))
         (v1
           (let [ ~(args 0) (.y ~(args 0))
                  ]
             ~@body))))
     (defmethod ~name [V2 V2] ~args
       (V2.
         (v1
           (let [ ~(args 0) (.x ~(args 0))
                  ~(args 1) (.x ~(args 1)) ]
             ~@body))
         (v1
           (let [ ~(args 0) (.y ~(args 0))
                  ~(args 1) (.y ~(args 1)) ]
             ~@body))))
     ))

(defmulti v-v type)
(defmethod v-v V1 [v] v)
(defmethod v-v V2 [v] (+ (.x v) (.y v)))
(defmethod v-v V3 [v] (+ (.x v) (.y v) (.z v)))

(defn1 v-neg   [x] (- x))
(defn1 v-floor [x] (nt/floor x))

(defn2 v-add [x y] (+ x y))
(defn2 v-sub [x y] (- x y))
(defn2 v-mul [x y] (* x y))
(defn2 v-div [x y] (if (zero? y) 0.0 (/ x y)))
(defn2 v-mod [x y] (if (zero? y) 0.0 (mod x y)))

(defn2 v-expt [x y] (if (zero? x) 0.0 (nt/expt x y)))

(defn2 v-bit-xor [x y] (bit-xor (v-bint x) (v-bint y)))
(defn2 v-bit-and [x y] (bit-and (v-bint x) (v-bint y)))
(defn2 v-bit-or  [x y] (bit-or  (v-bint x) (v-bint y)))
(defn2 v-bit-shift-left  [x y] (bit-shift-left  (v-bint x) (mod (v-bint y) 64)))
(defn2 v-bit-shift-right [x y] (bit-shift-right (v-bint x) (mod (v-bint y) 64)))

(defn1 v-sin  [x] (Math/sin x))
(defn1 v-cos  [x] (Math/cos x))
(defn1 v-asin [x] (Math/asin (mod x 1)))
(defn1 v-acos [x] (Math/acos (mod x 1)))
(defn2 v-atan2 [x y]
  (if (and (zero? x) (zero? y)) (v0 x)
    (Math/atan2 x y)))
(defn2 v-dist2 [x y]
  (nt/sqrt (+ (* x x) (* y y))))

(defn v-if [a b c]
  (if (positive? (v-v a)) b c))

(defn v-clamp
  "Clamp x in [a, b] interval."
  [x a b]
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
        x))))

(defn v-lerp
  "Linear Interpolation: x in [0, 1] => [x0, x1]."
  [xx x0 x1]
  (let [ x (v-v xx) ]
    (v-add (v-mul x0 (- 1.0 x)) (v-mul x1 x))))
  
(defn v-lerp-1
  "Inverse of lerp: x in [x0, x1] => [0, 1]."
  [x x0 x1]
  (let [ d (- x1 x0) ]
    (if (zero? d) (v0 x0) (v-div (v-sub x x0) d))))
