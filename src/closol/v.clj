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
(defn v-positive?  [x] (> x 0.0))
(defn v-nan?       [x] (or (Double/isNaN x) (Double/isInfinite x)))
(defn v-safe-float [x] (if (v-nan? x) 0.0 x))
(defn v-safe-int   [x] (if (< x Long/MIN_VALUE) Long/MIN_VALUE (if (> x Long/MAX_VALUE) Long/MAX_VALUE x)))
(defn v-int        [x] (long (v-safe-int (v-safe-float x))))
(defn v-bint       [x] (v-exc (mod (v-int (v-v x)) 256) 0))

;; Types
(def       V1 java.lang.Double)
(defrecord V2 [x y])
(defrecord V3 [x y z])

;; Deconstructors
(defmulti v-1 type)
(defmulti v-2 type)
(defmulti v-3 type)

(defmethod v-1 :default [v] (double v))
(defmethod v-2 :default [v] (double v))
(defmethod v-3 :default [v] (double v))

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
(def                 v1-0 0.0)
(defmethod v0 V1 [_] v1-0)
(def                 v2-0 (v2 0.0 0.0))
(defmethod v0 V2 [_] v2-0)
(def                 v3-0 (v3 0.0 0.0 0.0))
(defmethod v0 V3 [_] v3-0)

(if true
(do
(defn defmulti-expr [name args]
  `(defmulti ~name (fn ~args ~(vec (map #(list `type %1) args)))))

(defn v1-args [args] args)
#_
(defn v1-args [args]
  (vec (mapcat (fn [a] ['^double a]) args)))
(defmacro defn1 [name args & body]
  `(do
     ;;~(defmulti-expr name args)
     (defmulti  ~name (fn ~args ~(vec (map #(list `type %1) args))))
     (def ~(symbol (str "f" name)) ~name)
     (defmethod ~name ^double [V1] ~(v1-args args)
       (v-debug ~name ~args
         (v1 (do ~@body))))
     (defmethod ~name [V2] ~args
       (v-debug ~name ~args
       (V2.
         (v1
           (let [ ~(args 0) (.x ~(args 0)) ]
             ~@body))
         (v1
           (let [ ~(args 0) (.y ~(args 0)) ]
             ~@body)))))
     (defmethod ~name [V3] ~args
       (v-debug ~name ~args
       (V3.
         (v1
           (let [ ~(args 0) (.x ~(args 0)) ]
             ~@body))
         (v1
           (let [ ~(args 0) (.y ~(args 0)) ]
             ~@body))
         (v1
           (let [ ~(args 0) (.z ~(args 0)) ]
             ~@body)))))
     ))
(defmacro defn2 [name args & body]
  `(do
     ;; ~(defmulti-expr name args)
     (defmulti  ~name (fn ~args ~(vec (map #(list `type %1) args))))
     (def ~(symbol (str "f" name)) ~name)
     (defmethod ~name ^double [V1 V1] ~(v1-args args)
       (v-debug ~name ~args
       (v1 (do ~@body))))
     (defmethod ~name [V1 V2] ~args
       (v-debug ~name ~args
       (V2.
         (v1
           (let [
                  ~(args 1) (.x ~(args 1)) ]
             ~@body))
         (v1
           (let [ 
                  ~(args 1) (.y ~(args 1)) ]
             ~@body)))))
     (defmethod ~name [V2 V1] ~args
       (v-debug ~name ~args
       (V2.
         (v1
           (let [ ~(args 0) (.x ~(args 0))
                  ]
             ~@body))
         (v1
           (let [ ~(args 0) (.y ~(args 0))
                  ]
             ~@body)))))
     (defmethod ~name [V2 V2] ~args
       (v-debug ~name ~args
       (V2.
         (v1
           (let [ ~(args 0) (.x ~(args 0))
                  ~(args 1) (.x ~(args 1)) ]
             ~@body))
         (v1
           (let [ ~(args 0) (.y ~(args 0))
                  ~(args 1) (.y ~(args 1)) ]
             ~@body)))))
     ))
(defmacro defn3 [name args & body]
  `(do
     ;; ~(defmulti-expr name args)
     (defmulti  ~name (fn ~args ~(vec (map #(list `type %1) args))))
     (def ~(symbol (str "f" name)) ~name)
     (defmethod ~name ^double [V1 V1 V1] ~(v1-args args)
       (v-debug ~name ~args
         (v1 (do ~@body))))))
)
;; Macro only version
(do
  (defn make-macro [name args body]
    (let [ m-args   (vec (map gensym args))
           let-args (mapcat (fn [a b] `('~a ~b)) args m-args) ]
      `(do
         (defn ~(symbol (str "f" name)) ~args ~@body)
         (defmacro ~name ~m-args
           (list 'let (vector ~@let-args)
             (cons 'do '~body))))))

  (defmacro defn1 [name args & body] (make-macro name args body))
  (defmacro defn2 [name args & body] (make-macro name args body))
  (defmacro defn3 [name args & body] (make-macro name args body))
  ))

(defmulti  v-v type)
(defmethod v-v V1 [v] v)
(defmethod v-v V2 [v] (+ (.x v) (.y v)))
(defmethod v-v V3 [v] (+ (.x v) (.y v) (.z v)))
(defmethod v-v :default [v] (double v))

(defn1 v-neg   [x] (- x))
(defn1 v-floor [x] (nt/floor x))
(defn1 v-abs   [x] (Math/abs x))

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

(defmethod fv-min [Long Long] [x y] (min x y)) ;; hack for matrix-min-max
(defmethod fv-max [Long Long] [x y] (max x y)) ;; hack for matrix-min-max

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

(def functions
  (sort-by first (partition 2
    `(
       v-neg 1
       v-cos 1
       v-sin 1
       v-acos 1
       v-asin 1
       v-floor 1
       v-abs 1

       v-add 2
       v-sub 2
       v-mul 2
       v-div 2
       v-mod 2
       v-expt 2
       v-atan2 2
       v-dist2 2
       v-bit-xor 2
       v-bit-and 2
       v-bit-or  2
       v-bit-shift-left  2
       v-bit-shift-right 2
       v-lt 2
       v-gt 2
       v-le 2
       v-ge 2
       v-min 2
       v-max 2

       v-if 3
       v-clamp 3
       v-lerp 3
       v-lerp-1 3
     ))))
