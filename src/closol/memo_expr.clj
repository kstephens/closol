(ns closol.memo-expr
  (:require
   [clojure.set :as set]
   [clojure.walk :refer :all]))

(def ^:const parameter?    symbol?)
(def ^:const application?  seq?)
(def ^:const function      first)
(def ^:const arguments     rest)
(def ^:const constant?     number?)

(defn append!
  [trans col]
  (doseq [e col]
    (conj! trans e))
  trans)

(declare parameters)

(defn parameters-of
  [es]
  (apply set/union (map parameters es)))

(defn parameters
  [e]
  (cond
    (parameter?   e)  #{e}
    (application? e)  (parameters-of (arguments e))
    :else             #{}))

(defn parameterized?
  [e]
  (and (application? e) (not (empty? (parameters e)))))

(defn subexpressions
  [e]
  (if (application? e) (arguments e) []))

(defn memoize-subexpressions?
  [e]
  (and (not (parameter? e))
       (> (count (parameters e))
          (apply max 0 (map #(count (parameters %)) (subexpressions e))))))

(def generate-variable gensym)

(defn memoized-subexpression
  [se]
  (let [params (sort (parameters se))]
    (if (and (application? se) (not (empty? params)))
      (let [f        (generate-variable)
            func     `(memoize (fn ~(vec params) ~se))
            expr     (cons f params)]
        (list expr f func))
      (list se false false))))

(declare memoize-traverse)

(defn memoize-application
  [e fns]
  (let [e (cons (function e) (map #(memoize-traverse % fns) (arguments e)))]
    (if (memoize-subexpressions? e)
      (let [memos  (map memoized-subexpression (subexpressions e))]
        (append! fns (filter second memos))
        (cons (function e) (map first memos)))
      e)))

(defn memoize-traverse
  [e fns]
  (if (application? e)
    (memoize-application e fns)
    e))

(defn memoize-expression
  [params tle]
  (let [fns    (transient [])
        tle2   (memoize-traverse tle fns)
        fns    (persistent! fns)]
    `(let ~(vec (mapcat rest fns))
       (fn ~(vec params) ~tle2))))

