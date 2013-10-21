(ns closol.expr
  (:require
    [clojure.walk :as w]))

(defn subexpressions
  "Enumerates all improper subexpressions, including expr."
  [expr]
  (cond
    (list? expr)
      (if (empty? expr) '()
        (cons expr (apply concat (map subexpressions (rest expr)))))
    :else (list expr)))

(defn expression-complexity
  "A basic complexity metric."
  [expr]
  (cond
    (list? expr)   (reduce + 2 (map expression-complexity (rest expr)))
    (symbol? expr) 1
    :else          0))

(defn constant-expr?
  "Is expression constant?"
  [expr]
  (cond
    (number? expr) true
    (list? expr)   (every? constant-expr? (rest expr))
    :else          false))

(defn constant-fold
  "Folds constant expressions within."
  [expr]
  (cond
    (list? expr)
    (if (constant-expr? expr) (eval expr)
      (cons (first expr) (map constant-fold (rest expr))))
    :else expr))

(defn random-subexpression-of-complexity
  "Selects a random subexpression from root expression of the same complexity as expr.
Returns expr if one of similar complexity cannot be found."
  [expr root]
  (let [ expr-c     (expression-complexity expr)
         valid-exps (filter #(= (expression-complexity %1) expr-c)
                     (subexpressions root))]
    (if (empty? valid-exps) expr (rand-nth valid-exps))))
