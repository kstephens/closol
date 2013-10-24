(ns closol.expr)

(defn subexpressions
  "Enumerates all improper subexpressions, including expr."
  [expr]
  (cond
    (seq? expr)  (if (empty? expr) '()
                   (cons expr
                     (mapcat subexpressions (rest expr))))
    :else        (list expr)))

(defn expression-complexity
  "A basic complexity metric."
  [expr]
  (cond
    (seq? expr)     (reduce + 2 (map expression-complexity (rest expr)))
    (symbol? expr)  1
    :else           0))

(defn expression-depth
  "The max depth of the expression tree."
  [expr]
  (cond
    (seq? expr)     (+ 1 (apply max (map expression-depth expr)))
    :else           1))

(defn constant-expr?
  "Is expression constant?"
  [expr]
  (cond
    (number? expr) true
    (seq? expr)    (every? constant-expr? (rest expr))
    :else          false))

(defn constant-fold
  "Folds constant expressions within."
  [expr]
  (cond
    (seq? expr)  (if (constant-expr? expr)
                   (eval expr)
                   (map constant-fold expr))
    :else        expr))

