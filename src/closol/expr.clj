(ns closol.expr)

(defn enumerate-subexpressions [expr]
  "Enumerates all improper subexpressions, including expr."
  (cond
    (list? expr)
      (if (empty? expr) '()
        (cons expr (apply concat (map enumerate-subexpressions (rest expr)))))
    :else (list expr)))

(defn expression-complexity [expr]
  "Returns a basic complexity metric."
  (cond
    (list? expr)   (reduce + 2 (map expression-complexity (rest expr)))
    (symbol? expr) 1
    :else          0))

(defn constant-expr? [expr]
  "True if expr or its subexpressions are constant."
  (cond
    (number? expr) true
    (list? expr)   (every? constant-expr? (rest expr))
    :else          false))

(defn random-subexpression [expr root]
  "Selects a random subexpression from root expression of the same complexity as expr.
Returns expr if one of similar complexity cannot be found."
  (let [expr-c     (expression-complexity expr)
        valid-exps (filter (fn [e] (= (expression-complexity e) expr-c))
                     (enumerate-subexpressions root))]
    (if (empty? valid-exps) expr (rand-nth valid-exps))))
