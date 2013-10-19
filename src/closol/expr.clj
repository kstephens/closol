(ns closol.expr)

(defn enumerate-subexpressions [expr]
  (cond
    (list? expr)
      (if (empty? expr) '()
        (cons expr (apply concat (map enumerate-subexpressions expr))))
    :else (list expr)))

(defn expression-complexity [expr]
  (cond
    (list? expr) (reduce + 2 (map expression-complexity expr))
    (symbol? expr) 1
    :else 0
    ))

