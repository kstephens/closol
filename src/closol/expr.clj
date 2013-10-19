(ns closol.expr)

(defn enumerate-subexpressions [expr]
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

