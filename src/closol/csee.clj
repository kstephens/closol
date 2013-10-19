(ns closol.csee
  (:require [closol.expr :refer :all]))

  ;; 
  (defn csee
    "
Common Sub-Expression Elimination
Assume that expr is a simple s-expr (no let, lambda or other special forms)
Rewrite as a let* form with all subexpressions as local bindings to new symbols.
"
    [expr]
    (let [exprs (reverse (enumerate-subexpressions expr))]
      exprs))

