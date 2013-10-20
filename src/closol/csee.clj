(ns closol.csee
  (:require
    [closol.expr :refer :all]
    [clojure.walk :refer :all]))

(defn expand-binding-exprs
"Given ([b1 e1] [b2 e2] [b3 e3] ...)
Returns ([b1 e1] [b2 x2] [b3 x3] ...)
Where
  x2 is the substitution of b1 for e1 in e2,
  x3 is the substitution of b1 for e1 and b2 for e2 in e3,
  ...
"
  [binding-exprs]
  (loop [meb {} be binding-exprs]
    (if (empty? be) (reverse (map reverse meb))
      (let [ e        (first be)
             binding  (first e)
             expr     (second e)
             repl     (walk #(get meb %1 %1) #(get meb %1 %1) expr)
             ]
        (recur
          (assoc meb repl binding)
          (rest be))))))

(defn csee
"Common Sub-Expression Elimination

Assume expr is a simple s-expr (no let, lambda or other special forms)
Rewrite as a let* form with all complex subexpressions as local bindings to new symbols.
"
  [expr]
  (let [
         ;; Enumerate all subexpressions.
         exprs       (reverse (filter #(not (or (symbol? %1) (number? %1))) (enumerate-subexpressions expr)))
         ;; Count the number of times a subexpression is referenced.
         occurances  (frequencies exprs)
         ;; Common Subexpressions: all subexpressions that occur more than once.
         cses        (keys (filter #(> (val %1) 1) occurances))
         ; _ (println (list 'cses= cses))
         ]
    (if (empty? cses)
      expr
      (let [
             ;; Map bindings to CSEs.
             be             (map-indexed #(vector (symbol (str "%g" %1)) %2) cses)
             ; _ (println (list 'be= be))
             ;; Map let* bindings to accumulative CSE values.
             binding-exprs  (expand-binding-exprs be)
             ; _ (println (list 'binding-exprs= binding-exprs))
             ;; Map CSEs to bindings.
             expr-bindings  (apply hash-map (apply concat (map reverse be)))
             ; _ (println (list 'expr-bindings= expr-bindings))
             ;; Replace CSEs in expr with bindings.
             replaced-expr  (walk
                              #(get expr-bindings %1 %1)
                              #(get expr-bindings %1 %1) expr)
             ]
        `(let [~@(apply concat binding-exprs)] ~replaced-expr)))))

