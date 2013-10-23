(ns closol.match)

(def ^:dynamic pattern:*debug* false)
(def ^:dynamic pattern-replace:*debug* false)

(defn equal?
  "Recursive type-conforming equality."
  [x y]
  (if pattern:*debug* (println "      (equal?" x y ")"))
  (and (= (type x) (type y))
    (cond
      ;; (coll? x)    (and (empty? x) (empty? y))
      (vector? x)  (or (and (empty? x) (empty? y))
                     (and (equal? (first x) (first y)) (equal? (vec (rest x)) (vec (rest y)))))
      (map? x)     (every? #(and (contains? x %1) (equal? %1 (x %1))) (set (concat (keys x) (keys y))))
      (set? x)     (every? #(contains? x %1) (concat x y))
      (seq? x)     (or (and (empty? x) (empty? y))
                     (and (equal? (first x) (first y)) (equal? (rest x) (rest y))))
      :else        (= x y)
      )))

(defn procedure? [x]
  (instance? clojure.lang.IFn x))

(defn make-like [x y]
  (cond
    (vector? y) (vec x)
    :else       x))

(def not-found [:not-found])

(declare
  pattern-variable? pattern-variable:match? pattern-variable:replace
  pattern-predicate? pattern-predicate:match? pattern-predicate:replace
  pattern-rest? pattern-rest:pattern)

(defn proper-list?
  [x]
  (and
    (or (list? x)
      (instance? clojure.lang.Cons x)
      (instance? clojure.lang.LazySeq x))
    (not (empty? x))))

(defn proper-vector?
  [x]
  (and (vector? x) (not (empty? x))))

(defn match [pattern datum dictionary]
  (if pattern:*debug* (println "  (match" pattern ":" (type pattern) datum ":" (type datum) dictionary ")"))
  (cond
    (not dictionary)               false
    (pattern-rest? pattern)        (match (pattern-rest:pattern pattern) datum dictionary)
    (pattern-variable? pattern)    (pattern-variable:match? pattern datum dictionary)
    (pattern-predicate? pattern)   (pattern-predicate:match? pattern datum dictionary)
    (and (proper-vector? pattern)
      (proper-vector? datum))      (match (vec (rest pattern)) (vec (rest datum))
                                     (match (first pattern) (first datum) dictionary))
    (and (proper-list? pattern)
      (proper-list? datum))        (match (rest pattern) (rest datum)
                                     (match (first pattern) (first datum) dictionary))
    :else                          (and (equal? pattern datum) dictionary)))

(def rest-patterns {:&? :? :&?? :??})
(defn pattern-rest? [pattern]
  (and (coll? pattern)
    (coll? (first pattern))
    (rest-patterns (first (first pattern)))))

(defn pattern-rest:pattern [pattern]
  (let [ inner (first pattern) ]
    (make-like 
      (cons (rest-patterns (first inner)) (rest inner))
      inner)))

(defn pattern-variable? [pattern]
  (and (coll? pattern) (= :? (first pattern))))

(defn pattern-variable [pattern] (second pattern))

(defn pattern-variable:match? [pattern datum dictionary]
  (if pattern:*debug* (println "    (pattern-variable:match?" pattern datum dictionary ")"))
  (let [ dv (dictionary pattern not-found) ]
    (if (identical? not-found dv)
	(assoc dictionary pattern datum)
	(and (equal? dv datum) dictionary))))

;; (match `(:?? a ~number?) 12 {}) => {(:? a) 12}
(defn pattern-predicate? [pattern]
  (and (coll? pattern) (= :?? (first pattern))))

(defn pattern-predicate [pattern] (nth pattern 2))

(defn pattern-predicate:apply [pattern datum]
  (let [ pred (pattern-predicate pattern) ]
    (cond
      (procedure? pred)  (pred datum)
      :else              (eval `(~@pred datum)))))

(defn pattern-predicate:match? [pattern datum dictionary]
  (if pattern:*debug* (println "    (pattern-predicate:match?" pattern datum dictionary ")"))
  (and (pattern-predicate:apply pattern datum)
    (pattern-variable:match? `(:? ~(pattern-variable pattern)) datum dictionary)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replace
;;

(defn pattern-replace [pattern datum replacement dictionary]
  (if pattern-replace:*debug* (println "    (pattern-replace" pattern datum replacement dictionary ")"))
  (cond 
    ;; (procedure? replacement)      (replacement pattern datum dictionary)
    (pattern-rest? pattern)       (pattern-replace (pattern-rest:pattern pattern) datum dictionary)
    (pattern-variable? pattern)   (pattern-variable:replace pattern datum replacement)
    (pattern-predicate? pattern)  (pattern-predicate:replace pattern datum replacement)
    :else                         replacement))

(defn pattern-variable:replace [pattern datum replacement]
  replacement)

(defn pattern-predicate:replace [pattern datum replacement]
  (let [ pred (pattern-predicate pattern) ]
    (cond
      (procedure? pred)  (pred datum)
      :else              (eval `(~@pred datum))))) ;; ugly eval

(defn pattern-dictionary:replace [d pattern datum-or-rest]
  (let [ datum            (if (pattern-rest? datum-or-rest)
                            (pattern-rest:pattern datum-or-rest)
                            datum-or-rest)
         has-replacement  (contains? d datum)
         replacement      (d datum) ]
    (if pattern-replace:*debug* (println "  pattern-dictionary:replace " datum replacement d))
    (cond
      has-replacement        (pattern-replace pattern datum replacement d)
      (proper-list? datum)   (cons
                               (pattern-dictionary:replace d pattern (first datum))
                               (pattern-dictionary:replace d pattern (rest datum)))
      (proper-vector? datum) (vec
                               (cons
                                 (pattern-dictionary:replace d pattern (first datum))
                                 (pattern-dictionary:replace d pattern (vec (rest datum)))))
      ;; (procedure? datum)     (datum pattern datum d)
      :else                  datum)))

(declare pattern:compile-1 pattern-variable:compile)
(defn compile:top-level [pattern]
  (let [ datum 'datum
	 dictionary 'dictionary ]
    `(fn [ pattern ~datum ~dictionary ]
       ~(pattern:compile-1 pattern datum dictionary))))
(defn pattern:compile-1 [pattern datum dictionary]
   (cond 
     (pattern-variable? pattern)
     (pattern-variable:compile pattern datum dictionary)
     (list? pattern)
     `(and (list? ~datum) 
	   (let ((a (first ~datum))
		 (d (rest ~datum)))
	     ~(pattern:compile-1 (first pattern) 'a
			       (pattern:compile-1 (rest pattern) 'd
						dictionary))))
     :else
     `(and (equal? (quote ~pattern) ~datum)
           ~dictionary)))

(defn pattern-variable:compile [pattern datum dictionary]
   `(pattern-variable:match? ~pattern ~datum ~dictionary))
 
(defn rule:make
  ([x] (rule:make (first x) (nth x 2)))
  ([pattern replacement]
    [:rule pattern :=> replacement]))
(defn rule:pattern     [rule] (nth rule 1))
(defn rule:replacement [rule] (nth rule 3))

(defn rule:apply [rule datum]
  (let [ dict (match (rule:pattern rule) datum { }) ]
     (if dict
       ;;(begin
       ;;  (if pattern:*debug* (println "\n  rule:apply: matched\n" datum rule dict))
       (let [ result (pattern-dictionary:replace dict (rule:pattern rule) (rule:replacement rule)) ]
         ;;    (if pattern:*debug*) (println "\n  rule:apply: result " result))
         result)
       ;;)
       datum)))

(defn rule:applyn [rules datum]
  (reduce #(rule:apply %2 %1) datum rules))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn fixed-point
  "Create a fixed-point function g where (equality x (f x)) becomes true."
  [equality f]
  (fn [x]
    (loop [ x x ]
      (let [ x-prime (f x) ]
        ;; (println "x  => " x)
        ;; (println "x' => " x-prime)
        (if (equality x-prime x) x
          (recur x-prime))))))

(defn recursive
  "Create a recursively applied function."
  [f]
  (fn rf [x]
    (let [ x2 (f x) ]
      (cond
        (vector? x2) (vec (map rf x2))
        (seq? x2)    (map rf x2)
        :else        x2))))
 
