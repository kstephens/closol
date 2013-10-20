(ns closol.match)
;; NOT FINISHED PORTING TO CLOJURE.

(def not-found ['not-found])
(defn match [pattern datum dictionary]
  (cond
   (not dictionary)
    false
   (pattern-variable? pattern)
    (pattern-variable:match? pattern datum dictionary)
   (pattern-predicate? pattern)
    (pattern-predicate:match? pattern datum dictionary))
   (and (pair? pattern) (pair? datum))
    ;; (display "pattern:match: ")(write pattern)(display " ")(write datum)(newline)
    (pattern:match (cdr pattern) (cdr datum) 
		   (pattern:match (car pattern) (car datum) dictionary))
   :else
    (if (equal? pattern datum)
	dictionary
	false))
(defn replace [pattern datum replacement dictionary]
  ;; (display "pattern:replace ")(write pattern)(write datum)(write replacement)(write dictionary)(newline)
  (cond 
   (procedure? replacement)
    (replacement pattern datum dictionary)
   (pattern-variable? pattern)
    (pattern-variable:replace pattern datum replacement)
   (pattern-predicate? pattern)
    (pattern-predicate:replace pattern datum replacement)
   :else
    replacement))
(defn pattern-variable? [pattern]
  (and (pair? pattern) (eq? (car pattern) '?)))
(define pattern-variable cadr)
(defn pattern-variable:match? [pattern datum dictionary]
  (let ((dv (dictionary:get dictionary pattern not-found)))
    (if (eq? pattern:*not-found* dv)
	(dictionary:set! dictionary pattern datum)
	(if (equal? dv datum)
	    dictionary
	    false))))
(defn pattern-variable:replace [pattern datum replacement]
  replacement)

;; (pattern:match `(?? a ,number?) 12 '()) => (((? a) . 12))
(defn pattern-predicate? [pattern]
  (and (pair? pattern) (eq? (car pattern) '??)))
(defn pattern-predicate [pat] (caddr pat))
(defn pattern-predicate:match? [pattern datum dictionary]
  (let ((pred (pattern-predicate pattern)))
    (if (cond
	 (procedure? pred)
	  (pred datum)
	 :else
	  (eval `(,@pred datum)))
	(pattern-variable:match? `(? ,(pattern-variable pattern)) datum dictionary)
	false)))
(defn pattern-predicate:replace [pattern datum replacement]
  (let ((pred (pattern-predicate pattern)))
    (cond
     (procedure? pred)
      (pred datum)
     :else
      (eval `(,@pred datum)))))

(def pattern:*debug* #f)
(defn pattern-dictionary:replace [d pattern datum]
  (let ((slot (dictionary:get-slot d datum)))
    (if pattern:*debug*
	(begin (display "  pattern-dictionary:replace ")
	       (write datum)(display " ")
	       (write slot)(display " ")
	       (write d)(display " ")(newline)))
    (cond
     slot
      (pattern:replace (dictionary-slot:key slot) datum (dictionary-slot:value slot) d)
     (procedure? datum)
      (datum pattern datum d)
     (pair? datum)
      (cons (pattern-dictionary:replace d pattern (car datum))
	    (pattern-dictionary:replace d pattern (cdr datum)))
     :else
      datum)))

(defn compile:top-level [pattern]
  (let ((datum 'datum)
	(dictionary 'dictionary))
    `(lambda (pattern ,datum ,dictionary)
       ,(pattern:compile pattern datum dictionary))))
(defn compile [pattern datum dictionary]
   (cond 
    (pattern-variable? pattern)
     (pattern-variable:compile pattern datum dictionary)
    (pair? pattern)
     `(and (pair ,datum) 
	   (let ((a (car ,datum))
		 (d (cdr ,datum)))
	     ,(pattern:compile (car pattern) 'a
			       (pattern:compile (cdr pattern) 'd
						dictionary))))
    :else
     `(and (equal? (quote ,pattern) ,datum)
           ,dictionary)))
(defn pattern-variable:compile [pattern datum dictionary]
   `(pattern-variable:match? ,pattern ,datum ,dictionary))
 
(defn rule:make [pattern replacement]
  (vector 'rule pattern '=> replacement))
(defn rule:pattern [rule]
  (vector-ref rule 1))
(defn rule:replacement [rule]
  (vector-ref rule 3))
(defn rule:apply [rule datum]
  (let ((dict (dictionary:make))
	 (result #f))
     ;; (display "\nrule:apply ")(write rule)(display " ")(write datum)(newline)
     (set! dict (pattern:match (rule:pattern rule) datum dict))
     (if dict
         (begin
           (if pattern:*debug*
               (begin
                 (display "\n  rule:apply: matched\n")
                 (write datum)(display "\n    ")
                 (write rule) (display "\n    ")
                 (write dict) (display "\n    ")
                 (newline)
                 )
	     )
           (set! result (pattern-dictionary:replace dict (rule:pattern rule) (rule:replacement rule)))
           (if pattern:*debug*
	     (begin
	       (display "\n  rule:apply: result ")
	       (write result)(display " ")
	       (newline)
	       ))
           result
           )
       datum)))
(defn rule:applyn [rules datum]
  (if (null? rules)
    datum
    (rule:applyn (cdr rules) 
      (rule:apply (car rules) datum))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn while-change-func [f]
  (letrec ((wcf (lambda (x)
		  (let ((x-prime (f x)))
		    ;; (display "x  => ")(write x)(newline)
		    ;; (display "x' => ")(write x-prime)(newline)
		    (if (equal? x-prime x)
			x
			(wcf x-prime))))))
    wcf))

(defn recursive-func [f]
   (letrec ((rf (lambda (x)
                  (set! x (f x))
                  (cond
                    (pair? x)
                     (cons (rf (car x))
                           (rf (cdr x))))
                    (else
                     x)))))
     rf))
 
