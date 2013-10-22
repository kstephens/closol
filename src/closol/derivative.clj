(ns closol.derivative
  (:require
    [closol.match :refer :all]))

(defn negative? [x] (< x 0))
(defn not-number? [x]
  (not (number? x)))
(defn negative-number? [x]
  (and (number? x) (negative? x)))

;; Expand Lisp expression into simple unary and binary expressions.
(def expansion-rules
  (map rule:make
    (let [ a [:? :a]
           b [:? :b]
           c [:? :c]
           rest [:&? :rest] ]
      (partition 3
        `(
           (+)                => 0
           (+ ~a)             => ~a
           (+ ~a ~b ~c ~rest) => (+ ~a (+ ~b (+ ~c ~rest)))
	 
           (- ~a ~b ~c ~rest) => (- ~a (+ ~b ~c ~rest))

           (*)                => 1
           (* ~a)             => ~a
           (* ~a ~b ~c ~rest) => (* ~a (* ~b (* ~c ~rest)))
	 
           (/ ~a ~b ~c ~rest) => (/ ~a (* ~b ~c ~rest))
	 )))))

;; Simplify simple unary and binary expressions.
(def simplification-rules
  (map rule:make
    (let [ a [:? :a] ]
      `(
         ~@(map (fn [uop]
                  (let [ sym (first uop)
                         op (second uop) ]
                    `((~sym [:?? :a ~number?]) =>
                       ~(fn [p r dict] (op (dict [:? :a]))))))
             `( (- ~-)
                (/ ~/)
                ;(Math/sin ~Math/sin)
                ;(Math/cos ~Math/cos)
                ;(Math/tan ~Math/tan)
                ;(Math/log ~Math/log)
                ))
         ~@(map (fn [bop] 
                  (let [ sym (first bop)
                         op  (second bop) ]
                    `((~sym [:?? :a ~number?] [:?? :b ~number?]) =>
                       ~(fn [p r dict]
                          (op (dict '[:? :a]) (dict '[:? :b]))))))
             `( (- ~-)
                (+ ~+)
                (* ~*)
                (/ ~/)
                ;(expt ~Math/expt)
                ))

         ((+ ~a)     => ~a)     
         ((+ 0 ~a)   => ~a)
         ((+ ~a 0)   => ~a)
	 
         ((- ~a 0)   => ~a)
         ((- 0 ~a)   => (- ~a))
         ((- (- ~a)) => ~a)
	 
         ((* 1 ~a) => (* ~a))
         ((* -1 ~a) => (- ~a))  
         ((* ~a 1) => ~a)
         ((* ~a -1) => (- ~a))
	 
         ((* 0 ~a) => 0)
         ((* ~a 0) => 0)
         
         ((/ 0 ~a) => 0)
         ((/ ~a 1) => ~a)
         ((/ ~a -1) => (- ~a))
         ((/ ~a ~a) => 1)
	 
         ((expt 0 ~a) => 0)
         ((expt ~a 0) => 1)
         ((expt ~a 1) => ~a)
         ((expt ~a -1) => (/ 1 ~a))
	 
         ))))


;; Unify subexpressions and identities into simpler expressions.
(def unification-rules
  (map rule:make
    (let [ 
           a [:? :a]
           b [:? :b]
           c [:? :c]
           x [:? :x]
           y [:? :y]
           a-num [:?? :a number?]
           b-num [:?? :b number?]
           x-nan [:?? :x not-number?]
           ]
      `(
	 ((+ ~x-nan ~a-num) => (+ ~a ~x))
	 ((+ ~x-nan (+ ~a-num ~y)) => (+ ~a (+ ~x ~y)))
	 ((+ ~a-num (+ ~b-num ~x)) => (+ (+ ~a ~b) ~x))
	 ((+ ~x (- ~y)) => (- ~x ~y))
	 ((- ~x (- ~y)) => (+ ~x ~y))
	 (((+ (- ~x) ~y)) => (- ~y ~x))
	 
	 ((* ~x-nan ~a-num) => (* ~a ~x))
	 
	 ((* ~x-nan (* ~a-num ~y)) => (* ~a (* ~x ~y)))
	 
	 ((* ~a-num (* ~b-num ~y)) => (* (* ~a ~b) ~y))
	 ((* ~a (- ~b)) => (* (- ~a) ~b))
	 
	 ;; Distributive:
	 ((+ (* ~a ~b) (* ~a ~c)) => (* ~a (+ ~b ~c)))
	 ((+ (* ~b ~a) (* ~a ~c)) => (* ~a (+ ~b ~c)))
	 ((+ (* ~a ~b) (* ~c ~a)) => (* ~a (+ ~b ~c)))
	 
	 ((- (* ~a ~b) (* ~a ~c)) => (* ~a (- ~b ~c)))
	 ((- (* ~b ~a) (* ~a ~c)) => (* ~a (- ~b ~c)))
	 ((- (* ~a ~b) (* ~c ~a)) => (* ~a (- ~b ~c)))
	 
	 ;; Multiplicative: 
	 ((+ ~a ~a) => (* 2 ~a))
	 ((+ (+ ~a ~b) ~b) => (+ ~a (* 2 ~b)))
	 ((+ ~a (* ~b-num ~a)) => (* (+ ~b 1) ~a))
	 
	 ((* -1 ~a) => (- ~a))
	 ((- (- ~a)) => ~a)
	 ((- ~a ~a) => 0)
	 ((- (* ~a-num ~b)) => (* (- ~a) ~b))
	 
	 ((- (* ~b-num ~a) ~a) => (* (- ~b 1) ~a))
	 
	 ((* ~a-num (- ~b)) => (* (- ~a) ~b))
	 ((* (* ~a-num ~b) (- ~c)) => (* (* (- ~a) ~b) ~c))
	 
	 ((* ~a ~a)        => (expt ~a 2))
	 ((* ~a (* ~a ~b)) => (* (expt ~a 2) ~b))
	 ((* ~a (* ~b ~a)) => (* (expt ~a 2) ~b))
	 ((* (* ~a ~b) ~a) => (* (expt ~a 2) ~b))
	 ((* (* ~b ~a) ~a) => (* (expt ~a 2) ~b))
	 
	 ((* ~a-num (* ~b-num ~c)) => (* (* ~a ~b) ~c))
	 ((* ~x-nan (/ ~b-num ~c)) => (* ~b (/ ~x ~c)))
	 
	 ((/ ~a-num (* ~b-num ~c)) => (* (/ ~a ~b) ~c))
	 ((* ~a-num (/ ~b ~c))     => (/ (* ~a ~b) ~c))
	 ((/ ~a-num (/ ~b-num ~c)) => (/ (/ ~a ~b) ~c))
	 
	 #_
	 ((expt ~a (:?? b ~negative-number?))
	 => (/ 1 (expt ~a (- ~b))))
	 
	 ((* (expt ~a ~b) ~a)        => (expt ~a (+ ~b 1)))
	 ((* (expt ~a ~b) (* ~a ~c)) => (* (expt ~a (+ ~b 1)) ~c))
	 ((* ~a (expt ~a ~b))        => (expt ~a (+ ~b 1)))
	 
	 ((* (expt ~a ~b) (expt ~a ~c)) => (expt ~a (+ ~b ~c)))
	 ((/ (expt ~x ~a) (expt ~x ~b)) => (expt ~x (- ~a ~b)))
	 
	 ((* (expt ~a ~b) (* ~y (expt ~a ~c))) => (* ~y (expt ~a (+ ~b ~c))))
	 
	 ((/ ~a ~a) => 1)
	 
	 ((/ (expt ~a ~b) ~a)        => (expt ~a (- ~b 1)))
	 ((/ (* ~a ~x) (expt ~x ~b)) => (* ~a (/ (expt ~x 1) (expt ~x ~b))))          
	 ((expt (expt ~a ~b) ~c)     => (expt ~a (* ~b ~c)))
	 ((/ ~a (expt ~b (:?? c ~negative-number?))) => (* ~a (expt ~b (- ~c))))
	 ((* ~a (expt ~b (:?? c ~negative-number?))) => (/ ~a (expt ~b (- ~c))))
	 
	 ))))
;;(set! *unification-rules* '())
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn chain-rule [f-prime]
  `(* (d [:? :f] [:? :x])
      ~f-prime))

(defn vector-func? [sym]
  (#{'vector 'list 'cons} sym))

(def derivative-rules
  (map rule:make
    (let [ 
           a [:? :a]
           b [:? :b]
           c [:? :c]
           f [:? :f]
           g [:? :g]
           x [:? :x]
           y [:? :y]
           z [:? :z]
           a-num [:?? :a number?]
           b-num [:?? :b number?]
           c-num [:?? :c number?]
           x-nan [:?? :x not-number?]
           ]
      (partition 3 `(
         (d ~c-num ~x) => 0
         (d ~x ~x)     => 1

         (d (+ ~y ~z) ~x) => (+ (d ~y ~x) (d ~z ~x))
         (d (- ~y ~z) ~x) => (- (d ~y ~x) (d ~z ~x))
         
	 (d (- ~y) ~x) => (- (d ~y ~x))
	 
	 (d (* ~y ~z) ~x) 
	  => 
	  (+ (*    ~y        (d ~z ~x))
	     (* (d ~y ~x)    ~z       ))
	 
	 (d (/ ~y ~z) ~x) 
	  =>
	  (/ (- (* (d ~y ~x)    ~z       )
		(*    ~y        (d ~z ~x)))
	     (expt ~z 2))
	 
	 (d (expt ~x ~a-num) ~x) 
	  =>
	  (* ~a (expt ~x (- ~a 1)))
	 
	 (d (expt ~f ~g) ~x)
	  =>
	  (* (expt ~f ~g)
	     (+ (* (d ~f ~x)
		   (/ ~g ~f))
		(* (d ~g ~x)
		   (log ~f))))
	 
	 (d (log ~f ~x)) 
	  =>
	  (/ (d ~f ~x)
	     ~f)
	 
	 (d (/ 1 ~f) ~x) => ~(chain-rule `(/ -1 ~f))
	 
	 (d (sin ~f) ~x) => ~(chain-rule `(cos ~f))
	 (d (cos ~f) ~x) => ~(chain-rule `(- (sin ~f)))
	 (d (tan ~f) ~x) => ~(chain-rule `(* (expt (/ 1 (cos ~f)) 2)))
	 
	 (d (if ~a ~b) ~x)    => (if ~a (d ~b ~x))
	 (d (if ~a ~b ~c) ~x) => (if ~a (d ~b ~x) (d ~c ~x))
	 
	 (d ((:?? f ~vector-func?) [:&? :rest]) ~x)
	  =>
	  ~(fn [p r dict]
	     (let [ x (dict [:? :x]) ]
	       `(~(dict [:? :f])
                  ~@(map (fn [a] `(d ~a ~x))
                      (dict [:? :rest])))))
	 
	 )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def expand-expression
  (while-change-func 
    (recursive-func
      #(rule:applyn unification-rules
          (rule:applyn simplification-rules
            (rule:applyn expansion-rules %1))))))
 
(def ^:dynamic *expand-and-simplify* true)
(defn differentiate-expression [expr constants]
  (let [ expr          (expand-expression expr)
         d-rules       (concat constants derivative-rules)
         apply-d-rules (while-change-func (recursive-func #(rule:applyn d-rules %1))) ]
    (expand-expression (apply-d-rules expr))))

(def ^:dynamic d:*debug* false) 
(defn d
  [expr x & opts]
  (let [ constants     (if (empty? opts) '()
                         (map #(rule:make `(d ~%1 ~x)) (first opts)))
         expanded-expr (expand-expression expr) ]
    (if d:*debug* (println "expanded expr => " expr))
    (differentiate-expression `(d ~expanded-expr ~x) constants)))

 
