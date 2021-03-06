(ns closol.random)

(defn md5 [bytea]
  (.digest (java.security.MessageDigest/getInstance "MD5") bytea))

(defn ubyte [x]
  (let [i (int x)]
    (if (< i 0) (+ 256 i) i)))

(defn ubytes-to-integer [x]
  (let [count-x (count x)]
    (loop [ a 0N i 0 p 1N ]
      (if (< i count-x)
        (recur (+ a (* p (ubyte (nth x i))))
          (+ i 1)
          (* p 256))
        a))))

(def md5-denom
  (ubytes-to-integer (reverse (cons 1 (map (fn [_] 0) (md5 (.getBytes "")))))))
(def md5-denom-float
  (* 1.0 md5-denom))

(defn valid-as-long? [x]
  (and (<= Long/MIN_VALUE x) (<= x Long/MAX_VALUE)))

(defn trunc [x]
  (cond
    (ratio? x)    (trunc (quot (numerator x) (denominator x)))
    (float? x)    (if (valid-as-long? x) (long x) (bigint x))
    (integer? x)  (if (valid-as-long? x) (long x) x)
    :else         x))

(defn bconcat [& arrays]
  (let [sizes (map count arrays)
        sizes_r (vec (reductions + sizes))
        offsets (cons 0 (drop-last sizes_r))
        total (last sizes_r)
        out (byte-array total)]
    (dorun (map #(System/arraycopy %2 0 out %1 %3) offsets arrays sizes))
    out))

(defn make-random
"Returns a procedure r which returns pseudo-random values:

(r)   => [0.0, 1.0)
(r n) => [0, n - 1]
"
  [s]
  (let [ seed (md5 (.getBytes (str s)))
         state (ref seed) ]
    (fn [& args]
      (let [ new-state (alter state #(md5 (bconcat %1 seed)))
             n         (ubytes-to-integer new-state) ]
        (cond
          (empty? args)         (/ n md5-denom-float)
          (empty? (rest args))  (trunc (/ (* n (first args)) md5-denom))
          :else false))))) ;; ERROR

(defn random-element
  "Return a random element from seq."
  [r seq]
  (nth seq (r (count seq))))

(defn take-at [i seq]
  [(nth seq i) (concat (take i seq) (drop (+ i 1) seq))])

(defn random-sequence
  "Return a random sequence from seq."
  [r seq]
  (lazy-seq
    (loop [out [] in seq]
      (if (empty? in) out
      (let [x (take-at (r (count in)) in)]
        (recur (cons (first x) out) (second x)))))))

(defn random-merge
  "Randomly merge elements from sequences."
  ([r a b & seqs] (random-merge r (concat [a b] seqs)))
  ([r seqs]
    (lazy-seq
      (let [seqs (filter seq seqs)]
        (if (empty? seqs) '()
          (cons
            (first (random-element r seqs))
            (random-merge r (map rest seqs))))))))

