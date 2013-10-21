(ns closol.match-test
  (:require
    [clojure.test :refer :all]
    [closol.match :refer :all]))

(deftest pattern-match-test
  (testing "basics"
    (let [ d { } ]
      (is (= (match '() '() d) d))
      (is (= (match 'x '() d) false))
      (is (= (match 1 1 d) d))
      (is (= (match 'x 'x d) d))
      (is (= (match 'x d d) false))
      (is (= (match '[:? :x] 2 d) {[:? :x] 2}))
      (is (= (match '(1 . 2) '(1 . 2) d) d))
      (is (= (match '(1 . 2) '(1 . 3) d) false))
      (is (not (= (match '[:? :z] 3 d)
                 false)))
      (is (= (match '(+ (:? :a)) '(+ 12) {[:? :x] 2})
            {'(:? :a) 12, [:? :x] 2}))
      (is (= (match `(:?? :a ~number?) 3 { })
            {'(:? :a) 3}))
      (is (= (match `[:?? :a ~number?] :keyword { })
            false))
      (is (= (match `(:?? :a ~number?) 'sym { })
            false))
      )
    )
  (testing "match"
    (is (= (match true true { })
          { }))
    (is (= (match false false { })
          { }))
    (is (= (match 1 1 { })
          { }))
    (is (= (match 1 2 { })
          false))

    (is (= (match 'x 'x { })
          { }))
    (is (= (match 'x 'y { })
          false))

    (is (= (match :x :x { })
          { }))
    (is (= (match :x 'y { })
          false))

    (is (= (match [:? :x] 1 { })
          {[:? :x] 1}))

    (is (= (match [:? :x] [1 2] { })
          {[:? :x] [1 2]}))

    (is (= (match '( ) '( ) { })
          { }))
    (is (= (match '( ) '( 1 ) { })
          false))
    (is (= (match '( 1 ) '( ) { })
          false))

    (is (= (match '((:? :x) 2) '(1 2) { })
          {'(:? :x) 1}))
    (is (= (match '((:? :x) (:? :x)) '(1 1) { })
          {'(:? :x) 1}))
    (is (= (match '((:? :x) (:? :x)) '(1 2) { })
          false))
    (is (= (match '((:? :x) 2 (:? :x)) '(1 2 1) { })
          {'(:? :x) 1}))

    (is (= (match '((:? :x) 2 [:? :x]) '(1 2 1) { })
          {'(:? :x) 1}))

    (is (= (match [ ] [ ] { })
          { }))
    (is (= (match [ ] [ 1 ] { })
          false))
    (is (= (match [ 1 ] [ ] { })
          false))

    (is (= (match [[:? :x] 2] [1 2] { })
          {[:? :x] 1}))
    (is (= (match [[:? :x] [:? :x]] [1 1] { })
          {[:? :x] 1}))
    (is (= (match [[:? :x] [:? :x]] [1 2] { })
          false))
    (is (= (match [[:? :x] 2 [:? :x]] [1 2 1] { })
          {[:? :x] 1}))

    (is (= (match [1 2 [:&? :rest]] [1 2 3 4] { })
          {[:? :rest] [3 4]}))
    ))

(deftest pattern-replace-test
  (testing "pattern-dictionary:replace"
    (is (= (pattern-dictionary:replace
             {[:? :x] '[a b]}
             '(1 2 [:? :x])
             '(1 2 3 [:? :x] 4 5)
             )
          '(1 2 3 [a b] 4 5)))
    (is (= (pattern-dictionary:replace
             {[:? :x] '(a b)}
             '(1 2 [:? :x])
             '[1 2 3 [:? :x] 4 5]
             )
          '[1 2 3 (a b) 4 5]))

    (is (= (pattern-dictionary:replace
             {[:? :x] '(a b)}
             '(1 2 [:? :x])
             '[1 2 3 [:&? :x]]
             )
          '[1 2 3 a b]))
    ))
