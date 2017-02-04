(module comparator-test
   (library btest hoard)
   (export comparator-tests))

(define-test-suite comparator-tests

   ;;; string-comparator tests
   (test "+string-comparator+ is a comparator"
      (assert-true (comparator? +string-comparator+))
      (assert-false (comparator? 5)))

   (test "+string-comparator+ is ordered"
      (assert-true (comparator-ordered? +string-comparator+)))

   (test "+string-comparator+ is hashable"
      (assert-true (comparator-hashable? +string-comparator+)))

   (test "+string-comparator+ type predicate works"
      (assert-true (comparator-type? +string-comparator+ "test string"))
      (assert-false (comparator-type? +string-comparator+ 4)))

   (test "comparator=? works for +string-comparator+"
      (assert-true (comparator=? +string-comparator+ "123" "123"))
      (assert-false (comparator=? +string-comparator+ "123" "456")))

   (test "comparator<? works for +string-comparator+"
      (assert-false (comparator<? +string-comparator+ "123" "123"))
      (assert-true (comparator<? +string-comparator+ "123" "456")))

   (test "comparator>? works for +string-comparator+"
      (assert-false (comparator>? +string-comparator+ "123" "123"))
      (assert-true (comparator>? +string-comparator+ "456" "123")))

   (test "comparator<=? works for +string-comparator+"
      (assert-true (comparator<=? +string-comparator+ "123" "123"))
      (assert-true (comparator<=? +string-comparator+ "123" "456"))
      (assert-false (comparator<=? +string-comparator+ "456" "123")))

   (test "comparator>=? works for +string-comparator+"
      (assert-true (comparator>=? +string-comparator+ "123" "123"))
      (assert-true (comparator>=? +string-comparator+ "456" "123"))
      (assert-false (comparator>=? +string-comparator+ "123" "456")))

   (test "comparator-hash works for +string-comparator+"
      (assert-true (number? (comparator-hash +string-comparator+ "test string")))
      (assert-true (= (comparator-hash +string-comparator+ "test string")
                      (comparator-hash +string-comparator+ (string-append "test " "string")))))

   ;;; string-ci-comparator tests
   (test "+string-ci-comparator+ is a comparator"
      (assert-true (comparator? +string-ci-comparator+))
      (assert-false (comparator? 5)))

   (test "+string-ci-comparator+ is ordered"
      (assert-true (comparator-ordered? +string-ci-comparator+)))

   (test "+string-ci-comparator+ is hashable"
      (assert-true (comparator-hashable? +string-ci-comparator+)))

   (test "+string-ci-comparator+ type predicate works"
      (assert-true (comparator-type? +string-ci-comparator+ "test string"))
      (assert-false (comparator-type? +string-ci-comparator+ 4)))

   (test "comparator=? works for +string-ci-comparator+"
      (assert-true (comparator=? +string-ci-comparator+ "123" "123"))
      (assert-true (comparator=? +string-ci-comparator+ "abc" "ABC"))
      (assert-false (comparator=? +string-ci-comparator+ "123" "456")))

   (test "comparator<? works for +string-ci-comparator+"
      (assert-false (comparator<? +string-ci-comparator+ "123" "123"))
      (assert-false (comparator<? +string-ci-comparator+ "ABC" "abc"))
      (assert-true (comparator<? +string-ci-comparator+ "123" "456")))

   (test "comparator>? works for +string-ci-comparator+"
      (assert-false (comparator>? +string-ci-comparator+ "123" "123"))
      (assert-false (comparator>? +string-ci-comparator+ "ABC" "abc"))
      (assert-true (comparator>? +string-ci-comparator+ "456" "123")))

   (test "comparator<=? works for +string-ci-comparator+"
      (assert-true (comparator<=? +string-ci-comparator+ "123" "123"))
      (assert-true (comparator<=? +string-ci-comparator+ "123" "456"))
      (assert-true (comparator<=? +string-ci-comparator+ "ABC" "abc"))
      (assert-false (comparator<=? +string-ci-comparator+ "456" "123")))

   (test "comparator>=? works for +string-ci-comparator+"
      (assert-true (comparator>=? +string-ci-comparator+ "123" "123"))
      (assert-true (comparator>=? +string-ci-comparator+ "456" "123"))
      (assert-true (comparator>=? +string-ci-comparator+ "ABC" "abc"))
      (assert-false (comparator>=? +string-ci-comparator+ "123" "456")))

   (test "comparator-hash works for +string-ci-comparator+"
      (assert-true (number? (comparator-hash +string-ci-comparator+ "test string")))
      (assert-true (= (comparator-hash +string-ci-comparator+ "test string")
                      (comparator-hash +string-ci-comparator+ (string-append "test " "string"))))
      (assert-true (= (comparator-hash +string-ci-comparator+ "test string")
                      (comparator-hash +string-ci-comparator+ (string-append "teST " "strING")))))


    ;;; number-comparator tests
   (test "+number-comparator+ is a comparator"
      (assert-true (comparator? +number-comparator+))
      (assert-false (comparator? 5)))

   (test "+number-comparator+ is ordered"
      (assert-true (comparator-ordered? +number-comparator+)))

   (test "+number-comparator+ is hashable"
      (assert-true (comparator-hashable? +number-comparator+)))

   (test "+number-comparator+ type predicate works"
      (assert-true (comparator-type? +number-comparator+ 123))
      (assert-true (comparator-type? +number-comparator+ #z123))
      (assert-true (comparator-type? +number-comparator+ #l123))
      (assert-true (comparator-type? +number-comparator+ #e123))
      (assert-true (comparator-type? +number-comparator+ 123.0))
      (assert-false (comparator-type? +number-comparator+ "4")))

   (test "comparator=? works for +number-comparator+"
      (assert-true (comparator=? +number-comparator+ 123 123.0))
      (assert-true (comparator=? +number-comparator+ 123 #z123))
      (assert-false (comparator=? +number-comparator+ 123 456.5)))

   (test "comparator<? works for +number-comparator+"
      (assert-false (comparator<? +number-comparator+ 123 123.0))
      (assert-false (comparator<? +number-comparator+ 456 #z123))
      (assert-true (comparator<? +number-comparator+ 123 456)))

   (test "comparator>? works for +number-comparator+"
      (assert-false (comparator>? +number-comparator+ 123.0 123))
      (assert-false (comparator>? +number-comparator+ #z123 456))
      (assert-true (comparator>? +number-comparator+ 456 123)))

   (test "comparator<=? works for +number-comparator+"
      (assert-true (comparator<=? +number-comparator+ #l123 123))
      (assert-true (comparator<=? +number-comparator+ 123.0  456))
      (assert-false (comparator<=? +number-comparator+ 456 123)))

   (test "comparator>=? works for +number-comparator+"
      (assert-true (comparator>=? +number-comparator+ 123 123))
      (assert-true (comparator>=? +number-comparator+ 456.5 123))
      (assert-false (comparator>=? +number-comparator+ 123.0 456)))

   (test "comparator-hash works for +number-comparator+"
      (assert-true (number? (comparator-hash +number-comparator+ #z123)))
      (assert-true (= (comparator-hash +number-comparator+ 123)
                      (comparator-hash +number-comparator+ 123)))
      (assert-true (= (comparator-hash +number-comparator+ 123.0)
                      (comparator-hash +number-comparator+ 123)))
      (assert-true (= (comparator-hash +number-comparator+ #z1234)
                      (comparator-hash +number-comparator+ 1234))))

    ;;; char-comparator tests
   (test "+char-comparator+ is a comparator"
      (assert-true (comparator? +char-comparator+))
      (assert-false (comparator? 5)))

   (test "+char-comparator+ is ordered"
      (assert-true (comparator-ordered? +char-comparator+)))

   (test "+char-comparator+ is hashable"
      (assert-true (comparator-hashable? +char-comparator+)))

   (test "+char-comparator+ type predicate works"
      (assert-true (comparator-type? +char-comparator+ #\a))
      (assert-true (comparator-type? +char-comparator+ #\Z))
      (assert-true (comparator-type? +char-comparator+ #\b))
      (assert-true (comparator-type? +char-comparator+ #a032))
      (assert-true (comparator-type? +char-comparator+ #a056))
      (assert-false (comparator-type? +char-comparator+ "4")))

   (test "comparator=? works for +char-comparator+"
      (assert-true (comparator=? +char-comparator+ #\a #\a))
      (assert-true (comparator=? +char-comparator+ #\B #\B))
      (assert-false (comparator=? +char-comparator+ #\a #\b)))

   (test "comparator<? works for +char-comparator+"
      (assert-false (comparator<? +char-comparator+ #\b #\a))
      (assert-false (comparator<? +char-comparator+ #\b #\b))
      (assert-true (comparator<? +char-comparator+ #\a #\z)))

   (test "comparator>? works for +char-comparator+"
      (assert-false (comparator>? +char-comparator+ #\a #\a))
      (assert-false (comparator>? +char-comparator+ #\a #\b))
      (assert-true (comparator>? +char-comparator+ #\z #\a)))

   (test "comparator<=? works for +char-comparator+"
      (assert-true (comparator<=? +char-comparator+ #\a #\a))
      (assert-true (comparator<=? +char-comparator+ #\a  #\z))
      (assert-false (comparator<=? +char-comparator+ #\z #\a)))

   (test "comparator>=? works for +char-comparator+"
      (assert-true (comparator>=? +char-comparator+ #\a #\a))
      (assert-true (comparator>=? +char-comparator+ #\b #\a))
      (assert-false (comparator>=? +char-comparator+ #\a #\s)))

   (test "comparator-hash works for +char-comparator+"
      (assert-true (number? (comparator-hash +char-comparator+ #\a)))
      (assert-true (= (comparator-hash +char-comparator+ #\a)
                      (comparator-hash +char-comparator+ #\a))))
)