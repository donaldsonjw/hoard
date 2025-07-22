(module comparable-test
   (library btest hoard)
   (export comparable-tests))


(define-test-suite comparable-tests

   (test "comparable? works"
      (assert-true (comparable? 1))
      (assert-true (comparable? #z1))
      (assert-true (comparable? 1.0))
      (assert-true (comparable? #l3))
      (assert-true (comparable? "dog"))
      (assert-true (comparable? #u"dog"))
      (assert-true (comparable? 'dog))
      (assert-true (comparable? #\a))
      (assert-true (comparable? #u0020))
      (assert-false (comparable? (vector)))
      (assert-false (comparable? (list)))))


