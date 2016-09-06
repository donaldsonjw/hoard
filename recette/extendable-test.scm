(module extendable-test
   (library btest hoard)
   (export extendable-tests))




(define-test-suite extendable-tests
   (test "extending a list adds the item to the head of the list"
      (let ((t (list 1 2 3 4)))
         (collection-extend! t 5)
         (assert-equal? t '(5 1 2 3 4))))

   (test "list is extendable"
      (assert-true (collection-extendable? (list 1 2 3))))

   (test "vector is not extendable"
      (assert-false (collection-extendable? (vector 1 2 3))))
   )
