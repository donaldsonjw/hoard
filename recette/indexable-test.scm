(module indexable-test
   (library btest hoard)
   (export indexable-tests))





(define-test-suite indexable-tests

   (test "list is indexable"
      (assert-true (collection-indexable? (list 1 2 3))))

   (test "vector is indexable"
      (assert-true (collection-indexable? (vector 1 2 3))))

   (test "hashtable is indexable"
      (assert-true (collection-indexable? (create-hashtable))))

   (test "string is indexable"
      (assert-true (collection-indexable? "string"))))

