(module mutable-collection-test
   (library btest hoard)
   (export mutable-collection-tests))


(define-test-suite mutable-collection-tests

   (test "a list is a mutable collection"
      (assert-true (collection-mutable? (list))))

   
   (test "a vector is a mutable collection"
      (assert-true (collection-mutable? (vector))))

   (test "a string is a mutable collection"
      (assert-true (collection-mutable? (string))))

   (test "a hashtable is a mutable collection"
      (assert-true (collection-mutable? (create-hashtable))))

   
   )


  




   


