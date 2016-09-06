(module mutable-collection-test
   (library btest hoard)
   (export mutable-collection-tests))


(define-test-suite mutable-collection-tests

   (test "a list is a mutable collection"
      (assert-true (mutable-collection? (list))))

   
   (test "a vector is a mutable collection"
      (assert-true (mutable-collection? (vector))))

   (test "a string is a mutable collection"
      (assert-true (mutable-collection? (string))))

   (test "a hashtable is a mutable collection"
      (assert-true (mutable-collection? (create-hashtable))))

   (test "collection-element-set! works on lists"
      (let ((t (list 1 2 3 4)))
         (collection-element-set! t 2 5)
         (assert-equal? (list-ref t 2) 5)))

   (test "collection-element-set! works on vectors"
      (let ((t (vector 1 2 3 4)))
         (collection-element-set! t 2 5)
         (assert-equal? (vector-ref t 2) 5)))

   (test "collection-element-set! works on strings"
      (let ((t "abcdef"))
         (collection-element-set! t 3 #\k)
         (assert-equal? (string-ref t 3) #\k)))

   (test "collection-element-set! works on hashtables"
      (let ((t (create-hashtable)))
         (collection-element-set! t "key" 5)
         (collection-element-set! t "key" 4)
         (assert-equal? (hashtable-get t "key") 4)))
         
   (test "collection-element-set! on a list should throw an exception with an invalid index"
      (let ((t (list 1 2 3)))
         (assert-exception-thrown (collection-element-set! t 5 5) &invalid-index-exception)))

   (test "collection-element-set! on a vector should throw an exception with an invalid index"
      (let ((t (vector 1 2 3)))
         (assert-exception-thrown (collection-element-set! t 5 5) &invalid-index-exception)))

   (test "collection-element-set! on a string should throw an exception with an invalid index"
      (let ((t "abc"))
         (assert-exception-thrown (collection-element-set! t 5 5) &invalid-index-exception)))
   
   )


  




   


