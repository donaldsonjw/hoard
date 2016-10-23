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
      (assert-true (collection-indexable? "string")))


   (test "collection-ref for list works"
      (assert-equal? (collection-ref (list 1 2 3 4) 1)
         2))

   (test "collection-ref for vector works"
      (assert-equal? (collection-ref (vector 1 2 3 4) 1)
         2))
   
   (test "collection-ref for strings work"
      (assert-equal? (collection-ref "abcdedf" 1)
         #\b))

   (test "collection-ref for hashtable works"
      (let ((table (create-hashtable)))
         (hashtable-put! table "k1" 1)
         (hashtable-put! table "k2" 2)
         (assert-equal? (collection-ref table "k2")
            2)))

   (test "collection-ref for list with default works"
      (assert-equal? (collection-ref (list 1 2 3) 4 5)
         5))

   (test "collection-ref for vector with default works"
      (assert-equal? (collection-ref (vector 1 2 3) 4 5)
         5))

   (test "collection-ref for string with default works"
      (assert-equal? (collection-ref "abc" 4 #\d)
         #\d))

   (test "collection-ref for hashtable with default works"
      (let ((table (create-hashtable)))
         (hashtable-put! table "k1" 1)
         (hashtable-put! table "k2" 2)
         (assert-equal? (collection-ref table "k3" 4)
            4)))

   (test "collection-ref throws exception for invalid index on list"
      (assert-exception-thrown
         (collection-ref (list 1 2 3) 4) &invalid-index-exception))

   (test "collection-ref throws exception for invalid index on vector"
      (assert-exception-thrown
         (collection-ref (vector 1 2 3) 4) &invalid-index-exception))

   (test "collection-ref throws exception for invalid index on string"
      (assert-exception-thrown
         (collection-ref "abc" 4) &invalid-index-exception))

   (test "collection-ref throws exception for invalid index on hashtable"
      (assert-exception-thrown
         (collection-ref (create-hashtable) 4) &invalid-index-exception))

   (test "collection-set! works on lists"
      (let ((t (list 1 2 3 4)))
         (collection-set! t 2 5)
         (assert-equal? (list-ref t 2) 5)))

   (test "collection-set! works on vectors"
      (let ((t (vector 1 2 3 4)))
         (collection-set! t 2 5)
         (assert-equal? (vector-ref t 2) 5)))

   (test "collection-set! works on strings"
      (let ((t "abcdef"))
         (collection-set! t 3 #\k)
         (assert-equal? (string-ref t 3) #\k)))

   (test "collection-set! works on hashtables"
      (let ((t (create-hashtable)))
         (collection-set! t "key" 5)
         (collection-set! t "key" 4)
         (assert-equal? (hashtable-get t "key") 4)))
         
   (test "collection-set! on a list should throw an exception with an invalid index"
      (let ((t (list 1 2 3)))
         (assert-exception-thrown (collection-set! t 5 5) &invalid-index-exception)))

   (test "collection-set! on a vector should throw an exception with an invalid index"
      (let ((t (vector 1 2 3)))
         (assert-exception-thrown (collection-set! t 5 5) &invalid-index-exception)))

   (test "collection-set! on a string should throw an exception with an invalid index"
      (let ((t "abc"))
         (assert-exception-thrown (collection-set! t 5 5) &invalid-index-exception)))

   (test "collection-slice works on lists"
      (assert-equal? (enumerator->list (collection-slice '(1 2 3 4) (range :start 1 :end 3)))
         '(2 3)))
   
   (test "collection-slice works on vectors"
      (assert-equal? (enumerator->list (collection-slice '#(1 2 3 4) (range :start 1 :end 3)))
         '(2 3)))

   (test "collection-slice works on strings"
      (assert-equal? (enumerator->list (collection-slice "abcd" (range :start 1 :end 3)))
         '(#\b #\c)))

   (test "collection-slice works on hashtables"
      (let ((table (create-hashtable)))
         (hashtable-put! table "a" 1)
         (hashtable-put! table "b" 2)
         (hashtable-put! table "c" 1)
         (hashtable-put! table "d" 1)
         (assert-equal? (enumerator->list (collection-slice table '("b" "c")))
            '(2 1))))
   
   )


   