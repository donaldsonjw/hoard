(module collection-test
   (library btest hoard)
   (export collection-tests))

(define-test-suite collection-tests
   
   (test "a list is a collection"
      (assert-true (collection? (list 1 2 3))))
   
   (test "a vector is a collection"
      (assert-true (collection? (vector 1 2 3))))
   
   (test "a hashtable is a collection"
      (assert-true (collection? (create-hashtable))))
   
   (test "a string is a collection of characters"
      (assert-true (collection? (string))))

   (test "1 is not a collection"
      (assert-false (collection? 1)))

   (test "1.0 is not a collection"
      (assert-false (collection? 1.0)))

   (test "(1 2 3) has length 3"
      (assert= (collection-length '(1 2 3)) 3))

   (test "#(1 2 3) has length 3"
      (assert= (collection-length '#(1 2 3)) 3))

   (test "(hash (a . 1) (b . 2) (c . 3)) has length 3"
      (let ((hash (create-hashtable)))
         (hashtable-put! hash 'a 1)
         (hashtable-put! hash 'b 2)
         (hashtable-put! hash 'c 3)
         (assert= (collection-length hash) 3)))

   (test "\"abc\" has length 3"
      (assert= (collection-length "abc") 3))

   (test "collection-contains? works on lists"
      (assert-true (collection-contains? '(1 2 3) 1)))

   (test "collection-contains? works on vectors"
      (assert-true (collection-contains? '#(1 2 3) 1)))

   (test "collection-contains? works on hashtables"
      (let ((hash (create-hashtable)))
         (hashtable-put! hash 'a 1)
         (hashtable-put! hash 'b 2)
         (hashtable-put! hash 'c 3)
         (assert-true (collection-contains? hash 1))))

   (test "collection-element for list works"
      (assert-equal? (collection-element (list 1 2 3 4) 1)
         2))

   (test "collection-element for vector works"
      (assert-equal? (collection-element (vector 1 2 3 4) 1)
         2))
   
   (test "collection-element for strings work"
      (assert-equal? (collection-element "abcdedf" 1)
         #\b))

   (test "collection-element for hashtable works"
      (let ((table (create-hashtable)))
         (hashtable-put! table "k1" 1)
         (hashtable-put! table "k2" 2)
         (assert-equal? (collection-element table "k2")
            2)))

   (test "collection-element for list with default works"
      (assert-equal? (collection-element (list 1 2 3) 4 5)
         5))

   (test "collection-element for vector with default works"
      (assert-equal? (collection-element (vector 1 2 3) 4 5)
         5))

   (test "collection-element for string with default works"
      (assert-equal? (collection-element "abc" 4 #\d)
         #\d))

   (test "collection-element for hashtable with default works"
      (let ((table (create-hashtable)))
         (hashtable-put! table "k1" 1)
         (hashtable-put! table "k2" 2)
         (assert-equal? (collection-element table "k3" 4)
            4)))

   (test "collection-element throws exception for invalid index on list"
      (assert-exception-thrown
         (collection-element (list 1 2 3) 4) &invalid-index-exception))

   (test "collection-element throws exception for invalid index on vector"
      (assert-exception-thrown
         (collection-element (vector 1 2 3) 4) &invalid-index-exception))

   (test "collection-element throws exception for invalid index on string"
      (assert-exception-thrown
         (collection-element "abc" 4) &invalid-index-exception))

   (test "collection-element throws exception for invalid index on hashtable"
      (assert-exception-thrown
         (collection-element (create-hashtable) 4) &invalid-index-exception))


   (test "collection-empty? works on empty list"
      (assert-true (collection-empty? (list))))

   (test "collection-empty? works on non-empty list"
      (assert-false (collection-empty? (list 1))))

   (test "collection-empty? works on empty vector"
      (assert-true (collection-empty? (vector))))

   (test "collection-empty? works on non-empty vector"
      (assert-false (collection-empty? (vector 1))))

   (test "collection-empty? works on empty string"
      (assert-true (collection-empty? "")))

   (test "collection-empty? works on non-empty string"
      (assert-false (collection-empty? "a")))

   (test "collection-empty? works on empty hashtable"
      (assert-true (collection-empty? (create-hashtable))))

   (test "collection-empty? works on non-empty hashtable"
      (let ((t (create-hashtable)))
         (hashtable-put! t "a" 1)
         (assert-false (collection-empty? t))))
   
   
   )






