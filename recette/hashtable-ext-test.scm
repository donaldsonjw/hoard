(module hashtable-ext-test
   (library btest
            hoard)
   (export hashtable-ext-tests))


(define-test-suite hashtable-ext-tests

   (test "(hashtable '(a  1) '(b 2) '(c 3)))"
      (let ((res (hashtable '(a  1) '(b  2) '(c 3))))
         (assert-equal? (hashtable-get res 'a) 1)
         (assert-equal? (hashtable-get res 'b) 2)
         (assert-equal? (hashtable-get res 'c) 3)))

   (test "(hashtable 4) throws an invalid argument exception"
      (assert-exception-thrown (hashtable 4) &invalid-argument-exception))

   (test "(hashtable :weak 'keys) works"
      (let ((t1 (hashtable :weak 'keys))
            (t2 (hashtable)))
         (assert-true (hashtable-weak-keys? t1))
         (assert-false (hashtable-weak-keys? t2))))
   
   (test "(hashtable :weak 'data) works"
      (let ((t1 (hashtable :weak 'data))
            (t2 (hashtable)))
         (assert-true (hashtable-weak-data? t1))
         (assert-false (hashtable-weak-data? t2))))

   (test "(hashtable :weak 'both) works"
      (let ((t1 (hashtable :weak 'both))
            (t2 (hashtable)))
         (assert-true (hashtable-weak-data? t1))
         (assert-true (hashtable-weak-keys? t1)) 
         (assert-false (hashtable-weak-data? t2))
         (assert-false (hashtable-weak-keys? t2))))

   (test "(hashtable :eqtest string-ci=? :hash (lambda (s) (get-hashnumber (string-upcase s))) '(\"a\" 1) '(\"b\" 2) '(\"c\" 3)) works"
      (let ((t1 (hashtable :eqtest string-ci=? :hash (lambda (s) (get-hashnumber (string-upcase s))) '("a" 1) '("b" 2) '("c" 3))))
         (assert-true (hashtable-contains? t1 "a"))
         (assert-true (hashtable-contains? t1 "A"))
         (assert-true (hashtable-contains? t1 "b"))
         (assert-true (hashtable-contains? t1 "B"))
         (assert-true (hashtable-contains? t1 "C"))
         (assert-true (hashtable-contains? t1 "c"))
         ))

   
   )