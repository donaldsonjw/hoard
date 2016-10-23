(module dictionary-test
   (library btest
            hoard)
   (export dictionary-tests))


(define-test-suite dictionary-tests

   (test "dictionary? works"
      (assert-true (dictionary? (create-hashtable)))
      (assert-false (dictionary? (list))))

   (test "dictionary-get and dictionary-put!  works"
      (let ((dict (create-hashtable)))
         (dictionary-put! dict #\a 2)
         (assert-equal? (dictionary-get dict #\a) 2)
         (dictionary-put! dict #\b 3)
         (assert-equal? (dictionary-get dict #\b) 3)
         (dictionary-put! dict #\a 1)
         (assert-equal? (dictionary-get dict #\a) 1)))

   (test "dictionary-remove! works"
      (let ((dict (create-hashtable)))
         (dictionary-put! dict #\a 1)
         (dictionary-put! dict #\b 2)
         (dictionary-put! dict #\c 3)
         (dictionary-remove! dict #\a)
         (assert-false (dictionary-get dict #\a)) 
         (dictionary-remove! dict #\b)
         (assert-false (dictionary-get dict #\b))))
         
   
   )
