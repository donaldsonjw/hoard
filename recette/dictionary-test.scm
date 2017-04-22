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

   (test "dictionary-update!  works"
      (let ((dict (create-hashtable)))
         (dictionary-update! dict 1 2 (lambda (x) (+ x 1)))
         (assert-equal? (dictionary-get dict 1) 2)
         (dictionary-update! dict 1 0 (lambda (x) (+ x 1)))
         (assert-equal? (dictionary-get dict 1) 3)))
   
   (test "dictionary-remove! works"
      (let ((dict (create-hashtable)))
         (dictionary-put! dict #\a 1)
         (dictionary-put! dict #\b 2)
         (dictionary-put! dict #\c 3)
         (dictionary-remove! dict #\a)
         (assert-false (dictionary-get dict #\a)) 
         (dictionary-remove! dict #\b)
         (assert-false (dictionary-get dict #\b))))

   (test "dictionary-contains? works"
      (let ((dict (hashtable (=> #\a 1) (=> #\b 2) (=> #\c 3))))
         (assert-true (dictionary-contains? dict #\a))
         (assert-false (dictionary-contains? dict #\d))
         (assert-true (dictionary-contains? dict #\c))))
   
   (test "dictionary-empty? works"
      (let ((dict (hashtable)))
         (assert-true (dictionary-empty? dict))
         (dictionary-put! dict #\a 1)
         (assert-false (dictionary-empty? dict))))

   (test "dictionary-length works"
      (let ((dict (hashtable)))
         (assert-equal? (dictionary-length dict) 0)
         (dictionary-put! dict #\a 1)
         (assert-equal? (dictionary-length dict) 1)
         (dictionary-remove! dict #\a)
         (assert-equal? (dictionary-length dict) 0)))   
   )
