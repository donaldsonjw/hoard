(module association-test
   (library btest
            hoard)
   (export association-tests))

(define-test-suite association-tests

   (test "association? works"
      (assert-true (association? (=> #\a 1)))
      (assert-false (association? '(#\ . 1)))
      (assert-false (association? 1)))

   (test "=>key works"
      (let ((assoc (=> #\a 1)))
         (assert-equal? (=>key assoc) #\a)))

   (test "=>value works"
      (let ((assoc (=> #\a 1)))
         (assert-equal? (=>value assoc) 1)))

   (test "pair->association works"
      (let ((assoc (pair->association (cons #\a 1))))
         (assert-equal? (=>key assoc) #\a)
         (assert-equal? (=>value assoc) 1)))

   ;;; TODO: test association comparator 

   )

