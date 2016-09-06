(module stretchy-vector-test
   (library btest hoard)
   (export stretchy-vector-tests))

(define-test-suite stretchy-vector-tests

   (test "a list is not a stretchy-vector"
      (assert-false (stretchy-vector? '(1 2 3))))
   
   (test "a vector is not a stretchy-vector"
      (assert-false (stretchy-vector? '#(1 2 3))))

   (test "an empty stretchy-vector is a stretchy-vector"
      (let ((svec (make-stretchy-vector 0)))
         (assert-true (stretchy-vector? svec))
         (assert-equal? (stretchy-vector-capacity svec)
            +minimum-stretchy-vector-capacity+)))

   (test "(stretchy-vector 1 2 3 4 5) returns a stretchy-vector of length 5"
      (let ((svec (stretchy-vector 1 2 3 4 5)))
         (assert-equal? (stretchy-vector-length svec) 5)))

   (test "(make-stretchy-vector 5) returns a stretchy-vector of length 5"
      (let ((svec (make-stretchy-vector 5)))
         (assert-equal? (stretchy-vector-length svec) 5)))

   (test "referencing index 3 of (stretchy-vector 1 2 3 4) returns 4"
      (let ((svec (stretchy-vector 1 2 3 4)))
         (assert-equal? (stretchy-vector-ref svec 3) 4)))

   (test "referencing index 4 of (stretchy-vector 1 2 3 4) throws an invalid index exception"
      (let ((svec (stretchy-vector 1 2 3 4)))
         (assert-exception-thrown (stretchy-vector-ref svec 5) 
            &invalid-index-exception)))

   (test "setting index 4 of (make-stretchy-vector 1 2) succeeds"
      (let ((svec (make-stretchy-vector 2)))
         (stretchy-vector-set! svec 4 5)
         (assert-equal? (stretchy-vector-ref svec 4) 5)
         (assert-equal? (stretchy-vector-capacity svec) +minimum-stretchy-vector-capacity+)
         ))

   (test "(list->stretchy-vector '(1 2 3)) works"
      (let ((svec (list->stretchy-vector '(1 2 3))))
         (assert-equal? (stretchy-vector-ref svec 0) 1)
         (assert-equal? (stretchy-vector-ref svec 1) 2)
         (assert-equal? (stretchy-vector-ref svec 2) 3)))
   
   (test "(vector->stretchy-vector '#(1 2 3)) works"
      (let ((svec (vector->stretchy-vector '#(1 2 3))))
         (assert-equal? (stretchy-vector-ref svec 0) 1)
         (assert-equal? (stretchy-vector-ref svec 1) 2)
         (assert-equal? (stretchy-vector-ref svec 2) 3)))

   (test "(stretchy-vector->list (stretchy-vector 1 2 3)) works"
      (assert-equal? (stretchy-vector->list (stretchy-vector 1 2 3))
         '(1 2 3)))
   
   (test "(stretchy-vector->vector (stretchy-vector 1 2 3)) works"
      (assert-equal? (stretchy-vector->vector (stretchy-vector 1 2 3))
         '#(1 2 3)))


   (test "(stretchy-vector-resize! (stretchy-vector 1 2 3 4 5 6) 3) works"
      (let ((svec (stretchy-vector-resize! (stretchy-vector 1 2 3 4 5 6) 3)))
         (assert-equal? (stretchy-vector-length svec) 3)
         (assert-equal? (stretchy-vector-ref svec 0) 1)
         (assert-equal? (stretchy-vector-ref svec 1) 2)
         (assert-equal? (stretchy-vector-ref svec 2) 3)))

   (test "(stretchy-vector-copy (stretchy-vector 1 2 3)) works"
      (let* ((svec (stretchy-vector 1 2 3))
            (cvec (stretchy-vector-copy svec)))
         (assert-false (eq? svec cvec))
         (assert-equal? svec cvec)))

   (test "(stretchy-vector-map! (lambda (x) (+ x 1)) (stretchy-vector 1 2 3)) works"
      (let ((svec (stretchy-vector-map! (lambda (x) (+ x 1)) (stretchy-vector 1 2 3))))
         (assert-equal? (stretchy-vector-ref svec 0) 2)
         (assert-equal? (stretchy-vector-ref svec 1) 3)
         (assert-equal? (stretchy-vector-ref svec 2) 4)))

   (test "(stretchy-vector-map (lambda (x) (+ x 1)) (stretchy-vector 1 2 3)) works"
      (let* ((ovec (stretchy-vector 1 2 3))
            (svec (stretchy-vector-map (lambda (x) (+ x 1)) ovec)))
         (assert-equal? (stretchy-vector-ref svec 0) 2)
         (assert-equal? (stretchy-vector-ref svec 1) 3)
         (assert-equal? (stretchy-vector-ref svec 2) 4)
         (assert-false (eq? ovec svec))))

   (test "(stretchy-vector-extend! (stretchy-vector 1 2 3) 4) works"
      (let ((svec (stretchy-vector 1 2 3)))
         (stretchy-vector-extend! svec 4)
         (assert-equal? (stretchy-vector-ref svec 3) 4)
         ;; also confirm capacity doubling
         (assert-equal? (stretchy-vector-capacity svec) 6)))

   (test "(stretchy-vector-remove! (stretchy-vector 1 2 3)) works)"
      (let* ((svec (stretchy-vector 1 2 3))
             (res (stretchy-vector-remove! svec)))
         (assert-equal? res 3)
         (assert-exception-thrown (stretchy-vector-ref svec 4) 
          &invalid-index-exception)
         (assert-equal? (stretchy-vector-length svec) 2)))

   (test "stretchy-vector-remove! will cause resizing when the lenth is 1/4 capacity"
      (let* ((svec (stretchy-vector 1 2 3 4)))
         (stretchy-vector-remove! svec)
         (stretchy-vector-remove! svec)
         (stretchy-vector-remove! svec)
         (assert-equal? (stretchy-vector-capacity svec) 2)))
   


   )


