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

   (test "a stretchy-vector is mutable"
      (assert-true (collection-mutable? (stretchy-vector 1 2 3))))
   
   (test "a stretchy-vector is indexable"
      (assert-true (collection-indexable? (stretchy-vector 1 2))))

   (test "collection-ref for stretchy-vector works"
      (assert-equal? (collection-ref (stretchy-vector 1 2 3 4) 1)
         2))

   (test "collection-ref for stretch-vector with default works"
      (assert-equal? (collection-ref (vector 1 2 3) 4 5)
         5))

   (test "collection-set! works on stretchy-vectors"
      (let ((t (stretchy-vector 1 2 3 4)))
         (collection-set! t 2 5)
         (assert-equal? (stretchy-vector-ref t 2) 5)))

   (test "collection-set! on a stretchy-vector should not throw an exception with an invalid index but"
         " extend to include the new index"
      (let ((t (stretchy-vector 1 2 3)))
         (collection-set! t 5 5)
         (assert-equal? (stretchy-vector-length t) 6)))

   
   ;;;; enumerable tests
   (test "enumerable-for-each on stretch-vectors works"
      (let ((count 0))
         (enumerable-for-each (lambda (e) (set! count (+ count 1))) (stretchy-vector 1 2 3))
         (assert= count 3)))

   (test "enumerable-map on stretchy-vectors works"
         (assert-equal? (enumerator->list (enumerable-map (lambda (e) (+ e 1)) (stretchy-vector 1 2 3))) '(2 3 4)))

   (test "enumerable-filter on stretcht-vectors works"
      (assert-equal? (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) (stretchy-vector 1 2 3))) '(2 3)))

   (test "enumerable-fold on a stretchy-vector works"
      (assert= (enumerable-fold + 0 (stretchy-vector 1 2 3) ) 6))

   (test "enumerable-any? even? on (stretchy-vector 1  3  5) returns #f"
      (assert-equal?  (enumerable-any? even? (stretchy-vector 1  3  5)) #f))

   (test "enumerable-every? odd? on (stretchy-vector 1  3  5) returns #t"
      (assert-equal?  (enumerable-every? odd? (stretchy-vector 1  3  5)) #t))

   (test "enumerable-skip 2 (stretchy-vector 1 2 3 4 5) yields an enumerator with the first element 3"
      (let ((enum (enumerable-skip 2 (stretchy-vector 1 2 3 4 5))))
         (assert-equal? (enumerator-current enum) 3)))

   (test "enumerable-skip 2 (stretchy-vector 1 2 3 4 5) (stretchy-vector 6 7 8 9 10) yields an enumerator with the first element (3 8)"
      (let ((enum (enumerable-skip 2 (stretchy-vector 1 2 3 4 5) (stretchy-vector 6 7 8 9 10))))
         (assert-equal? (enumerator-current enum) '(3 8))))

   (test "enumerable-skip-while odd? (stretchy-vector 1 3 5 6 7) yields an enumerator with the first element 6"
      (let ((enum (enumerable-skip-while odd? (stretchy-vector 1 3 5 6 7))))
         (assert-equal? (enumerator-current enum) 6)))

   (test "enumerable-append works on stretchy-vectors"
      (let ((enum (enumerable-append (stretchy-vector 1 2 3) (stretchy-vector 4 5 6))))
         (assert-equal? (enumerator->list enum) '(1 2 3 4 5 6))))

   (test "cloning a %stretchy-vector-enumerator works correctly"
      (let* ((enum (get-enumer (stretchy-vector 1 2 3 4 5)))
             (cln (enumerator-clone enum)))
         (assert-equal? (enumerator->list enum) '(1 2 3 4 5))
         (assert-equal? (enumerator->list cln) '(1 2 3 4 5))))

   (test "enumerable-take works on stretchy vectors"
      (assert-equal? (enumerator->list (enumerable-take 2 (stretchy-vector 1 2 3 4)))
         '(1 2)))

   (test "enumerable-take-while works on stretchy vectors"
      (assert-equal? (enumerator->list (enumerable-take-while odd? (stretchy-vector 1 3 6 5)))
         '(1 3)))

   (test "stretchy-vector-append! works"
      (let* ((sv1 (stretchy-vector 1 2 3))
             (sv2 (stretchy-vector 4 5 6))
             (app (stretchy-vector-append! sv1 sv2)))
         (assert-equal? app
            (stretchy-vector 1 2 3 4 5 6))
         (assert-eq? sv1 app)))

   (test "stretchy-vector-append works"
      (let* ((sv1 (stretchy-vector 1 2 3))
             (sv2 (stretchy-vector 4 5 6))
             (app (stretchy-vector-append sv1 sv2)))
         (assert-equal? app
            (stretchy-vector 1 2 3 4 5 6))
         (assert-false (eq? sv1 app))))

   (test "collection-slice works on stretchy-vectors"
      (assert-equal? (enumerator->list (collection-slice (stretchy-vector 1 2 3 4) (range :start 1 :end 3)))
         '(2 3)))
   
   )


