(module range-test
(library btest
   hoard)
(export range-tests))



(define-test-suite range-tests

   (test "(range? 1) returns #f"
      (assert-false (range? 1)))

   (test "(range? (range :end 10) is #t)"
      (assert-true (range? (range :end 10))))

   (test "an empty range(i.e., (range :start 0: end: 0)) is valid"
      (range :start 0 :end 0))
   
   (test "(range end: 5 step: -1) throws an invalid argument exception"
      (assert-exception-thrown (range end: 5 step: -1)
         &invalid-argument-exception))

   (test "range end: -6 step: 1 throws an invalid argument exception"
      (assert-exception-thrown (range end: -6 step: 1)
         &invalid-argument-exception))
   
   (test "range-for-each works"
      (let ((res '()))
         (range-for-each (lambda (v)
                            (set! res (cons v res)))
            (range :end 10))
         (assert-equal? res '(9 8 7 6 5 4 3 2 1 0))))

   (test "range-map works"
      (assert-equal? (range-map (lambda (v) (+ v 1))
                        (range :end 5))
         '(1 2 3 4 5)))

   (test "(range-map (lambda (v) v) (range :end 10 :step 2)) works"
      (assert-equal? (range-map (lambda (v) v) (range :end 10 :step 2))
         '(0 2 4 6 8)))

   (test "(range-map (lambda (v) v) (range :start 20 :end 10 :step -2)) works"
      (assert-equal? (range-map (lambda (v) v) (range :start 20 :end 10 :step -2))
         '(20 18 16 14 12)))

   (test "(range-map (lambda (v) v) (range :start 20 :end 10 :step -3)) works"
         (assert-equal? (range-map (lambda (v) v) (range :start 20 :end 10 :step -3))
            '(20 17 14 11)))


   ;;;; enumerable tests

      ;;;; enumerable tests
   (test "enumerable-for-each on ranges works"
      (let ((count 0))
         (enumerable-for-each (lambda (e) (set! count (+ count 1))) (range :end 3))
            (assert= count 3)))

   (test "enumerable-map on ranges works"
         (assert-equal? (enumerator->list (enumerable-map (lambda (e) (+ e 1)) (range :start 1 :end 4))) '(2 3 4)))

   (test "enumerable-filter on range works"
      (assert-equal? (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) (range :start 1 :end 4))) '(2 3)))

   (test "enumerable-fold on a range works"
      (assert= (enumerable-fold + 0 (range :start 1 :end 4) ) 6))

   (test "enumerable-any? even? on (range :start 1 :end 6 :step 2) returns #f"
      (assert-equal?  (enumerable-any? even? (range :start 1 :end 6 :step 2)) #f))

   (test "enumerable-every? odd? on (range :start 1 :end 6 :step 2) returns #t"
      (assert-equal?  (enumerable-every? odd? (range :start 1 :end 6 :step 2)) #t))

   (test "enumerable-skip 2 (range :start 1 :end 6) yields an enumerator with the first element 3"
      (let ((enum (enumerable-skip 2 (range :start 1 :end 6))))
         (assert-equal? (enumerator-current enum) 3)))

   (test "enumerable-skip 2 (range :start 1 :end 6) (range :start 6 :end 11) yields an enumerator with the first element (3 8)"
      (let ((enum (enumerable-skip 2 (range :start 1 :end 6) (range :start 6 :end 11))))
         (assert-equal? (enumerator-current enum) '(3 8))))

   (test "enumerable-skip-while (lambda (x) (< x 6)) (range :start 1 :end 10) yields an enumerator with the first element 6"
      (let ((enum (enumerable-skip-while (lambda(x) (< x 6)) (range :start 1 :end 10 ))))
         (assert-equal? (enumerator-current enum) 6)))

   (test "enumerable-append works on ranges"
      (let ((enum (enumerable-append (range :start 1 :end 4) (range :start 4 :end 7))))
         (assert-equal? (enumerator->list enum) '(1 2 3 4 5 6))))

   (test "cloning a %range-enumerator works correctly"
      (let* ((enum (get-enumer (range :start  1 :end 6)))
             (cln (enumerator-clone enum)))
         (assert-equal? (enumerator->list enum) '(1 2 3 4 5))
         (assert-equal? (enumerator->list cln) '(1 2 3 4 5))))

   (test "enumerable-take works on stretchy vectors"
      (assert-equal? (enumerator->list (enumerable-take 2 (stretchy-vector 1 2 3 4)))
         '(1 2)))

   (test "enumerable-take-while works on ranges"
      (assert-equal? (enumerator->list (enumerable-take-while (lambda (x) (< x 6)) (range :start 1 :end 10 :step 2)))
         '(1 3 5)))

   
   )