(module contiguous-stack-test
   (library btest
            hoard)
   (export contiguous-stack-tests))



(define-test-suite contiguous-stack-tests

   (test "contiguous-stack? works"
      (assert-true (contiguous-stack? (make-contiguous-stack :capacity 3)))
      (assert-false (contiguous-stack? 0))
      (assert-false (contiguous-stack? (list 1))))

   (test "(contiguous-stack :capacity 3 1 2 3) works"
      (let ((stk (contiguous-stack :capacity 3 1 2 3)))
         (assert-equal? (contiguous-stack-pop! stk) 1)
         (assert-equal? (contiguous-stack-pop! stk) 2)
         (assert-equal? (contiguous-stack-pop! stk) 3)))

   (test "(make-contiguous-stack :capacity 3) works"
      (let ((stk (make-contiguous-stack :capacity 3)))
         (contiguous-stack-push! stk 1)
         (contiguous-stack-push! stk 2)
         (contiguous-stack-push! stk 3)
         (assert-equal? (contiguous-stack-pop! stk) 3)
         (assert-equal? (contiguous-stack-pop! stk) 2)
         (assert-equal? (contiguous-stack-pop! stk) 1)))

   (test "contiguous-stack-length works"
      (let ((stack (contiguous-stack :capacity 5 1 2 3 4 5)))
         (assert-equal? (contiguous-stack-length stack) 5)
         (contiguous-stack-pop! stack)
         (assert-equal? (contiguous-stack-length stack) 4)
         (contiguous-stack-pop! stack)
         (assert-equal? (contiguous-stack-length stack) 3)
         (contiguous-stack-pop! stack)
         (assert-equal? (contiguous-stack-length stack) 2)
         (contiguous-stack-pop! stack)
         (assert-equal? (contiguous-stack-length stack) 1)
         (contiguous-stack-pop! stack)
         (assert-equal? (contiguous-stack-length stack) 0)
         (assert-true (contiguous-stack-empty? stack))
         (contiguous-stack-push! stack 4)
         (assert-equal? (contiguous-stack-length stack) 1)))

   (test "contiguous-stack-top works"
      (let ((stack (contiguous-stack :capacity 3 1 2 3)))
         (assert-equal? (contiguous-stack-top stack) 1)
         (contiguous-stack-pop! stack)
         (assert-equal? (contiguous-stack-top stack) 2)
         (contiguous-stack-pop! stack)
         (assert-equal? (contiguous-stack-top stack) 3)
         (assert-equal? (contiguous-stack-length stack) 1)))

   (test "contiguous-stack-empty? works"
      (let ((stack (make-contiguous-stack :capacity 3)))
         (assert-true (contiguous-stack-empty? stack))
         (contiguous-stack-push! stack 1)
         (assert-false (contiguous-stack-empty? stack))
         (contiguous-stack-pop! stack)
         (assert-true (contiguous-stack-empty? stack))))

   (test "contiguous-stack-push! works"
      (let ((stack (make-contiguous-stack :capacity 3)))
         (assert-true (contiguous-stack-empty? stack))
         (contiguous-stack-push! stack 1)
         (assert-equal? (contiguous-stack-top stack) 1)
         (assert-false (contiguous-stack-empty? stack))
         (contiguous-stack-push! stack 2)
         (assert-equal? (contiguous-stack-top stack) 2)))

   (test "contiguous-stack-pop! works"
      (let ((stack (contiguous-stack :capacity 4 1 2 3 4)))
         (assert-equal? (contiguous-stack-top stack) 1)
         (assert-equal? (contiguous-stack-pop! stack) 1)
         (assert-equal? (contiguous-stack-top stack) 2)
         (assert-equal? (contiguous-stack-pop! stack) 2)
         (assert-equal? (contiguous-stack-top stack) 3)))


   (test "contiguous-stack-copy works"
      (let* ((stk1 (contiguous-stack :capacity 3 1 2 3))
             (stk2 (contiguous-stack-copy stk1)))
         (assert-equal? (contiguous-stack-length stk2)
            (contiguous-stack-length stk1))
         (assert-equal? (enumerable-collect stk2 +list-collector+)
            (enumerable-collect stk1 +list-collector+))            
         (contiguous-stack-pop! stk1)
         (assert-false (= (contiguous-stack-length stk1)
                          (contiguous-stack-length stk2)))))
   
   ;;;; generic stack protocol tests
   (test "a contiguous-stack is a stack"
      (assert-true (stack? (contiguous-stack :capacity 3))))

   (test "stack-push! and stack-pop! work on contiguous-stacks"
      (let ((stk (contiguous-stack :capacity 3)))
         (stack-push! stk 1)
         (stack-push! stk 2)
         (assert-equal? (stack-pop! stk) 2)
         (assert-equal? (stack-pop! stk) 1)))

   (test "stack-length works on contiguous stacks"
      (let ((stk (contiguous-stack :capacity 3 1 2 3)))
         (assert-equal? (stack-length stk) 3)
         (stack-pop! stk)
         (assert-equal? (stack-length stk) 2)
         (stack-pop! stk)
         (assert-equal? (stack-length stk) 1)
         (stack-pop! stk)
         (assert-equal? (stack-length stk) 0)))
   
   (test "stack-empty works on contiguous stacks"
      (let ((stk (contiguous-stack :capacity 3)))
         (assert-true (stack-empty? stk))
         (stack-push! stk 1)
         (assert-false (stack-empty? stk))
         (stack-pop! stk)
         (assert-true (stack-empty? stk))))

   (test "stack-top works on contiguous stacks"
      (let ((stk (contiguous-stack :capacity 3 1 2 3)))
         (assert-equal? (stack-top stk) 1)
         (stack-pop! stk)
         (assert-equal? (stack-top stk) 2)
         (stack-pop! stk)
         (assert-equal? (stack-top stk) 3)
         (stack-pop! stk)
         (assert-true (stack-empty? stk))))

   (test "stack-fixed-capacity? returns true for a contiguous-stack"
      (assert-true (stack-fixed-capacity? (contiguous-stack :capacity 3))))

   (test "stack-capacity returns 3 for a contigous-stack of capacity 3"
      (assert-equal? (stack-capacity (contiguous-stack :capacity 3)) 3))

   (test "stack-copy works"
      (let* ((stk1 (contiguous-stack :capacity 3 1 2 3))
             (stk2 (stack-copy stk1)))
         (assert-equal? (stack-length stk2)
            (stack-length stk1))
         (assert-equal? (enumerable-collect stk2 +list-collector+)
            (enumerable-collect stk1 +list-collector+))            
         (stack-pop! stk1)
         (assert-false (= (stack-length stk1)
                          (stack-length stk2)))))


   ;;;; contiguous-stack-enumerator tests
   (test "contiguous-stack-enumerator throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((enumer (collection-enumerator (contiguous-stack :capacity 3))))
         (assert-exception-thrown (enumerator-current enumer)
            &error)))
   
   (test "contiguous-stack-enumerator for immediately returns false on empty lst"
       (let ((enumer (collection-enumerator (contiguous-stack :capacity 3))))
         (assert-false (enumerator-move-next! enumer))))

   (test "contiguous-stack-enumerator for '(contiguous-stack :capacity 1 2 3) return 3 items"
      (let ((enumer (collection-enumerator (contiguous-stack :capacity 3 1 2 3))))
         (assert-equal? (let loop ((cont (enumerator-move-next! enumer))
                                   (res '()))
                           (if cont
                               (let ((t (enumerator-current enumer))) 
                                  (loop (enumerator-move-next! enumer)
                                     (cons t res)))
                               res)) (list 3 2 1))))

   (test "cloning enumerators works correctly on contiguous-stack-enumerators"
      (let* ((enumer (get-enumer (contiguous-stack :capacity 4 1 2 3 4)))
             (cln (enumerator-copy enumer)))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4))
         (assert-equal? (enumerator->list cln) '(1 2 3 4))))


   ;;;; collection tests
   
   (test "a contiguous-stack is a collection"
      (assert-true (collection? (contiguous-stack :capacity 3 1 2 3))))

   (test "(contiguous-stack :capacity 3 1 2 3) has length 3"
      (assert= (collection-length (contiguous-stack :capacity 3 1 2 3)) 3))

   (test "collection-contains? works on contiguous-stacks"
      (assert-true (collection-contains? (contiguous-stack :capacity 3 1 2 3) 1)))

   (test "collection-empty? works on an empty contiguous-stack"
      (assert-true (collection-empty? (contiguous-stack :capacity 3))))

   (test "collection-copy works"
      (let* ((stk1 (contiguous-stack :capacity 3 1 2 3))
             (stk2 (collection-copy stk1)))
         (assert-equal? (collection-length stk2)
            (collection-length stk1))
         (assert-equal? (enumerable-collect stk2 +list-collector+)
            (enumerable-collect stk1 +list-collector+))            
         (stack-pop! stk1)
         (assert-false (= (collection-length stk1)
                          (collection-length stk2)))))

   (test "a contiguous-stack is a mutable-collection"
      (assert-true (collection-mutable? (contiguous-stack :capacity 3))))

   ;;;; enumerable tests
   (test "enumerable-for-each on contiguous-stacks work"
      (let ((count 0))
         (enumerable-for-each  (lambda (e) (set! count (+ count 1))) (contiguous-stack :capacity 3 1 2 3))
         (assert= count 3)))

   (test "enumerable-map on contiguous-stacks work"
      (assert-equal? (enumerator->list (enumerable-map (lambda (e) (+ e 1)) (contiguous-stack :capacity 3 1 2 3)))
         '(2 3 4)))

    (test "enumerable-filter on contiguous-stacks work"
      (assert-equal? (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) (contiguous-stack :capacity 3 1 2 3)))
         '(2 3 )))

    (test "enumerable-fold on contiguous-stack work"
      (assert= (enumerable-fold + 0 (contiguous-stack :capacity 3 1 2 3) ) 6))

    (test "enumerable-any? odd? on (contiguous-stack :capacity 5 1 2 3 4 5) returns #t"
       (assert-equal?  (enumerable-any? odd? (contiguous-stack :capacity 5 1 2 3 4 5)) #t))
    
    (test "enumerable-every? odd? on (contiguous-stack :capacity 5 1 2 3 4 5) returns #f"
       (assert-equal?  (enumerable-every? odd? (contiguous-stack :capacity 5 1 2 3 4 5)) #f))

    (test "enumerable-every? odd? on (contiguous-stack :capacity 3 1  3  5) returns #t"
       (assert-equal?  (enumerable-every? odd? (contiguous-stack :capacity 3 1  3  5)) #t))

    (test "enumerable-skip 2 (contiguous-stack :capacity 5 1 2 3 4 5) yields an enumerator with the first element 3"
       (let ((enum (enumerable-skip 2 (contiguous-stack :capacity 5 1 2 3 4 5))))
          (assert-equal? (enumerator-current enum) 3)))

    (test "enumerable-skip-while odd? (contiguous-stack :capacity 5 1 3 5 6 7) yields an enumerator with the first element 6"
       (let ((enum (enumerable-skip-while odd? (contiguous-stack :capacity 5 1 3 5 6 7))))
          (assert-equal? (enumerator-current enum) 6)))
    
    (test "enumerable-append works on contiguous-stacks"
      (let ((enumer (enumerable-append (contiguous-stack :capacity 3  1 2 3) (contiguous-stack :capacity 3 4 5 6))))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))))

    (test "enumerable-take works on contiguous-stacks"
      (assert-equal? (enumerator->list (enumerable-take 2 (contiguous-stack :capacity 5 1 2 3 4 5)))
         '(1 2)))

    (test "enumerable-take-while works on contiguous-stacks"
       (assert-equal? (enumerator->list (enumerable-take-while odd? (contiguous-stack :capacity 5 1 3 4 5)))
          '(1 3)))
   )


