(module linked-stack-test
   (library btest
            hoard)
   (export linked-stack-tests))


(define-test-suite linked-stack-tests

   (test "linked-stack? works"
      (assert-true (linked-stack? (make-linked-stack)))
      (assert-false (linked-stack? (make-linked-queue)))
      (assert-false (linked-stack? (list 1))))

   (test "(linked-stack 1 2 3) works"
      (let ((stack (linked-stack 1 2 3)))
         (assert-true (linked-stack? stack))
         (assert-equal? (linked-stack-pop! stack) 1)
         (assert-equal? (linked-stack-pop! stack) 2)
         (assert-equal? (linked-stack-pop! stack) 3)))

   (test "linked-stack-length works"
      (let ((stack (linked-stack 1 2 3 4 5)))
         (assert-equal? (linked-stack-length stack) 5)
         (linked-stack-pop! stack)
         (assert-equal? (linked-stack-length stack) 4)
         (linked-stack-pop! stack)
         (assert-equal? (linked-stack-length stack) 3)
         (linked-stack-pop! stack)
         (assert-equal? (linked-stack-length stack) 2)
         (linked-stack-pop! stack)
         (assert-equal? (linked-stack-length stack) 1)
         (linked-stack-pop! stack)
         (assert-equal? (linked-stack-length stack) 0)
         (assert-true (linked-stack-empty? stack))
         (linked-stack-push! stack 4)
         (assert-equal? (linked-stack-length stack) 1)))

   (test "linked-stack-top works"
      (let ((stack (linked-stack 1 2 3)))
         (assert-equal? (linked-stack-top stack) 1)
         (linked-stack-pop! stack)
         (assert-equal? (linked-stack-top stack) 2)
         (linked-stack-pop! stack)
         (assert-equal? (linked-stack-top stack) 3)
         (assert-equal? (linked-stack-length stack) 1)))

   (test "linked-stack-empty? works"
      (let ((stack (make-linked-stack)))
         (assert-true (linked-stack-empty? stack))
         (linked-stack-push! stack 1)
         (assert-false (linked-stack-empty? stack))
         (linked-stack-pop! stack)
         (assert-true (linked-stack-empty? stack))))

   (test "linked-stack-push! works"
      (let ((stack (make-linked-stack)))
         (assert-true (linked-stack-empty? stack))
         (linked-stack-push! stack 1)
         (assert-equal? (linked-stack-top stack) 1)
         (assert-false (linked-stack-empty? stack))
         (linked-stack-push! stack 2)
         (assert-equal? (linked-stack-top stack) 2)))

   (test "linked-stack-pop! works"
      (let ((stack (linked-stack 1 2 3 4)))
         (assert-equal? (linked-stack-top stack) 1)
         (assert-equal? (linked-stack-pop! stack) 1)
         (assert-equal? (linked-stack-top stack) 2)
         (assert-equal? (linked-stack-pop! stack) 2)
         (assert-equal? (linked-stack-top stack) 3)))
   
   (test "linked-stack-copy works"
      (let* ((stk1 (linked-stack 1 2 3))
             (stk2 (linked-stack-copy stk1)))
         (assert-equal? (linked-stack-length stk2)
            (linked-stack-length stk1))
         (assert-equal? (enumerable-collect stk2 +list-collector+)
            (enumerable-collect stk1 +list-collector+))            
         (linked-stack-pop! stk1)
         (assert-false (= (linked-stack-length stk1)
                          (linked-stack-length stk2)))))



   ;;;; generic stack protocol tests
   (test "a linked-stack is a stack"
      (assert-true (stack? (linked-stack))))

   (test "stack-push! and stack-pop! work on linked-stacks"
      (let ((stk (linked-stack)))
         (stack-push! stk 1)
         (stack-push! stk 2)
         (assert-equal? (stack-pop! stk) 2)
         (assert-equal? (stack-pop! stk) 1)))

   (test "stack-length works on linked stacks"
      (let ((stk (linked-stack 1 2 3)))
         (assert-equal? (stack-length stk) 3)
         (stack-pop! stk)
         (assert-equal? (stack-length stk) 2)
         (stack-pop! stk)
         (assert-equal? (stack-length stk) 1)
         (stack-pop! stk)
         (assert-equal? (stack-length stk) 0)))
   
   (test "stack-empty works on linked stacks"
      (let ((stk (linked-stack)))
         (assert-true (stack-empty? stk))
         (stack-push! stk 1)
         (assert-false (stack-empty? stk))
         (stack-pop! stk)
         (assert-true (stack-empty? stk))))

   (test "stack-top works on linked stacks"
      (let ((stk (linked-stack 1 2 3)))
         (assert-equal? (stack-top stk) 1)
         (stack-pop! stk)
         (assert-equal? (stack-top stk) 2)
         (stack-pop! stk)
         (assert-equal? (stack-top stk) 3)
         (stack-pop! stk)
         (assert-true (stack-empty? stk))))

   (test "stack-fixed-capacity? returns false for a linked-stack"
      (assert-false (stack-fixed-capacity? (linked-stack))))

   (test "stack-capacity return #unspecified for a linked-stack"
      (assert-equal? (stack-capacity (linked-stack)) #unspecified))
   

   (test "stack-copy works"
      (let* ((stk1 (linked-stack 1 2 3))
             (stk2 (stack-copy stk1)))
         (assert-equal? (stack-length stk2)
            (stack-length stk1))
         (assert-equal? (enumerable-collect stk2 +list-collector+)
            (enumerable-collect stk1 +list-collector+))            
         (stack-pop! stk1)
         (assert-false (= (stack-length stk1)
                          (stack-length stk2)))))
         
     
   

   ;;;; collection tests
   
   (test "a linked-stack is a collection"
      (assert-true (collection? (linked-stack 1 2 3))))

   (test "(linked-stack 1 2 3) has length 3"
      (assert= (collection-length (linked-stack 1 2 3)) 3))

   (test "collection-contains? works on linked-stacks"
      (assert-true (collection-contains? (linked-stack 1 2 3) 1)))

   (test "collection-empty? works on an empty linked-stack"
      (assert-true (collection-empty? (linked-stack))))
   
   (test "collection-copy works"
      (let* ((stk1 (linked-stack 1 2 3))
             (stk2 (collection-copy stk1)))
         (assert-equal? (collection-length stk2)
            (collection-length stk1))
         (assert-equal? (enumerable-collect stk2 +list-collector+)
            (enumerable-collect stk1 +list-collector+))            
         (stack-pop! stk1)
         (assert-false (= (collection-length stk1)
                          (collection-length stk2)))))
   
   (test "a linked-stack is a mutable-collection"
      (assert-true (collection-mutable? (linked-stack))))


   ;;;; extendable tests
   (test "a linked-stack is extendable"
      (assert-true (collection-extendable? (linked-stack))))

   (test "collection-extend! works on linked-stack"
      (let ((stack (linked-stack)))
         (collection-extend! stack 3)
         (assert-equal? (linked-stack-top stack) 3)))


   ;;;; enumerable tests
   (test "enumerable-for-each on linked-stacks work"
      (let ((count 0))
         (enumerable-for-each  (lambda (e) (set! count (+ count 1))) (linked-stack 1 2 3))
         (assert= count 3)))

   (test "enumerable-map on linked-stacks work"
      (assert-equal? (enumerator->list (enumerable-map (lambda (e) (+ e 1)) (linked-stack 1 2 3)))
         '(2 3 4)))

    (test "enumerable-filter on linked-stacks work"
      (assert-equal? (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) (linked-stack 1 2 3)))
         '(2 3 )))

    (test "enumerable-fold on linked-stacks work"
      (assert= (enumerable-fold + 0 (linked-stack 1 2 3) ) 6))

    (test "enumerable-any? odd? on (linked-stack 1 2 3 4 5) returns #t"
       (assert-equal?  (enumerable-any? odd? (linked-stack 1 2 3 4 5)) #t))
    
    (test "enumerable-every? odd? on (linked-stack 1 2 3 4 5) returns #f"
       (assert-equal?  (enumerable-every? odd? (linked-stack 1 2 3 4 5)) #f))

    (test "enumerable-every? odd? on (linked-stack 1  3  5) returns #t"
       (assert-equal?  (enumerable-every? odd? (linked-stack 1  3  5)) #t))

    (test "enumerable-skip 2 (linked-stack 1 2 3 4 5) yields an enumerator with the first element 3"
       (let ((enum (enumerable-skip 2 (linked-stack 1 2 3 4 5))))
          (assert-equal? (enumerator-current enum) 3)))

    (test "enumerable-skip-while odd? (linked-stack 1 3 5 6 7) yields an enumerator with the first element 6"
       (let ((enum (enumerable-skip-while odd? (linked-stack 1 3 5 6 7))))
          (assert-equal? (enumerator-current enum) 6)))
    
    (test "enumerable-append works on linked-stacks"
      (let ((enumer (enumerable-append (linked-stack 1 2 3) (linked-stack 4 5 6))))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))))

    (test "enumerable-take works on linked-stacks"
      (assert-equal? (enumerator->list (enumerable-take 2 (linked-stack 1 2 3 4 5)))
         '(1 2)))

    (test "enumerable-take-while works on linked-stacks"
       (assert-equal? (enumerator->list (enumerable-take-while odd? (linked-stack 1 3 4 5)))
          '(1 3)))
   
   )

