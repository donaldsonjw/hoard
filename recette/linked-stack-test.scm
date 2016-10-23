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
   
   ;;;; collection tests
   
   (test "a linked-stack is a collection"
      (assert-true (collection? (linked-stack 1 2 3))))

   (test "(linked-stack 1 2 3) has length 3"
      (assert= (collection-length (linked-stack 1 2 3)) 3))

   (test "collection-contains? works on linked-stacks"
      (assert-true (collection-contains? (linked-stack 1 2 3) 1)))

   (test "collection-empty? works on an empty linked-stack"
      (assert-true (collection-empty? (linked-stack))))


   (test "a linked-stack is a mutable-collection"
      (assert-true (collection-mutable? (linked-stack))))
   

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

