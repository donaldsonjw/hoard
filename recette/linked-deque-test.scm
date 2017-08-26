(module linked-deque-test
   (library btest
            hoard)
   (export linked-deque-tests))



(define-test-suite linked-deque-tests

   (test "linked-deque? works"
      (assert-true (linked-deque? (make-linked-deque)))
      (assert-true (linked-deque? (linked-deque 1 2 3)))
      (assert-false (linked-deque? 1))
      (assert-false (linked-deque? (list 1 2 3))))

   (test "linked-deque-empty? works"
      (assert-true (linked-deque-empty? (linked-deque)))
      (assert-true (linked-deque-empty? (make-linked-deque)))
      (assert-false (linked-deque-empty? (linked-deque 1 2 3))))

   (test "linked-deque-enqueue! works"
      (let ((deque (make-linked-deque)))
         (linked-deque-enqueue! deque 1)
         (linked-deque-enqueue! deque 2)
         (linked-deque-enqueue! deque 3)
         (assert-equal? (linked-deque-dequeue! deque) 1)
         (assert-equal? (linked-deque-dequeue-back! deque) 3)
         (assert-equal? (linked-deque-length deque) 1)
         (assert-equal? (linked-deque-dequeue! deque) 2)
         (assert-true (linked-deque-empty? deque))))

   (test "(linked-deque 1 2 3) works"
      (let ((q (linked-deque 1 2 3)))
         (assert-equal? (linked-deque-dequeue! q) 1)
         (assert-equal? (linked-deque-dequeue! q) 2)
         (assert-equal? (linked-deque-dequeue! q) 3)
         (assert-true (linked-deque-empty? q))))

   (test "linked-deque-enqueue-front! works"
      (let ((deque (make-linked-deque)))
         (linked-deque-enqueue-front! deque 1)
         (linked-deque-enqueue-front! deque 2)
         (linked-deque-enqueue-front! deque 3)
         (assert-equal? (linked-deque-dequeue! deque) 3)
         (assert-equal? (linked-deque-dequeue-back! deque) 1)
         (assert-equal? (linked-deque-length deque) 1)
         (assert-equal? (linked-deque-dequeue-back! deque) 2)
         (assert-true (linked-deque-empty? deque))))

   (test "linked-deque-dequeue! works"
      (let ((deque (linked-deque 1 2 3)))
         (assert-equal? (linked-deque-dequeue! deque) 1)
         (assert-equal? (linked-deque-dequeue! deque) 2)
         (assert-equal? (linked-deque-dequeue! deque) 3)))

   (test "linked-deque-dequeue-back! works"
      (let ((deque (linked-deque 1 2 3)))
         (assert-equal? (linked-deque-dequeue-back! deque) 3)
         (assert-equal? (linked-deque-dequeue-back! deque) 2)
         (assert-equal? (linked-deque-dequeue-back! deque) 1)))


   (test "linked-deque-first works"
      (let ((deque (linked-deque 1 2 3)))
         (assert-equal? (linked-deque-first deque) 1)
         (linked-deque-dequeue! deque)
         (assert-equal? (linked-deque-first deque) 2)
         (linked-deque-dequeue! deque)
         (assert-equal? (linked-deque-first deque) 3)
         (linked-deque-dequeue! deque)
         (assert-true (linked-deque-empty? deque))))

   (test "linked-deque-last works"
      (let ((deque (linked-deque 1 2 3)))
         (assert-equal? (linked-deque-last deque) 3)
         (linked-deque-dequeue-back! deque)
         (assert-equal? (linked-deque-last deque) 2)
         (linked-deque-dequeue-back! deque)
         (assert-equal? (linked-deque-last deque) 1)
         (linked-deque-dequeue-back! deque)
         (assert-true (linked-deque-empty? deque))))

   (test "dequeue or deque-back from an empty deque throws an exception"
      (let ((deque (linked-deque)))
         (assert-exception-thrown (linked-deque-dequeue! deque) &invalid-state-exception)
         (assert-exception-thrown (linked-deque-dequeue-back! deque) &invalid-state-exception)))


   (test "linked-deque-length works"
      (let ((deque (linked-deque 1 2 3 4)))
         (assert-equal? (linked-deque-length deque) 4)
         (linked-deque-dequeue-back! deque)
         (assert-equal? (linked-deque-length deque) 3)
         (linked-deque-dequeue! deque)
         (assert-equal? (linked-deque-length deque) 2)
         (linked-deque-dequeue-back! deque)
         (assert-equal? (linked-deque-length deque) 1)
         (assert-equal? (linked-deque-first deque) 2)))


   (test "linked-deque-first or linked-deque-last on an empty deque throws an exception"
      (let ((deque (linked-deque)))
         (assert-exception-thrown (linked-deque-first deque) &invalid-state-exception)
         (assert-exception-thrown (linked-deque-last deque) &invalid-state-exception)))
   
   (test "linked-deque-copy works"
      (let* ((deque1 (linked-deque 1 2 3 4))
             (deque2 (linked-deque-copy deque1)))
         (assert-false (eq? deque1 deque2))
         (assert-true  (= (linked-deque-length deque1) (linked-deque-length deque2)))
         (assert-equal? (linked-deque-first deque1)
            (linked-deque-first deque2))
         (assert-equal? (linked-deque-last deque1)
            (linked-deque-last deque2))
         (linked-deque-dequeue! deque1)
         (assert-false (= (linked-deque-length deque1)
                          (linked-deque-length deque2)))))

   ;;;; linked-deque-enumerator tests
   (test "linked-deque-enumerator throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((enumer (collection-enumerator (linked-deque))))
         (assert-exception-thrown (enumerator-current enumer)
            &error)))
   
   (test "linked-deque-enumerator for immediately returns false on empty lst"
       (let ((enumer (collection-enumerator (linked-deque))))
         (assert-false (enumerator-move-next! enumer))))

   (test "linked-deque-enumerator for (linked-deque 1 2 3) returns 3 items"
      (let ((enumer (collection-enumerator (linked-deque 1 2 3))))
         (assert-equal? (let loop ((cont (enumerator-move-next! enumer))
                                   (res '()))
                           (if cont
                               (let ((t (enumerator-current enumer))) 
                                  (loop (enumerator-move-next! enumer)
                                     (cons t res)))
                               res)) (list 3 2 1))))

   ;;;; deque protocol
   (test "linked-deque is a deque"
      (assert-true (deque? (linked-deque)))
      (assert-true (deque? (make-linked-deque)))
      (assert-false (deque? (list))))

   (test "deque-empty works with a linked-deque"
      (let ((deque (linked-deque)))
         (assert-true (deque-empty? deque))
         (deque-enqueue! deque 3)
         (assert-false (deque-empty? deque))))

   (test "deque-first works with a linked-deque"
      (let ((deque (linked-deque 1 2 3 4)))
         (assert-equal? (deque-first deque) 1)
         (deque-dequeue! deque)
         (assert-equal? (deque-first deque) 2)
         (deque-dequeue! deque)
         (assert-equal? (deque-first deque) 3)
         (deque-dequeue! deque)
         (assert-equal? (deque-first deque) 4)))

   (test "deque-last works with a linked-deque"
      (let ((deque (linked-deque 1 2 3 4)))
         (assert-equal? (deque-last deque) 4)
         (deque-dequeue-back! deque)
         (assert-equal? (deque-last deque) 3)
         (deque-dequeue-back! deque)
         (assert-equal? (deque-last deque) 2)
         (deque-dequeue-back! deque)
         (assert-equal? (deque-last deque) 1)))

   (test "deque-enqueue! works with linked-deque"
      (let ((deque (linked-deque)))
         (deque-enqueue! deque 1)
         (assert-equal? (deque-last deque) 1)
         (deque-enqueue! deque 2)
         (assert-equal? (deque-last deque) 2)
         (deque-enqueue! deque 3)
         (assert-equal? (deque-last deque) 3)))

   (test "deque-enqueue-front! works with linked-deque"
      (let ((deque (linked-deque)))
         (deque-enqueue-front! deque 1)
         (assert-equal? (deque-first deque) 1)
         (deque-enqueue-front! deque 2)
         (assert-equal? (deque-first deque) 2)
         (deque-enqueue-front! deque 3)
         (assert-equal? (deque-first deque) 3)))

   (test "deque-dequeue! works with linked-deque"
      (let ((deque (linked-deque 1 2 3 4 5)))
         (assert-equal? (deque-dequeue! deque) 1)
         (assert-equal? (deque-dequeue! deque) 2)
         (assert-equal? (deque-dequeue! deque) 3)
         (assert-equal? (deque-dequeue! deque) 4)
         (deque-enqueue-front! deque -1)
         (assert-equal? (deque-dequeue! deque) -1)))
   
   (test "deque-dequeue-back! works with linked-deque"
      (let ((deque (linked-deque 1 2 3 4 5)))
         (assert-equal? (deque-dequeue-back! deque) 5)
         (assert-equal? (deque-dequeue-back! deque) 4)
         (assert-equal? (deque-dequeue-back! deque) 3)
         (assert-equal? (deque-dequeue-back! deque) 2)
         (deque-enqueue! deque -1)
         (assert-equal? (deque-dequeue-back! deque) -1)))

   (test "linked-deque does not have a fixed capacity"
      (assert-false (deque-fixed-capacity? (linked-deque))))

   (test "deque-copy works onf linked-deque"
      (let* ((deque1 (linked-deque 1 2 3 4))
             (deque2 (deque-copy deque1)))
         (assert-false (eq? deque1 deque2))
         (assert-true  (= (deque-length deque1) (deque-length deque2)))
         (assert-equal? (deque-first deque1)
            (deque-first deque2))
         (assert-equal? (deque-last deque1)
            (deque-last deque2))
         (deque-dequeue! deque1)
         (assert-false (= (deque-length deque1)
                          (deque-length deque2)))))

   
   ;;;; queue protocol 
   (test "a linked-deque is a queue?"
      (assert-true (queue? (linked-deque))))
   
   (test "queue-first works"
      (let ((q (linked-deque 1 2 3)))
         (assert-equal? (queue-first q) 1)
         (assert-equal? (queue-first q) 1)
         (queue-dequeue! q)
         (assert-equal? (queue-first q) 2)
         (assert-equal? (queue-length q) 2)))
   
   (test "queue-length and queue-dequeue! works"
      (let ((q (linked-deque 1 2 3 4 5)))
         (assert-equal? (queue-length q) 5)
         (assert-equal? (queue-dequeue! q) 1)
         (assert-equal? (queue-length q) 4)
         (assert-equal? (queue-dequeue! q) 2)
         (assert-equal? (queue-length q) 3)
         (assert-equal? (queue-dequeue! q) 3)
         (assert-equal? (queue-length q) 2)
         (assert-equal? (queue-dequeue! q) 4)
         (assert-equal? (queue-length q) 1)
         (assert-equal? (queue-dequeue! q) 5)
         (assert-equal? (queue-length q) 0)
         (assert-true (queue-empty? q))
         (queue-enqueue! q 4)
         (assert-equal? (queue-length q) 1)))


   (test "enqueueing 1 2 3 into a deque works"
      (let ((q (make-linked-deque)))
         (queue-enqueue! q 1)
         (queue-enqueue! q 2)
         (queue-enqueue! q 3)
         (assert-equal? (queue-length q) 3)
         (assert-equal? (queue-first q) 1)))

   (test "a linked-deque does not have fixed capacity"
      (let ((q (make-linked-deque)))
         (assert-false (queue-fixed-capacity? q))))

   (test "a linked-deque returns #unspecified for capacity"
      (let ((q (make-linked-deque)))
        (assert-equal? (queue-capacity q) #unspecified)))

   (test "queue-copy works on a linked-deque"
         (let* ((q1 (linked-deque 1 2 3 4))
                (q2 (queue-copy q1)))
           (assert-equal? (queue-length q1)
                         (queue-length q2))
           (assert-equal? (queue-first q1)
                         (queue-first q2))
           (queue-dequeue! q1)
           (assert-false (equal? (queue-length q1)
                                 (queue-length q2)))
           (queue-enqueue! q2 6)
           (assert-false (equal? (queue-first q1)
                                 (queue-first q2)))))
   
   ;;;; collection tests

   (test "a linked-deque is a collection"
      (assert-true (collection? (linked-deque 1 2 3))))

   (test "(linked-deque 1 2 3) has length 3"
      (assert= (collection-length (linked-deque 1 2 3)) 3))

   (test "collection-contains? works on linked-deques"
      (assert-true (collection-contains? (linked-deque 1 2 3) 1)))

   (test "collection-empty? works on an empty linked-deque"
      (assert-true (collection-empty? (linked-deque))))


   (test "a linked-deque is a mutable-collection"
         (assert-true (collection-mutable? (linked-deque))))

   (test "collection-copy works on a linked-deque"
      (let* ((q1 (linked-deque 1 2 3 4))
             (q2 (collection-copy q1)))
         (assert-equal? (collection-length q1)
            (collection-length q2))
         
         (assert-equal? (deque-first q1)
            (deque-first q2))
         (deque-dequeue! q1)
         (assert-false (equal? (collection-length q1)
                          (collection-length q2)))
         (deque-enqueue! q2 6)
         (assert-false (equal? (deque-first q1)
                          (deque-first q2)))
         )
      )


   ;;;; extendable tests
   (test "a linked-deque is extendable"
      (assert-true (collection-extendable? (linked-deque))))

   (test "collection-extend! works on linked-deque"
      (let ((deque (linked-deque)))
         (collection-extend! deque 3)
         (assert-equal? (linked-deque-first deque) 3)))
 

   ;;;; enumerable tests
   (test "enumerable-for-each on linked-deques work"
      (let ((count 0))
         (enumerable-for-each  (lambda (e) (set! count (+ count 1))) (linked-deque 1 2 3))
         (assert= count 3)))

   (test "enumerable-map on linked-deques work"
      (assert-equal? (enumerator->list (enumerable-map (lambda (e) (+ e 1)) (linked-deque 1 2 3)))
         '(2 3 4)))

    (test "enumerable-filter on linked-deques work"
      (assert-equal? (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) (linked-deque 1 2 3)))
         '(2 3 )))

    (test "enumerable-fold on linked-deques work"
      (assert= (enumerable-fold + 0 (linked-deque 1 2 3) ) 6))

    (test "enumerable-any? odd? on (linked-deque 1 2 3 4 5) returns #t"
       (assert-equal?  (enumerable-any? odd? (linked-deque 1 2 3 4 5)) #t))
    
    (test "enumerable-every? odd? on (linked-deque 1 2 3 4 5) returns #f"
       (assert-equal?  (enumerable-every? odd? (linked-deque 1 2 3 4 5)) #f))

    (test "enumerable-every? odd? on (linked-deque 1  3  5) returns #t"
       (assert-equal?  (enumerable-every? odd? (linked-deque 1  3  5)) #t))

    (test "enumerable-skip 2 (linked-deque 1 2 3 4 5) yields an enumerator with the first element 3"
       (let ((enum (enumerable-skip 2 (linked-deque 1 2 3 4 5))))
          (assert-equal? (enumerator-current enum) 3)))

    (test "enumerable-skip-while odd? (linked-deque 1 3 5 6 7) yields an enumerator with the first element 6"
       (let ((enum (enumerable-skip-while odd? (linked-deque 1 3 5 6 7))))
          (assert-equal? (enumerator-current enum) 6)))
    
    (test "enumerable-append works on linked-deques"
      (let ((enumer (enumerable-append (linked-deque 1 2 3) (linked-deque 4 5 6))))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))))

    (test "enumerable-take works on linked-deques"
      (assert-equal? (enumerator->list (enumerable-take 2 (linked-deque 1 2 3 4 5)))
         '(1 2)))

    (test "enumerable-take-while works on linked-deques"
       (assert-equal? (enumerator->list (enumerable-take-while odd? (linked-deque 1 3 4 5)))
          '(1 3)))

   )