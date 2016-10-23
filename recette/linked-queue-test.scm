(module linked-queue-test
   (library btest
            hoard)
   (export linked-queue-tests))



(define-test-suite linked-queue-tests

   (test "linked-queue? works"
      (assert-true (linked-queue? (make-linked-queue)))
      (assert-false (linked-queue? 0))
      (assert-false (linked-queue? (list 1))))


   (test "(linked-queue 1 2 3) works"
      (let ((q (linked-queue 1 2 3)))
         (assert-equal? (linked-queue-dequeue! q) 1)
         (assert-equal? (linked-queue-dequeue! q) 2)
         (assert-equal? (linked-queue-dequeue! q) 3)
         (assert-true (linked-queue-empty? q))))


   (test "linked-queue-length works"
      (let ((q (linked-queue 1 2 3 4 5)))
         (assert-equal? (linked-queue-length q) 5)
         (linked-queue-dequeue! q)
         (assert-equal? (linked-queue-length q) 4)
         (linked-queue-dequeue! q)
         (assert-equal? (linked-queue-length q) 3)
         (linked-queue-dequeue! q)
         (assert-equal? (linked-queue-length q) 2)
         (linked-queue-dequeue! q)
         (assert-equal? (linked-queue-length q) 1)
         (linked-queue-dequeue! q)
         (assert-equal? (linked-queue-length q) 0)
         (assert-true (linked-queue-empty? q))
         (linked-queue-enqueue! q 4)
         (assert-equal? (linked-queue-length q) 1)))

   (test "linked-queue-first works"
      (let ((q (linked-queue 1 2 3)))
         (assert-equal? (linked-queue-first q) 1)
         (assert-equal? (linked-queue-first q) 1)
         (linked-queue-dequeue! q)
         (assert-equal? (linked-queue-first q) 2)
         (assert-equal? (linked-queue-length q) 2)))

   (test "an initally created linked queue has size 0"
      (assert-equal? (linked-queue-length (make-linked-queue)) 0))

   (test "enqueueing 1 2 3 into a queue results in queue of length 3"
      (let ((q (make-linked-queue)))
         (linked-queue-enqueue! q 1)
         (linked-queue-enqueue! q 2)
         (linked-queue-enqueue! q 3)
         (assert-equal? (linked-queue-length q) 3)))

   (test "dequeueing from an empty queue throws an invalid state exception"
      (let ((q (linked-queue)))
         (assert-exception-thrown 
            (linked-queue-dequeue! q)
            &invalid-state-exception)))

   (test "pulling the first item from an empty queue throws an invalid state exception"
      (let ((q (linked-queue)))
         (assert-exception-thrown 
            (linked-queue-first q)
            &invalid-state-exception)))

   ;;;; queue protocol

   (test "a linked-queue is a queue?"
      (assert-true (queue? (linked-queue))))
   
   (test "queue-first works"
      (let ((q (linked-queue 1 2 3)))
         (assert-equal? (queue-first q) 1)
         (assert-equal? (queue-first q) 1)
         (queue-dequeue! q)
         (assert-equal? (queue-first q) 2)
         (assert-equal? (queue-length q) 2)))
   
   (test "queue-length and queue-dequeue! works"
      (let ((q (linked-queue 1 2 3 4 5)))
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


   (test "enqueueing 1 2 3 into a queue works"
      (let ((q (make-linked-queue)))
         (queue-enqueue! q 1)
         (queue-enqueue! q 2)
         (queue-enqueue! q 3)
         (assert-equal? (queue-length q) 3)
         (assert-equal? (queue-first q) 1)))

   ;;;; collection tests

   (test "a linked-queue is a collection"
      (assert-true (collection? (linked-queue 1 2 3))))

   (test "(linked-queue 1 2 3) has length 3"
      (assert= (collection-length (linked-queue 1 2 3)) 3))

   (test "collection-contains? works on linked-queues"
      (assert-true (collection-contains? (linked-queue 1 2 3) 1)))

   (test "collection-empty? works on an empty linked-queue"
      (assert-true (collection-empty? (linked-queue))))


   (test "a linked-queue is a mutable-collection"
      (assert-true (collection-mutable? (linked-queue))))
   

   ;;;; enumerable tests
   (test "enumerable-for-each on linked-queues work"
      (let ((count 0))
         (enumerable-for-each  (lambda (e) (set! count (+ count 1))) (linked-queue 1 2 3))
         (assert= count 3)))

   (test "enumerable-map on linked-queues work"
      (assert-equal? (enumerator->list (enumerable-map (lambda (e) (+ e 1)) (linked-queue 1 2 3)))
         '(2 3 4)))

    (test "enumerable-filter on linked-queues work"
      (assert-equal? (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) (linked-queue 1 2 3)))
         '(2 3 )))

    (test "enumerable-fold on linked-queues work"
      (assert= (enumerable-fold + 0 (linked-queue 1 2 3) ) 6))

    (test "enumerable-any? odd? on (linked-queue 1 2 3 4 5) returns #t"
       (assert-equal?  (enumerable-any? odd? (linked-queue 1 2 3 4 5)) #t))
    
    (test "enumerable-every? odd? on (linked-queue 1 2 3 4 5) returns #f"
       (assert-equal?  (enumerable-every? odd? (linked-queue 1 2 3 4 5)) #f))

    (test "enumerable-every? odd? on (linked-queue 1  3  5) returns #t"
       (assert-equal?  (enumerable-every? odd? (linked-queue 1  3  5)) #t))

    (test "enumerable-skip 2 (linked-queue 1 2 3 4 5) yields an enumerator with the first element 3"
       (let ((enum (enumerable-skip 2 (linked-queue 1 2 3 4 5))))
          (assert-equal? (enumerator-current enum) 3)))

    (test "enumerable-skip-while odd? (linked-queue 1 3 5 6 7) yields an enumerator with the first element 6"
       (let ((enum (enumerable-skip-while odd? (linked-queue 1 3 5 6 7))))
          (assert-equal? (enumerator-current enum) 6)))
    
    (test "enumerable-append works on linked-queues"
      (let ((enumer (enumerable-append (linked-queue 1 2 3) (linked-queue 4 5 6))))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))))

    (test "enumerable-take works on linked-queues"
      (assert-equal? (enumerator->list (enumerable-take 2 (linked-queue 1 2 3 4 5)))
         '(1 2)))

    (test "enumerable-take-while works on linked-queues"
       (assert-equal? (enumerator->list (enumerable-take-while odd? (linked-queue 1 3 4 5)))
          '(1 3))))


   