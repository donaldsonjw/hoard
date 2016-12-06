(module contiguous-queue-test
   (library btest
            hoard)
   (export contiguous-queue-tests))



(define-test-suite contiguous-queue-tests

   (test "contiguous-queue? works"
      (assert-true (contiguous-queue? (contiguous-queue :capacity 0)))
      (assert-true (contiguous-queue? (make-contiguous-queue :capacity 1)))
      (assert-false (contiguous-queue? 3))
      (assert-false (contiguous-queue? (vector))))
 
   (test "contiguous-queue-empty? works"
      (assert-true (contiguous-queue-empty? (contiguous-queue :capacity 4)))
      (assert-false (contiguous-queue-empty? (contiguous-queue :capacity 4 0 1 2 3))))

   (test "contiguouse-queue-enqueue! works"
      (let ((q (contiguous-queue :capacity 4)))
         (contiguous-queue-enqueue! q 1)
         (contiguous-queue-enqueue! q 2)
         (assert-equal? (contiguous-queue-dequeue! q) 1)
         (assert-equal? (contiguous-queue-dequeue! q) 2)
         (assert-true (contiguous-queue-empty? q))))
   
   (test "inqueueing into a full queue throws an exception"
      (let ((q (contiguous-queue :capacity 3  1 2 3)))
         (assert-exception-thrown (contiguous-queue-enqueue! q 4)
            &invalid-state-exception)))

   (test "contiguous-queue-dequeue! works"
      (let ((q (contiguous-queue :capacity 3 1 2 3)))
         (assert-equal? (contiguous-queue-dequeue! q)
            1)
         (assert-equal? (contiguous-queue-dequeue! q)
            2)
         (assert-equal? (contiguous-queue-dequeue! q)
            3)
         (assert-true (contiguous-queue-empty? q))))

   (test "contiguous-queue-first works"
      (let ((q (contiguous-queue :capacity 3 1 2 3)))
         
         (assert-equal? (contiguous-queue-first q)
            1)
         (contiguous-queue-dequeue! q)
         (assert-equal? (contiguous-queue-first q)
            2)
         (contiguous-queue-dequeue! q)
         (assert-equal? (contiguous-queue-first q)
            3)
         (contiguous-queue-dequeue! q)
         (assert-true (contiguous-queue-empty? q))))


   (test "dequeueing from an empty queue throws an exception"
      (let ((q (contiguous-queue :capacity 4)))
         (assert-exception-thrown (contiguous-queue-dequeue! q)
            &invalid-state-exception)))

   (test "contiguous-queue-length works"
      (let ((q (contiguous-queue :capacity 3 1 2 3)))
         (assert-equal? (contiguous-queue-length q) 3)
         (contiguous-queue-dequeue! q)
         (assert-equal? (contiguous-queue-length q) 2)
         (contiguous-queue-dequeue! q)
         (assert-equal? (contiguous-queue-length q) 1)))
   
   (test "pulling the first item from an empty queue throws an invalid state exception"
      (let ((q (contiguous-queue :capacity 3)))
         (assert-exception-thrown 
            (contiguous-queue-first q)
            &invalid-state-exception)))

   ;;;; queue protocol

   (test "a contiguous-queue is a queue?"
      (assert-true (queue? (contiguous-queue :capacity 3))))
   
   (test "queue-first works"
      (let ((q (contiguous-queue :capacity 3 1 2 3)))
         (assert-equal? (queue-first q) 1)
         (assert-equal? (queue-first q) 1)
         (queue-dequeue! q)
         (assert-equal? (queue-first q) 2)
         (assert-equal? (queue-length q) 2)))
   
   (test "queue-length and queue-dequeue! works"
      (let ((q (contiguous-queue :capacity 5 1 2 3 4 5)))
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
      (let ((q (make-contiguous-queue :capacity 3)))
         (queue-enqueue! q 1)
         (queue-enqueue! q 2)
         (queue-enqueue! q 3)
         (assert-equal? (queue-length q) 3)
         (assert-equal? (queue-first q) 1)))

   (test "a contiguous-queue does have fixed capacity"
      (let ((q (make-contiguous-queue :capacity 3)))
         (assert-true (queue-fixed-capacity? q))))

   (test "a contiguous-queue has  capacity"
      (let ((q (make-contiguous-queue :capacity 4)))
         (assert-equal? (queue-capacity q) 4)))

   ;;;; contiguous-queue-enumerator tests
   (test "contiguous-queue-enumerator throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((enumer (collection-enumerator (contiguous-queue :capacity 3))))
         (assert-exception-thrown (enumerator-current enumer)
            &error)))
   
   (test "contiguous-enumerator for immediately returns false on empty lst"
       (let ((enumer (collection-enumerator (contiguous-queue :capacity 3))))
         (assert-false (enumerator-move-next! enumer))))

   (test "contiguous-enumerator for '(contiguous-queue :capacity 1 2 3) return 3 items"
      (let ((enumer (collection-enumerator (contiguous-queue :capacity 3 1 2 3))))
         (assert-equal? (let loop ((cont (enumerator-move-next! enumer))
                                   (res '()))
                           (if cont
                               (let ((t (enumerator-current enumer))) 
                                  (loop (enumerator-move-next! enumer)
                                     (cons t res)))
                               res)) (list 3 2 1))))

   (test "cloning enumerators works correctly on contiguous-queue-enumerators"
      (let* ((enumer (get-enumer (contiguous-queue :capacity 4 1 2 3 4)))
             (cln (enumerator-clone enumer)))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4))
         (assert-equal? (enumerator->list cln) '(1 2 3 4))))

    ;;;; collection tests
   (test "a contiguous-queue is a collection"
   (assert-true (collection? (contiguous-queue :capacity 3 1 2 3))))

   (test "(contiguous-queue 1 2 3) has length 3"
      (assert= (collection-length (contiguous-queue :capacity 3  1 2 3)) 3))

   (test "collection-contains? works on contiguous-queues"
      (assert-true (collection-contains? (contiguous-queue :capacity 3 1 2 3) 1)))

   (test "collection-empty? works on an empty contiguous-queue"
      (assert-true (collection-empty? (contiguous-queue :capacity 3))))

   (test "a contiguous-queue is a mutable-collection"
      (assert-true (collection-mutable? (contiguous-queue :capacity 3))))

   ;;;; enumerable tests
   (test "enumerable-for-each on contiguous-queues work"
      (let ((count 0))
         (enumerable-for-each  (lambda (e) (set! count (+ count 1))) (contiguous-queue :capacity 3 1 2 3))
         (assert= count 3)))

   (test "enumerable-map on contiguous-queues work"
      (assert-equal? (enumerator->list (enumerable-map (lambda (e) (+ e 1)) (contiguous-queue :capacity 3 1 2 3)))
         '(2 3 4)))

    (test "enumerable-filter on contiguous-queues work"
      (assert-equal? (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) (contiguous-queue :capacity 3 1 2 3)))
         '(2 3 )))

    (test "enumerable-fold on contiguous-queues work"
      (assert= (enumerable-fold + 0 (contiguous-queue :capacity 3 1 2 3) ) 6))

    (test "enumerable-any? odd? on (contiguous-queue :capacity 5 1 2 3 4 5) returns #t"
       (assert-equal?  (enumerable-any? odd? (contiguous-queue :capacity 5 1 2 3 4 5)) #t))
    
    (test "enumerable-every? odd? on (contiguous-queue :capacity 5 1 2 3 4 5) returns #f"
       (assert-equal?  (enumerable-every? odd? (contiguous-queue :capacity 5 1 2 3 4 5)) #f))

    (test "enumerable-every? odd? on (contiguous-queue :capacity 3 1  3  5) returns #t"
       (assert-equal?  (enumerable-every? odd? (contiguous-queue :capacity 3 1  3  5)) #t))

    (test "enumerable-skip 2 (contiguous-queue :capacity 5 1 2 3 4 5) yields an enumerator with the first element 3"
       (let ((enum (enumerable-skip 2 (contiguous-queue :capacity 5 1 2 3 4 5))))
          (assert-equal? (enumerator-current enum) 3)))

    (test "enumerable-skip-while odd? (contiguous-queue :capacity 5 1 3 5 6 7) yields an enumerator with the first element 6"
       (let ((enum (enumerable-skip-while odd? (contiguous-queue :capacity 5 1 3 5 6 7))))
          (assert-equal? (enumerator-current enum) 6)))
    
    (test "enumerable-append works on contiguous-queues"
      (let ((enumer (enumerable-append (contiguous-queue :capacity 3  1 2 3) (contiguous-queue :capacity 3 4 5 6))))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))))

    (test "enumerable-take works on contiguous-queues"
      (assert-equal? (enumerator->list (enumerable-take 2 (contiguous-queue :capacity 5 1 2 3 4 5)))
         '(1 2)))

    (test "enumerable-take-while works on contiguous-queues"
       (assert-equal? (enumerator->list (enumerable-take-while odd? (contiguous-queue :capacity 5 1 3 4 5)))
          '(1 3)))

   
   )


