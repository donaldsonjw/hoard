(module ring-buffer-test
   (library btest
            hoard)
   (export ring-buffer-tests))


(define-test-suite ring-buffer-tests

   (test "ring-buffer? works"
      (assert-true (ring-buffer? (ring-buffer :capacity 1)))
      (assert-true (ring-buffer? (make-ring-buffer :capacity 1)))
      (assert-false (ring-buffer? 3)))

   (test "ring-buffer-empty? works"
      (assert-true (ring-buffer-empty? (ring-buffer :capacity 4)))
      (assert-false (ring-buffer-empty? (ring-buffer :capacity 4 0 1 2 3))))


   (test "ring-buffer-push-back! works"
      (let ((rb (ring-buffer :capacity 4)))
         (ring-buffer-push-back! rb 1)
         (ring-buffer-push-back! rb 2)
         (assert-equal? (ring-buffer-pop-front! rb) 1)
         (assert-equal? (ring-buffer-pop-front!  rb) 2)
         (assert-true (ring-buffer-empty? rb))))

   (test "push-back into a full queue throws an exception"
      (let ((rb (ring-buffer :capacity 3  1 2 3)))
         (assert-exception-thrown (ring-buffer-push-back! rb 4)

            &invalid-state-exception)))

   (test "ring-buffer-push-front! works"
      (let ((rb (ring-buffer :capacity 4)))
         (ring-buffer-push-front! rb 1)
         (ring-buffer-push-front! rb 2)
         (assert-equal? (ring-buffer-pop-back! rb) 1)
         (assert-equal? (ring-buffer-pop-back!  rb) 2)
         (assert-true (ring-buffer-empty? rb))))

   (test "push-front into a full queue throws an exception"
      (let ((rb (ring-buffer :capacity 3  1 2 3)))
         (assert-exception-thrown (ring-buffer-push-front! rb 4)
            &invalid-state-exception)))
   
   (test "pop-front! works"
      (let ((rb (ring-buffer :capacity 3 1 2 3)))
         (assert-equal? (ring-buffer-pop-front! rb)
            1)
         (assert-equal? (ring-buffer-pop-front! rb)
            2)
         (assert-equal? (ring-buffer-pop-front! rb)
            3)
         (assert-true (ring-buffer-empty? rb))))

    (test "ring-buffer-front works"
      (let ((rb (ring-buffer :capacity 3 1 2 3)))
         
         (assert-equal? (ring-buffer-front rb)
            1)
         (ring-buffer-pop-front! rb)
         (assert-equal? (ring-buffer-front rb)
            2)
         (ring-buffer-pop-front! rb)
         (assert-equal? (ring-buffer-front rb)
            3)
         (ring-buffer-pop-front! rb)
         (assert-true (ring-buffer-empty? rb))))

    
    (test "pop front from an empty ring buffer throws an exception"
      (let ((rb (ring-buffer :capacity 4)))
         (assert-exception-thrown (ring-buffer-pop-front! rb)
            &invalid-state-exception)))

    (test "pop-back! works"
       (let ((rb (ring-buffer :capacity 3 1 2 3)))
          (print rb)
         (assert-equal? (ring-buffer-pop-back! rb)
            3)
         (print rb)
         (assert-equal? (ring-buffer-pop-back! rb)
            2)
a?         (assert-equal? (ring-buffer-pop-back! rb)
            1)
         (assert-true (ring-buffer-empty? rb))))

    (test "ring-buffer-back works"
      (let ((rb (ring-buffer :capacity 3 1 2 3)))
         
         (assert-equal? (ring-buffer-back rb)
            3)
         (ring-buffer-pop-back! rb)
         (assert-equal? (ring-buffer-back rb)
            2)
         (ring-buffer-pop-back! rb)
         (assert-equal? (ring-buffer-back rb)
            1)
         (ring-buffer-pop-back! rb)
         (assert-true (ring-buffer-empty? rb))))

    
    (test "pop back from an empty ring buffer throws an exception"
      (let ((rb (ring-buffer :capacity 4)))
         (assert-exception-thrown (ring-buffer-pop-back! rb)
            &invalid-state-exception)))
    
   (test "ring-buffer-length works"
      (let ((rb (ring-buffer :capacity 3 1 2 3)))
         (assert-equal? (ring-buffer-length rb) 3)
         (ring-buffer-pop-front! rb)
         (assert-equal? (ring-buffer-length rb) 2)
         (ring-buffer-pop-front! rb)
         (assert-equal? (ring-buffer-length rb) 1)))
   
   (test "pulling the first or last item from an empty ring-buffer throws an invalid state exception"
      (let ((rb (ring-buffer :capacity 3)))
         (assert-exception-thrown 
            (ring-buffer-front rb)
            &invalid-state-exception)
         (assert-exception-thrown 
            (ring-buffer-back rb)
            &invalid-state-exception)
         ))

   (test "ring-buffer-copy works on a ring-buffer"
         (let* ((rb1 (ring-buffer :capacity 6 1 2 3 4))
                (rb2 (ring-buffer-copy rb1)))
           (assert-equal? (ring-buffer-length rb1)
                         (ring-buffer-length rb2))
           (assert-equal? (ring-buffer-front rb1)
                         (ring-buffer-front rb2))
           (ring-buffer-pop-front! rb1)
           (assert-false (equal? (ring-buffer-length rb1)
                                 (ring-buffer-length rb2)))
           (ring-buffer-push-back! rb2 6)
           (assert-false (equal? (ring-buffer-front rb1)
                            (ring-buffer-front rb2)))))

   (test "ring-buffer is a deque"
      (assert-true (deque? (ring-buffer :capacity 3)))
      (assert-true (deque? (make-ring-buffer :capacity 3)))
      (assert-false (deque? (list))))

   (test "deque-empty works with a ring-buffer"
      (let ((deque (ring-buffer :capacity 3)))
         (assert-true (deque-empty? deque))
         (deque-enqueue! deque 3)
         (assert-false (deque-empty? deque))))

   (test "deque-first works with a ring-buffer"
      (let ((deque (ring-buffer :capacity 4 1 2 3 4)))
         (assert-equal? (deque-first deque) 1)
         (deque-dequeue! deque)
         (assert-equal? (deque-first deque) 2)
         (deque-dequeue! deque)
         (assert-equal? (deque-first deque) 3)
         (deque-dequeue! deque)
         (assert-equal? (deque-first deque) 4)))

   (test "deque-last works with a ring-buffer"
      (let ((deque (ring-buffer :capacity 4 1 2 3 4)))
         (assert-equal? (deque-last deque) 4)
         (deque-dequeue-back! deque)
         (assert-equal? (deque-last deque) 3)
         (deque-dequeue-back! deque)
         (assert-equal? (deque-last deque) 2)
         (deque-dequeue-back! deque)
         (assert-equal? (deque-last deque) 1)))

   (test "deque-enqueue! works with ring-buffer"
      (let ((deque (ring-buffer :capacity 4)))
         (deque-enqueue! deque 1)
         (assert-equal? (deque-last deque) 1)
         (deque-enqueue! deque 2)
         (assert-equal? (deque-last deque) 2)
         (deque-enqueue! deque 3)
         (assert-equal? (deque-last deque) 3)))

   (test "deque-enqueue-front! works with ring-buffer"
      (let ((deque (ring-buffer :capacity 4)))
         (deque-enqueue-front! deque 1)
         (assert-equal? (deque-first deque) 1)
         (deque-enqueue-front! deque 2)
         (assert-equal? (deque-first deque) 2)
         (deque-enqueue-front! deque 3)
         (assert-equal? (deque-first deque) 3)))

   (test "deque-dequeue! works with ring-buffer"
      (let ((deque (ring-buffer :capacity 5 1 2 3 4 5)))
         (assert-equal? (deque-dequeue! deque) 1)
         (assert-equal? (deque-dequeue! deque) 2)
         (assert-equal? (deque-dequeue! deque) 3)
         (assert-equal? (deque-dequeue! deque) 4)
         (deque-enqueue-front! deque -1)
         (assert-equal? (deque-dequeue! deque) -1)))
   
   (test "deque-dequeue-back! works with ring-buffer"
      (let ((deque (ring-buffer :capacity 5 1 2 3 4 5)))
         (assert-equal? (deque-dequeue-back! deque) 5)
         (assert-equal? (deque-dequeue-back! deque) 4)
         (assert-equal? (deque-dequeue-back! deque) 3)
         (assert-equal? (deque-dequeue-back! deque) 2)
         (deque-enqueue! deque -1)
         (assert-equal? (deque-dequeue-back! deque) -1)))

   (test "ring-buffer has a fixed capacity"
      (assert-true (deque-fixed-capacity? (ring-buffer :capacity 3))))

   (test "deque-capacity works with ring-buffer"
      (assert-equal? (deque-capacity (ring-buffer :capacity 3)) 3))
   
   (test "deque-copy works on ring-buffer"
      (let* ((deque1 (ring-buffer :capacity 4 1 2 3 4))
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

   (test "a ring-buffer is a queue?"
      (assert-true (queue? (ring-buffer :capacity 3))))
   
   (test "queue-first works"
      (let ((q (ring-buffer :capacity 3 1 2 3)))
         (assert-equal? (queue-first q) 1)
         (queue-dequeue! q)
         (assert-equal? (queue-first q) 2)
         (assert-equal? (queue-length q) 2)))
   
   (test "queue-length and queue-dequeue! works"
      (let ((q (ring-buffer :capacity 5 1 2 3 4 5)))
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
      (let ((q (make-ring-buffer :capacity 3)))
         (queue-enqueue! q 1)
         (queue-enqueue! q 2)
         (queue-enqueue! q 3)
         (assert-equal? (queue-length q) 3)
         (assert-equal? (queue-first q) 1)))

   (test "a ring-buffer does have fixed capacity"
      (let ((q (make-ring-buffer :capacity 3)))
         (assert-true (queue-fixed-capacity? q))))

   (test "a ring-buffer has  capacity"
      (let ((q (make-ring-buffer :capacity 4)))
        (assert-equal? (queue-capacity q) 4)))

   (test "queue-copy works on a ring-buffer"
         (let* ((q1 (ring-buffer :capacity 6 1 2 3 4))
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

   ;;;; ring-buffer-enumerator tests
   (test "ring-buffer-enumerator throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((enumer (collection-enumerator (ring-buffer :capacity 3))))
         (assert-exception-thrown (enumerator-current enumer)
            &error)))
   
   (test "contiguous-enumerator for immediately returns false on empty lst"
       (let ((enumer (collection-enumerator (ring-buffer :capacity 3))))
         (assert-false (enumerator-move-next! enumer))))

   (test "contiguous-enumerator for '(ring-buffer :capacity 1 2 3) return 3 items"
      (let ((enumer (collection-enumerator (ring-buffer :capacity 3 1 2 3))))
         (assert-equal? (let loop ((cont (enumerator-move-next! enumer))
                                   (res '()))
                           (if cont
                               (let ((t (enumerator-current enumer))) 
                                  (loop (enumerator-move-next! enumer)
                                     (cons t res)))
                               res)) (list 3 2 1))))

   (test "cloning enumerators works correctly on ring-buffer-enumerators"
      (let* ((enumer (get-enumer (ring-buffer :capacity 4 1 2 3 4)))
             (cln (enumerator-copy enumer)))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4))
         (assert-equal? (enumerator->list cln) '(1 2 3 4))))

    ;;;; collection tests
   (test "a ring-buffer is a collection"
   (assert-true (collection? (ring-buffer :capacity 3 1 2 3))))

   (test "(ring-buffer 1 2 3) has length 3"
      (assert= (collection-length (ring-buffer :capacity 3  1 2 3)) 3))

   (test "collection-contains? works on ring-buffers"
      (assert-true (collection-contains? (ring-buffer :capacity 3 1 2 3) 1)))

   (test "collection-empty? works on an empty ring-buffer"
         (assert-true (collection-empty? (ring-buffer :capacity 3))))

   (test "collection-copy works on a ring-buffer"
         (let* ((q1 (ring-buffer :capacity 6 1 2 3 4))
                (q2 (collection-copy q1)))
           (assert-equal? (collection-length q1)
                         (collection-length q2))
           (assert-equal? (queue-first q1)
                         (queue-first q2))
           (queue-dequeue! q1)
           (assert-false (equal? (collection-length q1)
                                 (collection-length q2)))
           (queue-enqueue! q2 6)
           (assert-false (equal? (queue-first q1)
                                 (queue-first q2)))))

   (test "a ring-buffer is a mutable-collection"
      (assert-true (collection-mutable? (ring-buffer :capacity 3))))

   ;;;; extendable tests
   (test "a ring-buffer is extendable"
      (assert-true (collection-extendable? (ring-buffer :capacity 4))))

   (test "collection-extend! works on ring-buffer"
      (let ((queue (ring-buffer :capacity 4)))
         (collection-extend! queue 3)
         (assert-equal? (ring-buffer-front queue) 3)))
   
   ;;;; enumerable tests
   (test "enumerable-for-each on ring-buffers work"
      (let ((count 0))
         (enumerable-for-each  (lambda (e) (set! count (+ count 1))) (ring-buffer :capacity 3 1 2 3))
         (assert= count 3)))

   (test "enumerable-map on ring-buffers work"
      (assert-equal? (enumerator->list (enumerable-map (lambda (e) (+ e 1)) (ring-buffer :capacity 3 1 2 3)))
         '(2 3 4)))

    (test "enumerable-filter on ring-buffers work"
      (assert-equal? (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) (ring-buffer :capacity 3 1 2 3)))
         '(2 3 )))

    (test "enumerable-fold on ring-buffers work"
      (assert= (enumerable-fold + 0 (ring-buffer :capacity 3 1 2 3) ) 6))

    (test "enumerable-any? odd? on (ring-buffer :capacity 5 1 2 3 4 5) returns #t"
       (assert-equal?  (enumerable-any? odd? (ring-buffer :capacity 5 1 2 3 4 5)) #t))
    
    (test "enumerable-every? odd? on (ring-buffer :capacity 5 1 2 3 4 5) returns #f"
       (assert-equal?  (enumerable-every? odd? (ring-buffer :capacity 5 1 2 3 4 5)) #f))

    (test "enumerable-every? odd? on (ring-buffer :capacity 3 1  3  5) returns #t"
       (assert-equal?  (enumerable-every? odd? (ring-buffer :capacity 3 1  3  5)) #t))

    (test "enumerable-skip 2 (ring-buffer :capacity 5 1 2 3 4 5) yields an enumerator with the first element 3"
       (let ((enum (enumerable-skip 2 (ring-buffer :capacity 5 1 2 3 4 5))))
          (assert-equal? (enumerator-current enum) 3)))

    (test "enumerable-skip-while odd? (ring-buffer :capacity 5 1 3 5 6 7) yields an enumerator with the first element 6"
       (let ((enum (enumerable-skip-while odd? (ring-buffer :capacity 5 1 3 5 6 7))))
          (assert-equal? (enumerator-current enum) 6)))
    
    (test "enumerable-append works on ring-buffers"
      (let ((enumer (enumerable-append (ring-buffer :capacity 3  1 2 3) (ring-buffer :capacity 3 4 5 6))))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))))

    (test "enumerable-take works on ring-buffers"
      (assert-equal? (enumerator->list (enumerable-take 2 (ring-buffer :capacity 5 1 2 3 4 5)))
         '(1 2)))

    (test "enumerable-take-while works on ring-buffers"
       (assert-equal? (enumerator->list (enumerable-take-while odd? (ring-buffer :capacity 5 1 3 4 5)))
          '(1 3)))



   
   )

