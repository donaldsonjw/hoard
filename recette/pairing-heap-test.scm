(module pairing-heap-test
   (library btest
            hoard)
   (export pairing-heap-tests))


(define-test-suite pairing-heap-tests
   (test "pairing-heap? works"
      ;(assert-true (pairing-heap? (pairing-heap :lessthan < 1 2 3)))
      (assert-true (pairing-heap? (make-pairing-heap :lessthan <)))
      (assert-false (pairing-heap? 1))
      (assert-false (pairing-heap? (list 1))))

   (test "pairing-heap and make-pairing-heap throws an exception if lessthan is of an invalid type"
      (assert-exception-thrown (pairing-heap :lessthan 4)
         &invalid-argument-exception)
      (assert-exception-thrown (make-pairing-heap :lessthan 4)
         &invalid-argument-exception)
      )

   (test "pairing-heap-empty? works"
      (let ((heap (make-pairing-heap :lessthan <)))
         (assert-true (pairing-heap-empty? heap))
         (pairing-heap-enqueue! heap 4)
         (assert-false (pairing-heap-empty? heap))
         (pairing-heap-dequeue! heap)
         (assert-true (pairing-heap-empty? heap))))

   (test "pairing-heap-enqueue! and pairing-heap-dequeue! works"
      (let ((heap (pairing-heap :lessthan < 4 2 1 3)))
         (assert-equal? (pairing-heap-dequeue! heap) 1)
         (pairing-heap-enqueue! heap 5)
         (assert-equal? (pairing-heap-dequeue! heap) 2)
         (assert-equal? (pairing-heap-dequeue! heap) 3)
         (pairing-heap-enqueue! heap 2)
         (assert-equal? (pairing-heap-dequeue! heap) 2)
         (assert-equal? (pairing-heap-dequeue! heap) 4)
         (assert-equal? (pairing-heap-dequeue! heap) 5)
         ))
   
   (test "pairing-heap-first works"
      (let ((heap (pairing-heap :lessthan < 3 2 5 8)))
         (assert-equal? (pairing-heap-first heap) 2)
         (pairing-heap-dequeue! heap)
         (assert-equal? (pairing-heap-first heap) 3)
         (pairing-heap-dequeue! heap)
         (assert-equal? (pairing-heap-first heap) 5)
         (pairing-heap-dequeue! heap)
         (assert-equal? (pairing-heap-first heap) 8)))
   
   (test "pairing-heap-length works"
      (let ((heap (pairing-heap :lessthan < 1 8)))
         (assert-equal? (pairing-heap-length heap) 2)
         (pairing-heap-dequeue! heap)
         (assert-equal? (pairing-heap-length heap) 1)
         (pairing-heap-dequeue! heap)
         (assert-equal? (pairing-heap-length heap) 0)))

   (test "pairing-heap-dequeue! throws an exception when deqeueing into a empty heap"
      (let ((heap (pairing-heap :lessthan <)))
         (assert-exception-thrown (pairing-heap-dequeue! heap)
            &invalid-state-exception)))

   ;;;; priority queue protocol tests
   (test "a pairing-heap is a priority queue"
      (assert-true (priority-queue? (pairing-heap :lessthan <))))

   (test "priority-queue-enqueue! and priority-queue-dequeue! works on a pairing-heap"
      (let ((heap (pairing-heap :lessthan < 4 2 1 3)))
         (assert-equal? (priority-queue-dequeue! heap) 1)
         (priority-queue-enqueue! heap 5)
         (assert-equal? (priority-queue-dequeue! heap) 2)
         (priority-queue-enqueue! heap 1)
         (assert-equal? (priority-queue-dequeue! heap) 1)
         (priority-queue-enqueue! heap 2)
         (assert-equal? (priority-queue-dequeue! heap) 2)
         (assert-equal? (priority-queue-dequeue! heap) 3)
         (assert-equal? (priority-queue-dequeue! heap) 4)
         (assert-equal? (priority-queue-dequeue! heap) 5)
         ))
   
   (test "priority-queue-first works"
      (let ((heap (pairing-heap :lessthan < 3 2 5 8)))
         (assert-equal? (priority-queue-first heap) 2)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-first heap) 3)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-first heap) 5)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-first heap) 8)))

   (test "priority-queue-length works"
      (let ((heap (pairing-heap :lessthan < 1 8)))
         (assert-equal? (priority-queue-length heap) 2)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-length heap) 1)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-length heap) 0)))

   (test "priority-queue-fixed-capacity? returns false for a pairing-heap"
      (assert-false (priority-queue-fixed-capacity? (pairing-heap :lessthan <))))
   
   (test "priority-queue-capacity works"
      (assert-equal? (priority-queue-capacity (pairing-heap :lessthan <))
         #unspecified))
   
   (test "priority-queue-empty? works"
      (let ((heap (pairing-heap :lessthan <)))
         (assert-true (priority-queue-empty? heap))
         (priority-queue-enqueue! heap 4)
         (assert-false (priority-queue-empty? heap))
         (priority-queue-dequeue! heap)
         (assert-true (priority-queue-empty? heap))))

   ;;;; pairing-heap-enumerator tests
   (test "pairing-heap-enumerator throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((enumer (collection-enumerator (pairing-heap :lessthan <))))
         (assert-exception-thrown (enumerator-current enumer)
            &error)))
   
   (test "pairing-heap-enumerator immediately returns false when empty"
       (let ((enumer (collection-enumerator (pairing-heap :lessthan <))))
         (assert-false (enumerator-move-next! enumer))))

   (test "pairing-heap-enumerator for '(pairing-heap :lessthan < 1 2 3) return 3 items"
      (let ((enumer (collection-enumerator (pairing-heap :lessthan < 1 2 3))))
         (assert-equal? (let loop ((cont (enumerator-move-next! enumer))
                                   (res '()))
                           (if cont
                               (let ((t (enumerator-current enumer))) 
                                  (loop (enumerator-move-next! enumer)
                                     (cons t res)))
                               res)) (list 2 3 1))))https://medium.com/

   (test "cloning enumerators works correctly on pairing-heap-enumerators"
      (let* ((enumer (get-enumer (pairing-heap :lessthan < 1 2 3 4)))
             (cln (enumerator-clone enumer)))
         (assert-equal? (enumerator->list enumer) (enumerator->list cln))))


    ;;;; collection tests
   (test "a pairing-heap is a collection"
   (assert-true (collection? (pairing-heap :lessthan < 1 2 3))))

   (test "(pairing-heap :lessthan <  1 2 3) has length 3"
      (assert= (collection-length (pairing-heap :lessthan < 1 2 3)) 3))

   (test "collection-contains? works on pairing-heaps"
      (assert-true (collection-contains? (pairing-heap :lessthan < 1 2 3) 1)))

   (test "collection-empty? works on an empty pairing-heaps"
      (assert-true (collection-empty? (pairing-heap :lessthan <))))

   (test "a pairing-heap is a mutable-collection"
      (assert-true (collection-mutable? (pairing-heap :lessthan <))))
   
   )
