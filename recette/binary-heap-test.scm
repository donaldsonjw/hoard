(module binary-heap-test
   (library btest
            hoard)
   (export binary-heap-tests))


(define-test-suite binary-heap-tests
 
   (test "binary-heap? works"
      (assert-true (binary-heap? (binary-heap :capacity 8 :lessthan < 1 2 3)))
      (assert-true (binary-heap? (make-binary-heap :capacity 8 :lessthan <)))
      (assert-false (binary-heap? 1))
      (assert-false (binary-heap? (list 1))))

   (test "binary-heap throws an exception if capacity or lessthan is of an invalid type"
      (assert-exception-thrown (binary-heap :capacity #\a :lessthan <)
         &invalid-argument-exception)
      (assert-exception-thrown (binary-heap :capacity 5 :lessthan 4)
         &invalid-argument-exception))

   (test "make-binary-heap throws an exception if capacity or lessthan is of an invalid type"
      (assert-exception-thrown (make-binary-heap :capacity #\a :lessthan <)
         &invalid-argument-exception)
      (assert-exception-thrown (make-binary-heap :capacity 5 :lessthan 4)
         &invalid-argument-exception))

   
   (test "binary-heap-enqueue! and binary-heap-dequeue! works"
      (let ((heap (binary-heap :capacity 4 :lessthan < 4 2 1 3)))
         (assert-equal? (binary-heap-dequeue! heap) 1)
         (binary-heap-enqueue! heap 5)
         (assert-equal? (binary-heap-dequeue! heap) 2)
         (binary-heap-enqueue! heap 1)
         (assert-equal? (binary-heap-dequeue! heap) 1)
         (binary-heap-enqueue! heap 2)
         (assert-equal? (binary-heap-dequeue! heap) 2)
         (assert-equal? (binary-heap-dequeue! heap) 3)
         (assert-equal? (binary-heap-dequeue! heap) 4)
         (assert-equal? (binary-heap-dequeue! heap) 5)
         ))

   (test "binary-heap-first works"
      (let ((heap (binary-heap :capacity 4 :lessthan < 3 2 5 8)))
         (assert-equal? (binary-heap-first heap) 2)
         (binary-heap-dequeue! heap)
         (assert-equal? (binary-heap-first heap) 3)
         (binary-heap-dequeue! heap)
         (assert-equal? (binary-heap-first heap) 5)
         (binary-heap-dequeue! heap)
         (assert-equal? (binary-heap-first heap) 8)))

   (test "binary-heap-length works"
      (let ((heap (binary-heap :capacity 8 :lessthan < 1 8)))
         (assert-equal? (binary-heap-length heap) 2)
         (binary-heap-dequeue! heap)
         (assert-equal? (binary-heap-length heap) 1)
         (binary-heap-dequeue! heap)
         (assert-equal? (binary-heap-length heap) 0)))
   
   (test "binary-heap-enqueue! throws an exception inqueueing into a full heap"
      (let ((heap (binary-heap :capacity 1 :lessthan < 1)))
         (assert-exception-thrown (binary-heap-enqueue! heap 2)
            &invalid-state-exception)))

   (test "binary-heap-dequeue! throws an exception when deqeueing into a empty heap"
      (let ((heap (binary-heap :capacity 1 :lessthan <)))
         (assert-exception-thrown (binary-heap-dequeue! heap)
            &invalid-state-exception)))

   (test "binary-heap-capacity works"
      (assert-equal? (binary-heap-capacity (binary-heap :capacity 5 :lessthan <))
         5))

   (test "binary-heap-empty? works"
      (let ((heap (binary-heap :capacity 5 :lessthan <)))
         (assert-true (binary-heap-empty? heap))
         (binary-heap-enqueue! heap 4)
         (assert-false (binary-heap-empty? heap))
         (binary-heap-dequeue! heap)
         (assert-true (binary-heap-empty? heap))))

   ;;;; priority queue protocol tests
   (test "a binary-heap is a priority queue"
      (assert-true (priority-queue? (binary-heap :capacity 0 :lessthan <))))

   (test "priority-queue-enqueue! and priority-queue-dequeue! works on a binary-heap"
      (let ((heap (binary-heap :capacity 4 :lessthan < 4 2 1 3)))
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
      (let ((heap (binary-heap :capacity 4 :lessthan < 3 2 5 8)))
         (assert-equal? (priority-queue-first heap) 2)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-first heap) 3)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-first heap) 5)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-first heap) 8)))

   (test "priority-queue-length works"
      (let ((heap (binary-heap :capacity 8 :lessthan < 1 8)))
         (assert-equal? (priority-queue-length heap) 2)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-length heap) 1)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-length heap) 0)))

   (test "priority-queue-fixed-capacity? returns true for a binary-heap"
      (assert-true (priority-queue-fixed-capacity? (binary-heap :capacity 5 :lessthan <))))
   
   (test "priority-queue-capacity works"
      (assert-equal? (priority-queue-capacity (binary-heap :capacity 5 :lessthan <))
         5))
   
   (test "priority-queue-empty? works"
      (let ((heap (binary-heap :capacity 5 :lessthan <)))
         (assert-true (priority-queue-empty? heap))
         (priority-queue-enqueue! heap 4)
         (assert-false (priority-queue-empty? heap))
         (priority-queue-dequeue! heap)
         (assert-true (priority-queue-empty? heap))))
 
   ;;;; binary-heap-enumerator tests
   (test "binary-heap-enumerator throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((enumer (collection-enumerator (binary-heap :capacity 3 :lessthan <))))
         (assert-exception-thrown (enumerator-current enumer)
            &error)))
   
   (test "binary-heap-enumerator immediately returns false on empty lst"
       (let ((enumer (collection-enumerator (binary-heap :capacity 3 :lessthan <))))
         (assert-false (enumerator-move-next! enumer))))

   (test "binary-heap-enumerator for '(binary-heap :capacity 3 :lessthan < 1 2 3) return 3 items"
      (let ((enumer (collection-enumerator (binary-heap :capacity 3 :lessthan < 1 2 3))))
         (assert-equal? (let loop ((cont (enumerator-move-next! enumer))
                                   (res '()))
                           (if cont
                               (let ((t (enumerator-current enumer))) 
                                  (loop (enumerator-move-next! enumer)
                                     (cons t res)))
                               res)) (list 3 2 1))))

   (test "cloning enumerators works correctly on binary-heap-enumerators"
      (let* ((enumer (get-enumer (binary-heap :capacity 4 :lessthan < 1 2 3 4)))
             (cln (enumerator-clone enumer)))
         (assert-equal? (enumerator->list enumer) (enumerator->list cln))))
   
       ;;;; collection tests
   (test "a binary-heap is a collection"
   (assert-true (collection? (binary-heap :capacity 3 :lessthan < 1 2 3))))

   (test "(binary-heap :capacity 3 :lessthan <  1 2 3) has length 3"
      (assert= (collection-length (binary-heap :capacity 3 :lessthan < 1 2 3)) 3))

   (test "collection-contains? works on binary-heaps"
      (assert-true (collection-contains? (binary-heap :capacity 3 :lessthan < 1 2 3) 1)))

   (test "collection-empty? works on an empty binary-heaps"
      (assert-true (collection-empty? (binary-heap :capacity 3 :lessthan <))))

   (test "a binary-heap is a mutable-collection"
      (assert-true (collection-mutable? (binary-heap :capacity 3 :lessthan <))))


   )