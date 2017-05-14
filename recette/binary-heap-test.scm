(module binary-heap-test
   (library btest
            hoard)
   (export binary-heap-tests))


(define-test-suite binary-heap-tests
 
   (test "binary-heap? works"
      (assert-true (binary-heap? (binary-heap :capacity 8 :comparator +number-comparator+ 1 2 3)))
      (assert-true (binary-heap? (make-binary-heap :capacity 8 :comparator +number-comparator+)))
      (assert-false (binary-heap? 1))
      (assert-false (binary-heap? (list 1))))

   (test "binary-heap throws an exception if capacity or lessthan is of an invalid type"
      (assert-exception-thrown (binary-heap :capacity #\a :comparator +number-comparator+)
         &invalid-argument-exception)
      (assert-exception-thrown (binary-heap :capacity 5 :comparator 4)
         &invalid-argument-exception))

   (test "make-binary-heap throws an exception if capacity or lessthan is of an invalid type"
      (assert-exception-thrown (make-binary-heap :capacity #\a :comparator +number-comparator+)
         &invalid-argument-exception)
      (assert-exception-thrown (make-binary-heap :capacity 5 :comparator 4)
         &invalid-argument-exception))

   
   (test "binary-heap-enqueue! and binary-heap-dequeue! works"
      (let ((heap (binary-heap :capacity 4 :comparator +number-comparator+ 4 2 1 3)))
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
      (let ((heap (binary-heap :capacity 4 :comparator +number-comparator+ 3 2 5 8)))
         (assert-equal? (binary-heap-first heap) 2)
         (binary-heap-dequeue! heap)
         (assert-equal? (binary-heap-first heap) 3)
         (binary-heap-dequeue! heap)
         (assert-equal? (binary-heap-first heap) 5)
         (binary-heap-dequeue! heap)
         (assert-equal? (binary-heap-first heap) 8)))

   (test "binary-heap-length works"
      (let ((heap (binary-heap :capacity 8 :comparator +number-comparator+ 1 8)))
         (assert-equal? (binary-heap-length heap) 2)
         (binary-heap-dequeue! heap)
         (assert-equal? (binary-heap-length heap) 1)
         (binary-heap-dequeue! heap)
         (assert-equal? (binary-heap-length heap) 0)))
   
   (test "binary-heap-enqueue! throws an exception inqueueing into a full heap"
      (let ((heap (binary-heap :capacity 1 :comparator +number-comparator+ 1)))
         (assert-exception-thrown (binary-heap-enqueue! heap 2)
            &invalid-state-exception)))

   (test "binary-heap-dequeue! throws an exception when deqeueing into a empty heap"
      (let ((heap (binary-heap :capacity 1 :comparator +number-comparator+)))
         (assert-exception-thrown (binary-heap-dequeue! heap)
            &invalid-state-exception)))

   (test "binary-heap-capacity works"
      (assert-equal? (binary-heap-capacity (binary-heap :capacity 5 :comparator +number-comparator+))
         5))

   (test "binary-heap-empty? works"
      (let ((heap (binary-heap :capacity 5 :comparator +number-comparator+)))
         (assert-true (binary-heap-empty? heap))
         (binary-heap-enqueue! heap 4)
         (assert-false (binary-heap-empty? heap))
         (binary-heap-dequeue! heap)
         (assert-true (binary-heap-empty? heap))))


   (test "binary-heap-copy works"
      (let* ((h1 (binary-heap :capacity 6 :comparator +number-comparator+ 9 3 8 5))
            (h2 (binary-heap-copy h1)))
         (assert-equal? (binary-heap-length h1)
            (binary-heap-length h2))
         (assert-equal? (binary-heap-first h1)
            (binary-heap-first h2))
         (binary-heap-dequeue! h1)
         (assert-false (equal? (binary-heap-first h1)
                          (binary-heap-first h2)))
         (assert-false (equal? (binary-heap-length h1)
                          (binary-heap-length h2)))))

   ;;;; priority queue protocol tests
   (test "a binary-heap is a priority queue"
      (assert-true (priority-queue? (binary-heap :capacity 0 :comparator +number-comparator+))))

   (test "priority-queue-enqueue! and priority-queue-dequeue! works on a binary-heap"
      (let ((heap (binary-heap :capacity 4 :comparator +number-comparator+ 4 2 1 3)))
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
      (let ((heap (binary-heap :capacity 4 :comparator +number-comparator+ 3 2 5 8)))
         (assert-equal? (priority-queue-first heap) 2)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-first heap) 3)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-first heap) 5)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-first heap) 8)))

   (test "priority-queue-length works"
      (let ((heap (binary-heap :capacity 8 :comparator +number-comparator+ 1 8)))
         (assert-equal? (priority-queue-length heap) 2)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-length heap) 1)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-length heap) 0)))

   (test "priority-queue-fixed-capacity? returns true for a binary-heap"
      (assert-true (priority-queue-fixed-capacity? (binary-heap :capacity 5 :comparator +number-comparator+))))
   
   (test "priority-queue-capacity works"
      (assert-equal? (priority-queue-capacity (binary-heap :capacity 5 :comparator +number-comparator+))
         5))
   
   (test "priority-queue-empty? works"
      (let ((heap (binary-heap :capacity 5 :comparator +number-comparator+)))
         (assert-true (priority-queue-empty? heap))
         (priority-queue-enqueue! heap 4)
         (assert-false (priority-queue-empty? heap))
         (priority-queue-dequeue! heap)
         (assert-true (priority-queue-empty? heap))))


   (test "priority-queue-copy works"
      (let* ((h1 (binary-heap :capacity 6 :comparator +number-comparator+ 9 3 8 5))
            (h2 (priority-queue-copy h1)))
         (assert-equal? (priority-queue-length h1)
            (binary-heap-length h2))
         (assert-equal? (priority-queue-first h1)
            (priority-queue-first h2))
         (priority-queue-dequeue! h1)
         (assert-false (equal? (priority-queue-first h1)
                          (priority-queue-first h2)))
         (assert-false (equal? (priority-queue-length h1)
                          (priority-queue-length h2)))))

 
   ;;;; binary-heap-enumerator tests
   (test "binary-heap-enumerator throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((enumer (collection-enumerator (binary-heap :capacity 3 :comparator +number-comparator+))))
         (assert-exception-thrown (enumerator-current enumer)
            &error)))
   
   (test "binary-heap-enumerator immediately returns false on empty lst"
       (let ((enumer (collection-enumerator (binary-heap :capacity 3 :comparator +number-comparator+))))
         (assert-false (enumerator-move-next! enumer))))

   (test "binary-heap-enumerator for '(binary-heap :capacity 3 :comparator +number-comparator+ 1 2 3) return 3 items"
      (let ((enumer (collection-enumerator (binary-heap :capacity 3 :comparator +number-comparator+ 1 2 3))))
         (assert-equal? (let loop ((cont (enumerator-move-next! enumer))
                                   (res '()))
                           (if cont
                               (let ((t (enumerator-current enumer))) 
                                  (loop (enumerator-move-next! enumer)
                                     (cons t res)))
                               res)) (list 3 2 1))))

   (test "cloning enumerators works correctly on binary-heap-enumerators"
      (let* ((enumer (get-enumer (binary-heap :capacity 4 :comparator +number-comparator+ 1 2 3 4)))
             (cln (enumerator-copy enumer)))
         (assert-equal? (enumerator->list enumer) (enumerator->list cln))))
   
       ;;;; collection tests
   (test "a binary-heap is a collection"
   (assert-true (collection? (binary-heap :capacity 3 :comparator +number-comparator+ 1 2 3))))

   (test "(binary-heap :capacity 3 :comparator +number-comparator+  1 2 3) has length 3"
      (assert= (collection-length (binary-heap :capacity 3 :comparator +number-comparator+ 1 2 3)) 3))

   (test "collection-contains? works on binary-heaps"
      (assert-true (collection-contains? (binary-heap :capacity 3 :comparator +number-comparator+ 1 2 3) 1)))

   (test "collection-empty? works on an empty binary-heaps"
      (assert-true (collection-empty? (binary-heap :capacity 3 :comparator +number-comparator+))))

   (test "collection-copy works"
      (let* ((h1 (binary-heap :capacity 6 :comparator +number-comparator+ 9 3 8 5))
            (h2 (collection-copy h1)))
         (assert-equal? (collection-length h1)
            (collection-length h2))
         (assert-equal? (priority-queue-first h1)
            (priority-queue-first h2))
         (priority-queue-dequeue! h1)
         (assert-false (equal? (priority-queue-first h1)
                          (priority-queue-first h2)))
         (assert-false (equal? (collection-length h1)
                          (collection-length h2)))))
   
   (test "a binary-heap is a mutable-collection"
      (assert-true (collection-mutable? (binary-heap :capacity 3 :comparator +number-comparator+))))


   (test "a binary-heap is extendable"
      (assert-true (collection-extendable? (binary-heap :capacity 4 :comparator +number-comparator+))))

   (test "extending a binary heap works"
      (let ((heap (binary-heap :capacity 5 :comparator +number-comparator+)))
         (collection-extend! heap 5)
         (assert-equal? (binary-heap-dequeue! heap) 5)))


   ;;;; enumerable tests
   (test "enumerable-for-each on binary-heaps work"
      (let ((count 0))
         (enumerable-for-each  (lambda (e) (set! count (+ count 1))) (binary-heap :capacity 5 :comparator +number-comparator+ 1 2 3))
         (assert= count 3)))

   (test "enumerable-map on binary-heaps work"
      (assert-equal? (enumerator->list (enumerable-map (lambda (e) e) (binary-heap :capacity 5 :comparator +number-comparator+ 1 2 3 4 5)))
         '(1 2 3 4 5)))

    (test "enumerable-filter on binary-heaps work"
      (assert-equal? (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) (binary-heap :capacity 5 :comparator +number-comparator+ 1 2 3)))
         '(2 3)))

    (test "enumerable-fold on binary-heaps work"
      (assert= (enumerable-fold + 0 (binary-heap :capacity 5 :comparator +number-comparator+ 1 2 3) ) 6))

    (test "enumerable-any? odd? on (binary-heap :capacity 5 :comparator +number-comparator+ 1 2 3 4 5) returns #t"
       (assert-equal?  (enumerable-any? odd? (binary-heap :capacity 5 :comparator +number-comparator+ 1 2 3 4 5)) #t))
    
    (test "enumerable-every? odd? on (binary-heap :capacity 5 :comparator +number-comparator+ 1 2 3 4 5) returns #f"
       (assert-equal?  (enumerable-every? odd? (binary-heap :capacity 5 :comparator +number-comparator+ 1 2 3 4 5)) #f))

    (test "enumerable-every? odd? on (binary-heap :capacity 5 :comparator +number-comparator+  1  3  5) returns #t"
       (assert-equal?  (enumerable-every? odd? (binary-heap :capacity 5 :comparator +number-comparator+ 1  3  5)) #t))

    (test "enumerable-skip 2 (binary-heap :capacity 5 :comparator +number-comparator+ 1 2 3 4 5) yields an enumerator with the first element 3"
       (let ((enum (enumerable-skip 2 (binary-heap :capacity 5 :comparator +number-comparator+ 1 2 3 4 5))))
          (assert-equal? (enumerator-current enum) 3)))

    (test "enumerable-skip-while odd? (binary-heap :capacity 5 :comparator +number-comparator+ 1 3 5 6 7) yields an enumerator with the first element 6"
       (let ((enum (enumerable-skip-while odd? (binary-heap :capacity 5 :comparator +number-comparator+ 1 3 5 6 7))))
          (assert-equal? (enumerator-current enum) 6)))
    
    (test "enumerable-append works on binary-heap"
      (let ((enumer (enumerable-append (binary-heap :capacity 5 :comparator +number-comparator+ 1 2 3) (binary-heap :capacity 5 :comparator +number-comparator+ 4 5 6))))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))))

    (test "enumerable-take works on binary-heaps"
      (assert-equal? (enumerator->list (enumerable-take 2 (binary-heap :capacity 5 :comparator +number-comparator+ 1 2 3 4 5)))
         '(1 2)))

    (test "enumerable-take-while works on binary-heap"
       (assert-equal? (enumerator->list (enumerable-take-while odd? (binary-heap :capacity 5 :comparator +number-comparator+ 1 3 4 5)))
          '(1 3)))

   )