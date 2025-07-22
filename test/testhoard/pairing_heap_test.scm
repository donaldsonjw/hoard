(module pairing-heap-test
   (library btest
            hoard)
   (export pairing-heap-tests))


(define-test-suite pairing-heap-tests
   (test "pairing-heap? works"
      ;(assert-true (pairing-heap? (pairing-heap :comparator +number-comparator+ 1 2 3)))
      (assert-true (pairing-heap? (make-pairing-heap :comparator +number-comparator+)))
      (assert-false (pairing-heap? 1))
      (assert-false (pairing-heap? (list 1))))

   (test "pairing-heap and make-pairing-heap throws an exception if comparator is of an invalid type"
      (assert-exception-thrown (pairing-heap :comparator 4)
         &invalid-argument-exception)
      (assert-exception-thrown (make-pairing-heap :comparator 4)
         &invalid-argument-exception)
      )

   (test "pairing-heap-empty? works"
      (let ((heap (make-pairing-heap :comparator +number-comparator+)))
         (assert-true (pairing-heap-empty? heap))
         (pairing-heap-enqueue! heap 4)
         (assert-false (pairing-heap-empty? heap))
         (pairing-heap-dequeue! heap)
         (assert-true (pairing-heap-empty? heap))))

   (test "pairing-heap-enqueue! and pairing-heap-dequeue! works"
      (let ((heap (pairing-heap :comparator +number-comparator+ 4 2 1 3)))
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
      (let ((heap (pairing-heap :comparator +number-comparator+ 3 2 5 8)))
         (assert-equal? (pairing-heap-first heap) 2)
         (pairing-heap-dequeue! heap)
         (assert-equal? (pairing-heap-first heap) 3)
         (pairing-heap-dequeue! heap)
         (assert-equal? (pairing-heap-first heap) 5)
         (pairing-heap-dequeue! heap)
         (assert-equal? (pairing-heap-first heap) 8)))
   
   (test "pairing-heap-length works"
      (let ((heap (pairing-heap :comparator +number-comparator+ 1 8)))
         (assert-equal? (pairing-heap-length heap) 2)
         (pairing-heap-dequeue! heap)
         (assert-equal? (pairing-heap-length heap) 1)
         (pairing-heap-dequeue! heap)
         (assert-equal? (pairing-heap-length heap) 0)))

   (test "pairing-heap-dequeue! throws an exception when deqeueing into a empty heap"
      (let ((heap (pairing-heap :comparator +number-comparator+)))
         (assert-exception-thrown (pairing-heap-dequeue! heap)
            &invalid-state-exception)))
   
   (test "pairing-heap-copy works"
      (let* ((h1 (pairing-heap :comparator +number-comparator+ 9 3 8 5))
            (h2 (pairing-heap-copy h1)))
         (assert-equal? (pairing-heap-length h1)
            (pairing-heap-length h2))
         (assert-equal? (pairing-heap-first h1)
            (pairing-heap-first h2))
         (pairing-heap-dequeue! h1)
         (assert-false (equal? (pairing-heap-first h1)
                          (pairing-heap-first h2)))
         (assert-false (equal? (pairing-heap-length h1)
                          (pairing-heap-length h2)))))


   ;;;; priority queue protocol tests
   (test "a pairing-heap is a priority queue"
      (assert-true (priority-queue? (pairing-heap :comparator +number-comparator+))))

   (test "priority-queue-enqueue! and priority-queue-dequeue! works on a pairing-heap"
      (let ((heap (pairing-heap :comparator +number-comparator+ 4 2 1 3)))
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
      (let ((heap (pairing-heap :comparator +number-comparator+ 3 2 5 8)))
         (assert-equal? (priority-queue-first heap) 2)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-first heap) 3)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-first heap) 5)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-first heap) 8)))

   (test "priority-queue-length works"
      (let ((heap (pairing-heap :comparator +number-comparator+ 1 8)))
         (assert-equal? (priority-queue-length heap) 2)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-length heap) 1)
         (priority-queue-dequeue! heap)
         (assert-equal? (priority-queue-length heap) 0)))

   (test "priority-queue-fixed-capacity? returns false for a pairing-heap"
      (assert-false (priority-queue-fixed-capacity? (pairing-heap :comparator +number-comparator+))))
   
   (test "priority-queue-capacity works"
      (assert-equal? (priority-queue-capacity (pairing-heap :comparator +number-comparator+))
         #unspecified))
   
   (test "priority-queue-empty? works"
      (let ((heap (pairing-heap :comparator +number-comparator+)))
         (assert-true (priority-queue-empty? heap))
         (priority-queue-enqueue! heap 4)
         (assert-false (priority-queue-empty? heap))
         (priority-queue-dequeue! heap)
         (assert-true (priority-queue-empty? heap))))

   (test "priority-queue-copy works"
      (let* ((h1 (pairing-heap :comparator +number-comparator+ 9 3 8 5))
            (h2 (priority-queue-copy h1)))
         (assert-equal? (priority-queue-length h1)
            (priority-queue-length h2))
         (assert-equal? (priority-queue-first h1)
            (priority-queue-first h2))
         (priority-queue-dequeue! h1)
         (assert-false (equal? (priority-queue-first h1)
                          (priority-queue-first h2)))
         (assert-false (equal? (priority-queue-length h1)
                          (priority-queue-length h2)))))


   ;;;; pairing-heap-enumerator tests
   (test "pairing-heap-enumerator throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((enumer (collection-enumerator (pairing-heap :comparator +number-comparator+))))
         (assert-exception-thrown (enumerator-current enumer)
            &error)))
   
   (test "pairing-heap-enumerator immediately returns false when empty"
       (let ((enumer (collection-enumerator (pairing-heap :comparator +number-comparator+))))
         (assert-false (enumerator-move-next! enumer))))

   (test "pairing-heap-enumerator for '(pairing-heap :comparator +number-comparator+ 1 2 3) return 3 items"
      (let ((enumer (collection-enumerator (pairing-heap :comparator +number-comparator+ 1 2 3))))
         (assert-equal? (let loop ((cont (enumerator-move-next! enumer))
                                   (res '()))
                           (if cont
                               (let ((t (enumerator-current enumer))) 
                                  (loop (enumerator-move-next! enumer)
                                     (cons t res)))
                               res)) (list 2 3 1))))https://medium.com/

   (test "cloning enumerators works correctly on pairing-heap-enumerators"
      (let* ((enumer (get-enumer (pairing-heap :comparator +number-comparator+ 1 2 3 4)))
             (cln (enumerator-copy enumer)))
         (assert-equal? (enumerator->list enumer) (enumerator->list cln))))


    ;;;; collection tests
   (test "a pairing-heap is a collection"
   (assert-true (collection? (pairing-heap :comparator +number-comparator+ 1 2 3))))

   (test "(pairing-heap :comparator +number-comparator+  1 2 3) has length 3"
      (assert= (collection-length (pairing-heap :comparator +number-comparator+ 1 2 3)) 3))

   (test "collection-contains? works on pairing-heaps"
      (assert-true (collection-contains? (pairing-heap :comparator +number-comparator+ 1 2 3) 1)))

   (test "collection-empty? works on an empty pairing-heaps"
      (assert-true (collection-empty? (pairing-heap :comparator +number-comparator+))))
   
   (test "collection-copy works"
      (let* ((h1 (pairing-heap :comparator +number-comparator+ 9 3 8 5))
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

   
   (test "a pairing-heap is a mutable-collection"
      (assert-true (collection-mutable? (pairing-heap :comparator +number-comparator+))))


   (test "a pairing-heap is extendable"
      (assert-true (collection-extendable? (pairing-heap :comparator +number-comparator+))))

   (test "extending a pairing heap works"
      (let ((heap (pairing-heap :comparator +number-comparator+)))
         (collection-extend! heap 5)
         (assert-equal? (pairing-heap-dequeue! heap) 5)))

   ;;;; enumerable tests
   (test "enumerable-for-each on pairing-heaps work"
      (let ((count 0))
         (enumerable-for-each  (lambda (e) (set! count (+ count 1))) (pairing-heap :comparator +number-comparator+ 1 2 3))
         (assert= count 3)))

   (test "enumerable-map on pairing-heaps work"
      (assert-equal? (enumerator->list (enumerable-map (lambda (e) e) (pairing-heap :comparator +number-comparator+ 1 2 3 4 5)))
         '(1 5 4 3 2)))

    (test "enumerable-filter on pairing-heaps work"
      (assert-equal? (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) (pairing-heap :comparator +number-comparator+ 1 2 3)))
         '(3 2)))

    (test "enumerable-fold on pairing-heaps work"
      (assert= (enumerable-fold + 0 (pairing-heap :comparator +number-comparator+ 1 2 3) ) 6))

    (test "enumerable-any? odd? on (pairing-heap :comparator +number-comparator+ 1 2 3 4 5) returns #t"
       (assert-equal?  (enumerable-any? odd? (pairing-heap :comparator +number-comparator+ 1 2 3 4 5)) #t))
    
    (test "enumerable-every? odd? on (pairing-heap :comparator +number-comparator+ 1 2 3 4 5) returns #f"
       (assert-equal?  (enumerable-every? odd? (pairing-heap :comparator +number-comparator+ 1 2 3 4 5)) #f))

    (test "enumerable-every? odd? on (pairing-heap :comparator +number-comparator+  1  3  5) returns #t"
       (assert-equal?  (enumerable-every? odd? (pairing-heap :comparator +number-comparator+ 1  3  5)) #t))

    (test "enumerable-skip 2 (pairing-heap :comparator +number-comparator+ 1 2 3 4 5) yields an enumerator with the first element 3"
       (let ((enum (enumerable-skip 2 (pairing-heap :comparator +number-comparator+ 1 2 3 4 5))))
          (assert-equal? (enumerator-current enum) 4)))

    (test "enumerable-skip-while odd? (pairing-heap :comparator +number-comparator+ 1 3 5 6 7) yields an enumerator with the first element 6"
       (let ((enum (enumerable-skip-while odd? (pairing-heap :comparator +number-comparator+ 1 3 5 6 7))))
          (assert-equal? (enumerator-current enum) 6)))
    
    (test "enumerable-append works on pairing-heap"
      (let ((enumer (enumerable-append (pairing-heap :comparator +number-comparator+ 1 2 3) (pairing-heap :comparator +number-comparator+ 4 5 6))))
         (assert-equal? (enumerator->list enumer) '(1 3 2 4 6 5))))

    (test "enumerable-take works on pairing-heaps"
      (assert-equal? (enumerator->list (enumerable-take 2 (pairing-heap :comparator +number-comparator+ 1 2 3 4 5)))
         '(1 5)))

    (test "enumerable-take-while works on pairing-heap"
       (assert-equal? (enumerator->list (enumerable-take-while odd? (pairing-heap :comparator +number-comparator+ 1 3 4 5)))
          '(1 5)))
   
    
   )
