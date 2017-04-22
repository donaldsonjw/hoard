(module sorted-bag-test
   (library btest
            hoard)
   (export sorted-bag-tests))

(define-test-suite sorted-bag-tests
   
   (test "sorted-bag? works"
      (assert-true (sorted-bag? (sorted-bag :comparator +string-comparator+)))
      (assert-true (sorted-bag? (make-sorted-bag :comparator +string-comparator+)))
      (assert-false (sorted-bag? (list))))

   (test "sorted-bag works"
      (let ((bag (sorted-bag :comparator +number-comparator+ 1 2 1 4 1)))
         (assert-true (sorted-bag-contains? bag 1))
         (assert-true (sorted-bag-contains? bag 2))
         (assert-true (sorted-bag-contains? bag 4))
         (assert-false (sorted-bag-contains? bag 5))))

   (test "sorted-bag-insert! works"
      (let ((bag (make-sorted-bag :comparator +number-comparator+)))
         (assert-false (sorted-bag-contains? bag 1))
         (sorted-bag-insert! bag 1)
         (assert-true (sorted-bag-contains? bag 1))
         (sorted-bag-insert! bag 2)
         (assert-true (sorted-bag-contains? bag 2))))

   (test "sorted-bag-delete! works"
      (let ((bag (sorted-bag :comparator +number-comparator+  1 1 2 3)))
         (assert-true (sorted-bag-contains? bag 2))
         (sorted-bag-delete! bag 2)
         (assert-false (sorted-bag-contains? bag 2))
         (sorted-bag-delete! bag 1)
         (assert-true (sorted-bag-contains?  bag 1))
         (sorted-bag-delete! bag 1)
         (assert-false (sorted-bag-contains? bag 1))))

   (test "sorted-bag-empty? works"
      (let ((bag (sorted-bag :comparator +number-comparator+)))
         (assert-true (sorted-bag-empty? bag))
         (sorted-bag-insert! bag 4)
         (assert-false (sorted-bag-empty? bag))
         (sorted-bag-delete! bag 4)
         (assert-true (sorted-bag-empty? bag))))

   (test "sorted-bag-length works"
      (let ((bag (sorted-bag :comparator +number-comparator+ 1 1 2 3)))
         (assert-equal? (sorted-bag-length bag) 4)
         (sorted-bag-delete! bag 1)
         (assert-equal? (sorted-bag-length bag) 3)
         (sorted-bag-delete! bag 2)
         (assert-equal? (sorted-bag-length bag) 2)))

   (test "sorted-bag-contains? works"
      (let ((bag (sorted-bag :comparator +number-comparator+ 2 4 5 6)))
         (assert-true (sorted-bag-contains? bag 5))
         (assert-true (sorted-bag-contains? bag 2))
         (assert-false (sorted-bag-contains? bag 8))))

   (test "sorted-bag-count works"
      (let ((bag (sorted-bag :comparator +number-comparator+ 1 1 1 4 7 11)))
         (assert-equal? (sorted-bag-count bag 1) 3)
         (assert-equal? (sorted-bag-count bag 4) 1)
         (assert-equal? (sorted-bag-count bag 9) 0)))

   (test "sorted-bag-count-set! works"
      (let ((bag (sorted-bag :comparator +number-comparator+)))
         (sorted-bag-count-set! bag 4 3)
         (assert-equal? (sorted-bag-count bag 4) 3)
         (sorted-bag-count-set! bag 4 2)
         (assert-equal? (sorted-bag-count bag 4) 2)))

      ;;; bag protocol
   (test "bag-insert! works"
      (let ((bag (make-sorted-bag :comparator +number-comparator+)))
         (assert-false (bag-contains? bag 1))
         (bag-insert! bag 1)
         (assert-true (bag-contains? bag 1))
         (bag-insert! bag 2)
         (assert-true (bag-contains? bag 2))))
      
   (test "bag-delete! works"
      (let ((bag (sorted-bag :comparator +number-comparator+  1 1 2 3)))
         (assert-true (bag-contains? bag 2))
         (bag-delete! bag 2)
         (assert-false (bag-contains? bag 2))
         (bag-delete! bag 1)
         (assert-true (bag-contains?  bag 1))
         (bag-delete! bag 1)
         (assert-false (bag-contains? bag 1))))
      
   (test "bag-empty? works"
      (let ((bag (sorted-bag :comparator +number-comparator+)))
         (assert-true (bag-empty? bag))
         (bag-insert! bag 4)
         (assert-false (bag-empty? bag))
         (bag-delete! bag 4)
         (assert-true (bag-empty? bag))))

      
   (test "bag-contains? works"
      (let ((bag (sorted-bag :comparator +number-comparator+ 2 4 5 6)))
         (assert-true (bag-contains? bag 5))
         (assert-true (bag-contains? bag 2))
         (assert-false (bag-contains? bag 8))))

   (test "bag-count works"
      (let ((bag (sorted-bag :comparator +number-comparator+ 1 1 1 4 7 11)))
         (assert-equal? (bag-count bag 1) 3)
         (assert-equal? (bag-count bag 4) 1)
         (assert-equal? (bag-count bag 9) 0)))


   (test "bag-count-set! works"
      (let ((bag (sorted-bag :comparator +number-comparator+)))
         (bag-count-set! bag 4 3)
         (assert-equal? (bag-count bag 4) 3)
         (bag-count-set! bag 4 2)
         (assert-equal? (bag-count bag 4) 2)))

   (test "bag-length works"
      (let ((bag (sorted-bag :comparator +number-comparator+ 1 1 2 3)))
         (assert-equal? (bag-length bag) 4)
         (bag-delete! bag 1)
         (assert-equal? (bag-length bag) 3)
         (bag-delete! bag 2)
         (assert-equal? (bag-length bag) 2)))

   
      ;;;; sorted-bag-enumerator tests
   (test "sorted-bag-enumerator throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((enumer  (make-sorted-bag-enumerator (sorted-bag :comparator +number-comparator+ 1 1 2 3 4))))
         (assert-exception-thrown (enumerator-current enumer)
            &error)))
   
  
   (test "sorted-bag-enumerator immediately returns false when empty"
      (let ((enumer (make-sorted-bag-enumerator (sorted-bag :comparator +number-comparator+))))
         (assert-false (enumerator-move-next! enumer))))

   (test "sorted-bag-enumerator for (sorted-bag :comparator +number-comparator+ 1 1 2 7 3 6 4 )  return 8 items"
      (let ((bag (sorted-bag :comparator +number-comparator+ 1 1 2 7 3 6 4 5)))
         (let ((enum (make-sorted-bag-enumerator bag)))
            (assert-equal? (let loop ((cont (enumerator-move-next! enum))
                                      (res '()))
                              (if cont
                                  (let ((t (enumerator-current enum))) 
                                     (loop (enumerator-move-next! enum)
                                        (cons t res)))
                                  (length res))) 8))))
   
   (test "cloning enumerators works correctly on sorted-bag-enumerators"
      (let* ((bag (sorted-bag :comparator +number-comparator+ 1 4 2 3))
             (enumer (make-sorted-bag-enumerator bag))
             (cln (enumerator-clone enumer)))
         (assert-equal? (enumerator->list enumer)
            (enumerator->list cln))))

    ;;;; enumerable tests
   (test "sorted-bag is an enumerable"
      (assert-true (enumerable? (make-sorted-bag :comparator +number-comparator+)))
      (assert-false (enumerable? 5)))
   
   (test "enumerable-for-each on sorted-bags work"
      (let ((count 0))
         (enumerable-for-each  (lambda (e) (set! count (+ count 1)))
            (sorted-bag :comparator +number-comparator+ 1 1 2 3))
         (assert= count 4)))

   (test "enumerable-map on sorted-bags work"
      (let ((res (enumerator->list (enumerable-map (lambda (e) (+ e 1))
                                      (sorted-bag :comparator +number-comparator+ 1 2 3)))))
         (assert-true (and (collection-contains? res 2)
                           (collection-contains? res 3)
                           (collection-contains? res 4)))))
   
    (test "enumerable-filter on sorted-bags work"
       (let ((res (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) (sorted-bag :comparator +number-comparator+ 1 2 3)))))
          (assert-true (and (collection-contains? res 2)
                            (collection-contains? res 3)))))

    (test "enumerable-fold on sorted-bags work"
      (assert= (enumerable-fold + 0 (sorted-bag :comparator +number-comparator+ 1 2 3)) 6))

    (test "enumerable-any? odd? on (sorted-bag :comparator +number-comparator+ 1 2 3 4 5) returns #t"
       (assert-equal?  (enumerable-any? odd? (sorted-bag :comparator +number-comparator+ 1 2 3 4 5)) #t))
    
    (test "enumerable-every? odd? on (sorted-bag :comparator +number-comparator+ 1 2 3 4 5) returns #f"
       (assert-equal?  (enumerable-every? odd? (sorted-bag :comparator +number-comparator+ 1 2 3 4 5)) #f))

    (test "enumerable-every? odd? on (sorted-bag :comparator +number-comparator+ 1  3  5) returns #t"
       (assert-equal?  (enumerable-every? odd? (sorted-bag :comparator +number-comparator+ 1  3  5)) #t))

    (test "enumerable-skip 2 (sorted-bag :comparator +number-comparator+ 1 2 3 4 5) yields an enumerator with the first element 3"
       (let ((enum (enumerable-skip 2 (sorted-bag :comparator +number-comparator+ 1 2 3 4 5))))
          (assert-equal? (enumerator-current enum) 3)))

    (test "enumerable-skip-while odd? (sorted-bag :comparator +number-comparator+ 1 3 5 6 7) yields an enumerator with the first element 6"
       (let ((enum (enumerable-skip-while odd? (sorted-bag :comparator +number-comparator+  1 3 5 6 7))))
          (assert-equal? (enumerator-current enum) 6)))
    
    (test "enumerable-append works on sorted-bags"
      (let ((enumer (enumerable-append (sorted-bag  :comparator +number-comparator+ 1 2 3) (sorted-bag :comparator +number-comparator+ 4 5 6))))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))))

    (test "enumerable-take works on sorted-bags"
      (assert-equal? (enumerator->list (enumerable-take 2 (sorted-bag :comparator +number-comparator+ 1 2 3 4 5)))
         '(1 2)))

    (test "enumerable-take-while works on sorted-bags"
       (assert-equal? (enumerator->list (enumerable-take-while odd? (sorted-bag :comparator +number-comparator+ 1 3 4 5)))
          '(1 3)))


   ;;;; collection tests    
   (test "a sorted-bag is a collection"
      (assert-true (collection? (sorted-bag :comparator +number-comparator+ 2 3))))

   (test "(sorted-bag :comparator +number-comparator+ 1 2 3) has length 3"
      (assert= (collection-length (sorted-bag :comparator +number-comparator+ 1 2 3)) 3))

   (test "collection-contains? works on sorted-bag"
      (assert-true (collection-contains? (sorted-bag :comparator +number-comparator+ 1 2 3) 1)))

   (test "collection-empty? works on an empty sorted-bag"
      (assert-true (collection-empty? (sorted-bag :comparator +number-comparator+))))

   ;;;; sorted-bag is mutable
   (test "sorted-bag is mutable"
      (assert-true (collection-mutable? (sorted-bag :comparator +number-comparator+))))


   ;;;; extendable tests
   (test "sorted-bag is extendable"
      (assert-true (collection-extendable? (sorted-bag :comparator +number-comparator+))))

   (test "collection-extend! works on hash-bags"
      (let ((bag (sorted-bag :comparator +number-comparator+)))
         (assert-false (sorted-bag-contains? bag 1))
         (collection-extend! bag 1)
         (assert-true (sorted-bag-contains?  bag 1))))
   
   )