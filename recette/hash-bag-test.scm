(module hash-bag-test
   (library btest hoard)
   (export hash-bag-tests))


(define-test-suite hash-bag-tests

   (test "hash-bag? works"
      (assert-true (hash-bag? (make-hash-bag)))
      (assert-true (hash-bag? (hash-bag 1 2 3 4)))
      (assert-false (hash-bag? 4))
      (assert-false (hash-bag? '(1 2 3))))

   (test "hash-bag works"
      (let ((bag (hash-bag 1 2 1 4 1)))
         (assert-true (hash-bag-contains? bag 1))
         (assert-true (hash-bag-contains? bag 2))
         (assert-true (hash-bag-contains? bag 4))
         (assert-false (hash-bag-contains? bag 5))))


   (test "hash-bag with comparator works"
      (let ((bag (hash-bag :comparator +string-ci-comparator+ "A" "a" "B" "b" "b")))
         (assert-equal? (hash-bag-count bag "a") 2)
         (assert-equal? (hash-bag-count bag "A") 2)
         (assert-equal? (hash-bag-count bag "B") 3)
         (assert-equal? (hash-bag-count bag "b") 3)))
   
   (test "hash-bag-insert! works"
      (let ((bag (make-hash-bag)))
         (assert-false (hash-bag-contains? bag 1))
         (hash-bag-insert! bag 1)
         (assert-true (hash-bag-contains? bag 1))
         (hash-bag-insert! bag 2)
         (assert-true (hash-bag-contains? bag 2))))

   (test "hash-bag-delete! works"
      (let ((bag (hash-bag  1 1 2 3)))
         (assert-true (hash-bag-contains? bag 2))
         (hash-bag-delete! bag 2)
         (assert-false (hash-bag-contains? bag 2))
         (hash-bag-delete! bag 1)
         (assert-true (hash-bag-contains?  bag 1))
         (hash-bag-delete! bag 1)
         (assert-false (hash-bag-contains? bag 1))))

   (test "hash-bag-empty? works"
      (let ((bag (hash-bag)))
         (assert-true (hash-bag-empty? bag))
         (hash-bag-insert! bag 4)
         (assert-false (hash-bag-empty? bag))
         (hash-bag-delete! bag 4)
         (assert-true (hash-bag-empty? bag))))

   (test "hash-bag-length works"
      (let ((bag (hash-bag 1 1 2 3)))
         (assert-equal? (hash-bag-length bag) 4)
         (hash-bag-delete! bag 1)
         (assert-equal? (hash-bag-length bag) 3)
         (hash-bag-delete! bag 2)
         (assert-equal? (hash-bag-length bag) 2)))
         
   (test "hash-bag-contains? works"
      (let ((bag (hash-bag 2 4 5 6)))
         (assert-true (hash-bag-contains? bag 5))
         (assert-true (hash-bag-contains? bag 2))
         (assert-false (hash-bag-contains? bag 8))))

   (test "hash-bag-count works"
      (let ((bag (hash-bag 1 1 1 4 7 11)))
         (assert-equal? (hash-bag-count bag 1) 3)
         (assert-equal? (hash-bag-count bag 4) 1)
         (assert-equal? (hash-bag-count bag 9) 0)))


   (test "hash-bag-count-set! works"
      (let ((bag (hash-bag)))
         (hash-bag-count-set! bag 4 3)
         (assert-equal? (hash-bag-count bag 4) 3)
         (hash-bag-count-set! bag 4 2)
         (assert-equal? (hash-bag-count bag 4) 2)))

      (test "hash-bag-copy works"
      (let* ((bag1 (hash-bag :comparator +number-comparator+ 1 2 1 4 1))
             (bag2 (hash-bag-copy bag1)))
         (assert-true (and (hash-bag-contains? bag2 1)
                           (hash-bag-contains? bag2 2)
                           (hash-bag-contains? bag2 4)))
         (assert-equal? (hash-bag-length bag1)
             (hash-bag-length bag2))
         (hash-bag-delete! bag2 4)
         (assert-false (equal? (hash-bag-length bag1)
                          (hash-bag-length bag2)))
         (assert-false (hash-bag-contains? bag2 4))))

   ;;; bag protocol
   (test "bag-insert! works"
      (let ((bag (make-hash-bag)))
         (assert-false (bag-contains? bag 1))
         (bag-insert! bag 1)
         (assert-true (bag-contains? bag 1))
         (bag-insert! bag 2)
         (assert-true (bag-contains? bag 2))))
      
   (test "bag-delete! works"
      (let ((bag (hash-bag  1 1 2 3)))
         (assert-true (bag-contains? bag 2))
         (bag-delete! bag 2)
         (assert-false (bag-contains? bag 2))
         (bag-delete! bag 1)
         (assert-true (bag-contains?  bag 1))
         (bag-delete! bag 1)
         (assert-false (bag-contains? bag 1))))
      
   (test "bag-empty? works"
      (let ((bag (hash-bag)))
         (assert-true (bag-empty? bag))
         (bag-insert! bag 4)
         (assert-false (bag-empty? bag))
         (bag-delete! bag 4)
         (assert-true (bag-empty? bag))))

      
   (test "bag-contains? works"
      (let ((bag (hash-bag 2 4 5 6)))
         (assert-true (bag-contains? bag 5))
         (assert-true (bag-contains? bag 2))
         (assert-false (bag-contains? bag 8))))

   (test "bag-count works"
      (let ((bag (hash-bag 1 1 1 4 7 11)))
         (assert-equal? (bag-count bag 1) 3)
         (assert-equal? (bag-count bag 4) 1)
         (assert-equal? (bag-count bag 9) 0)))


   (test "bag-count-set! works"
      (let ((bag (hash-bag)))
         (bag-count-set! bag 4 3)
         (assert-equal? (bag-count bag 4) 3)
         (bag-count-set! bag 4 2)
         (assert-equal? (bag-count bag 4) 2)))

   (test "bag-length works"
      (let ((bag (hash-bag 1 1 2 3)))
         (assert-equal? (bag-length bag) 4)
         (bag-delete! bag 1)
         (assert-equal? (bag-length bag) 3)
         (bag-delete! bag 2)
         (assert-equal? (bag-length bag) 2)))

   (test "bag-copy works"
      (let* ((bag1 (hash-bag :comparator +number-comparator+ 1 2 1 4 1))
             (bag2 (bag-copy bag1)))
         (assert-true (and (bag-contains? bag2 1)
                           (bag-contains? bag2 2)
                           (bag-contains? bag2 4)))
         (assert-equal? (bag-length bag1)
             (hash-bag-length bag2))
         (hash-bag-delete! bag2 4)
         (assert-false (equal? (hash-bag-length bag1)
                          (hash-bag-length bag2)))
         (assert-false (hash-bag-contains? bag2 4))))

   
   ;;;; hash-bag-enumerator tests
   (test "hash-bag-enumerator throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((enumer  (make-hash-bag-enumerator (hash-bag 1 1 2 3 4))))
         (assert-exception-thrown (enumerator-current enumer)
            &error)))
   
  
   (test "hash-bag-enumerator immediately returns false when empty"
      (let ((enumer (make-hash-bag-enumerator (hash-bag))))
         (assert-false (enumerator-move-next! enumer))))

   (test "hash-bag-enumerator for (hash-bag 1 1 2 7 3 6 4 )  return 8 items"
      (let ((bag (hash-bag 1 1 2 7 3 6 4 5)))
         (let ((enum (make-hash-bag-enumerator bag)))
            (assert-equal? (let loop ((cont (enumerator-move-next! enum))
                                      (res '()))
                              (if cont
                                  (let ((t (enumerator-current enum))) 
                                     (loop (enumerator-move-next! enum)
                                        (cons t res)))
                                  (length res))) 8))))
   
   (test "cloning enumerators works correctly on hash-bag-enumerators"
      (let* ((bag (hash-bag 1 4 2 3))
             (enumer (make-hash-bag-enumerator bag))
             (cln (enumerator-copy enumer)))
         (assert-equal? (enumerator->list enumer)
            (enumerator->list cln))))

   ;;;; enumerable tests

   (test "hash-bag is an enumerable"
      (assert-true (enumerable? (make-hash-bag)))
      (assert-false (enumerable? 5)))
   
   (test "enumerable-for-each on hash-bags work"
      (let ((count 0))
         (enumerable-for-each  (lambda (e) (set! count (+ count 1))) (hash-bag 1 1 2 3))
         (assert= count 4)))

   (test "enumerable-map on hash-bags work"
      (let ((res (enumerator->list (enumerable-map (lambda (e) (+ e 1)) (hash-bag 1 2 3)))))
         (assert-true (and (collection-contains? res 2)
                           (collection-contains? res 3)
                           (collection-contains? res 4)))))
   
    (test "enumerable-filter on hash-bags work"
       (let ((res (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) (hash-bag 1 2 3)))))
          (assert-true (and (collection-contains? res 2)
                            (collection-contains? res 3)))))

    (test "enumerable-fold on hash-bags work"
      (assert= (enumerable-fold + 0 (hash-bag 1 2 3)) 6))

    (test "enumerable-any? odd? on (hash-bag 1 2 3 4 5) returns #t"
       (assert-equal?  (enumerable-any? odd? (hash-bag 1 2 3 4 5)) #t))
    
    (test "enumerable-every? odd? on (hash-bag 1 2 3 4 5) returns #f"
       (assert-equal?  (enumerable-every? odd? (hash-bag 1 2 3 4 5)) #f))

    (test "enumerable-every? odd? on (hash-bag 1  3  5) returns #t"
       (assert-equal?  (enumerable-every? odd? (hash-bag 1  3  5)) #t))

    (test "enumerable-skip 2 (hash-bag 1 2 3 4 5) yields an enumerator with the first element 3"
       (let ((enum (enumerable-skip 2 (hash-bag 1 2 3 4 5))))
          (assert-equal? (enumerator-current enum) 3)))

    (test "enumerable-skip-while odd? (hash-bag 1 3 5 6 7) yields an enumerator with the first element 6"
       (let ((enum (enumerable-skip-while odd? (hash-bag 1 3 5 6 7))))
          (assert-equal? (enumerator-current enum) 6)))
    
    (test "enumerable-append works on hash-bags"
      (let ((enumer (enumerable-append (hash-bag 1 2 3) (hash-bag 4 5 6))))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))))

    (test "enumerable-take works on hash-bags"
      (assert-equal? (enumerator->list (enumerable-take 2 (hash-bag 1 2 3 4 5)))
         '(1 2)))

    (test "enumerable-take-while works on hash-bags"
       (assert-equal? (enumerator->list (enumerable-take-while odd? (hash-bag 1 3 4 5)))
          '(1 3)))


   ;;;; collection tests    
   (test "a hash-bag is a collection"
      (assert-true (collection? (hash-bag 2 3))))

   (test "(hash-bag 1 2 3) has length 3"
      (assert= (collection-length (hash-bag 1 2 3)) 3))

   (test "collection-contains? works on hash-bag"
      (assert-true (collection-contains? (hash-bag 1 2 3) 1)))

   (test "collection-empty? works on an empty hash-bag"
      (assert-true (collection-empty? (hash-bag))))

   (test "collection-copy works"
      (let* ((bag1 (hash-bag :comparator +number-comparator+ 1 2 1 4 1))
             (bag2 (collection-copy bag1)))
         (assert-true (and (collection-contains? bag2 1)
                           (collection-contains? bag2 2)
                           (collection-contains? bag2 4)))
         (assert-equal? (collection-length bag1)
             (collection-length bag2))
         (bag-delete! bag2 4)
         (assert-false (equal? (collection-length bag1)
                          (collection-length bag2)))
         (assert-false (collection-contains? bag2 4))))

   ;;;; hash-bag is mutable
   (test "hash-bag is mutable"
      (assert-true (collection-mutable? (hash-bag))))


   ;;;; hash-bag extendable tests
   (test "hash-bag is extendable"
      (assert-true (collection-extendable? (hash-bag))))

   (test "collection-extend! works on hash-bags"
      (let ((bag (hash-bag)))
         (assert-false (hash-bag-contains? bag 1))
         (collection-extend! bag 1)
         (assert-true (hash-bag-contains?  bag 1))))
   
   )

   