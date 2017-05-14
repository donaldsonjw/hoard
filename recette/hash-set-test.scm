(module hash-set-test
   (library btest
            hoard)
   (export hash-set-tests))

(define-test-suite hash-set-tests

   (test "hash-set? works"
      (assert-true (hash-set? (hash-set)))
      (assert-false (hash-set? (vector 1 2 3))))

   (test "hash-set works"
      (let ((set (hash-set 1 2 3 4)))
         (assert-true (hash-set-contains? set 1))
         (assert-true (hash-set-contains? set 2))
         (assert-true (hash-set-contains? set 3))
         (assert-true (hash-set-contains? set 4))
         (assert-false (hash-set-contains? set 5))))

   (test "make-hash-set works"
      (let ((set (make-hash-set)))
         (hash-set-insert! set 1)
         (hash-set-insert! set 2)
         (hash-set-insert! set 3)
         (hash-set-insert! set 4)
         (assert-true (hash-set-contains? set 1))
         (assert-true (hash-set-contains? set 2))
         (assert-true (hash-set-contains? set 3))
         (assert-true (hash-set-contains? set 4))
         (assert-false (hash-set-contains? set 5))))

   (test "hash-set with explicit comparator works"
      (let ((set (hash-set :comparator +string-ci-comparator+ "A" "b" "C")))
         (assert-true (hash-set-contains? set "a"))
         (assert-true (hash-set-contains? set "B"))
         (assert-true (hash-set-contains? set "C"))
         (assert-false (hash-set-contains? set "d"))))
         

   (test "hash-set-insert! works"
      (let ((set (hash-set :comparator +number-comparator+)))
         (hash-set-insert! set 4)
         (assert-true (hash-set-contains? set 4))
         (hash-set-insert! set 5)
         (assert-true (hash-set-contains? set 5))))


   (test "hash-set-delete! works"
      (let ((set (hash-set :comparator +number-comparator+ 1 2 2)))
         (assert-true (hash-set-contains? set 2))
         (hash-set-delete! set 2)
         (assert-false (hash-set-contains? set 2))
         (assert-true (hash-set-contains? set 1))
         (hash-set-delete! set 1)
         (assert-false (hash-set-contains? set 1))))

   (test "hash-set-length works"
      (let ((set (hash-set :comparator +number-comparator+ 1 2 2 2 3)))
         (assert-equal? (hash-set-length set) 3)))


   (test "hash-set-empty? works"
      (let ((set (hash-set :comparator +number-comparator+)))
         (assert-true (hash-set-empty? set))
         (hash-set-insert! set 4)
         (assert-false (hash-set-empty? set))))

   (test "hash-set-copy works"
      (let* ((s1 (hash-set :comparator +number-comparator+ 1 2 3 4))
            (s2 (hash-set-copy s1)))
         (assert-true (and (hash-set-contains? s2 1)
                           (hash-set-contains? s2 2)
                           (hash-set-contains? s2 3)
                           (hash-set-contains? s2 4)))
         (assert-equal? (hash-set-length s1)
            (hash-set-length s2))
         (hash-set-delete! s2 1)
         (assert-false (equal? (hash-set-length s1)
                          (hash-set-length s2)))
         (assert-false (hash-set-contains? s2 1))))


   ;;; set protocol tests

   (test "set? works"
      (assert-true (set? (hash-set)))
      (assert-false (set? (vector 1 2 3))))

   (test "set-insert! works"
      (let ((set (hash-set :comparator +number-comparator+)))
         (set-insert! set 4)
         (assert-true (set-contains? set 4))
         (set-insert! set 5)
         (assert-true (set-contains? set 5))))


   (test "set-delete! works"
      (let ((set (hash-set :comparator +number-comparator+ 1 2 2)))
         (assert-true (set-contains? set 2))
         (set-delete! set 2)
         (assert-false (set-contains? set 2))
         (assert-true (set-contains? set 1))
         (set-delete! set 1)
         (assert-false (set-contains? set 1))))

   (test "set-length works"
      (let ((set (hash-set :comparator +number-comparator+ 1 2 2 2 3)))
         (assert-equal? (set-length set) 3)))


   (test "set-empty? works"
      (let ((set (hash-set :comparator +number-comparator+)))
         (assert-true (set-empty? set))
         (set-insert! set 4)
         (assert-false (set-empty? set))))


   (test "set-copy works"
      (let* ((s1 (hash-set :comparator +number-comparator+ 1 2 3 4))
            (s2 (set-copy s1)))
         (assert-true (and (set-contains? s2 1)
                           (set-contains? s2 2)
                           (set-contains? s2 3)
                           (set-contains? s2 4)))
         (assert-equal? (set-length s1)
            (set-length s2))
         (set-delete! s2 1)
         (assert-false (equal? (set-length s1)
                          (set-length s2)))
         (assert-false (set-contains? s2 1))))


  
   (test "set-union! works"
      (let* ((set1 (hash-set :comparator +number-comparator+ 1 2 3))
             (set2 (hash-set :comparator +number-comparator+ 2 3 4)))
         (set-union! set1 set2)
         (assert-equal? (set-length set1) 4)
         (assert-true (and (set-contains? set1 1)
                           (set-contains? set1 2)
                           (set-contains? set1 3)
                           (set-contains? set1 4)))
         (assert-equal? (set-length set2) 3)))
   

   (test "set-union works"
      (let* ((set1 (hash-set :comparator +number-comparator+ 1 2 3))
             (set2 (hash-set :comparator +number-comparator+ 2 3 4))
             (set3 (set-union set1 set2)))
         (assert-equal? (set-length set3) 4)
         (assert-true (and (set-contains? set3 1)
                           (set-contains? set3 2)
                           (set-contains? set3 3)
                           (set-contains? set3 4)))
         (assert-equal? (set-length set1) 3)
         (assert-equal? (set-length set2) 3)))


   (test "set-intersect! works"
      (let* ((set1 (hash-set :comparator +number-comparator+ 1 2 3))
             (set2 (hash-set :comparator +number-comparator+ 2 3 4)))
         (set-intersect! set1 set2)
         (assert-equal? (set-length set1) 2)
         (assert-true (and (set-contains? set1 2)
                           (set-contains? set1 3)))
         (assert-equal? (set-length set2) 3)))
   

   (test "set-intersect works"
      (let* ((set1 (hash-set :comparator +number-comparator+ 1 2 3))
             (set2 (hash-set :comparator +number-comparator+ 2 3 4))
             (set3 (set-intersect set1 set2)))
         (assert-equal? (set-length set3) 2)
         (assert-true (and (set-contains? set3 2)
                           (set-contains? set3 3)))
         (assert-equal? (set-length set1) 3)
         (assert-equal? (set-length set2) 3)))

   (test "set-difference! works"
      (let* ((set1 (hash-set :comparator +number-comparator+ 1 2 3))
             (set2 (hash-set :comparator +number-comparator+ 2 3 4)))
         (set-difference! set1 set2)
         (assert-equal? (set-length set1) 1)
         (assert-true (set-contains? set1 1))
         (assert-equal? (set-length set2) 3)))
   

   (test "set-difference works"
      (let* ((set1 (hash-set :comparator +number-comparator+ 1 2 3))
             (set2 (hash-set :comparator +number-comparator+ 2 3 4))
             (set3 (set-difference set1 set2)))
         (assert-equal? (set-length set3) 1)
         (assert-true (set-contains? set3 1))
         (assert-equal? (set-length set1) 3)
         (assert-equal? (set-length set2) 3)))

   
   ;;;; hash-set-enumerator tests
   (test "hash-set-enumerator throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((enumer  (make-hash-set-enumerator (hash-set 1 1 2 3 4))))
         (assert-exception-thrown (enumerator-current enumer)
            &error)))
   
  
   (test "hash-set-enumerator immediately returns false when empty"
      (let ((enumer (make-hash-set-enumerator (hash-set))))
         (assert-false (enumerator-move-next! enumer))))

   (test "hash-bag-enumerator for (hash-set 1 1 2 7 3 6 4 )  return 8 items"
      (let ((bag (hash-set 1 1 2 7 3 6 4 5)))
         (let ((enum (make-hash-set-enumerator bag)))
            (assert-equal? (let loop ((cont (enumerator-move-next! enum))
                                      (res '()))
                              (if cont
                                  (let ((t (enumerator-current enum))) 
                                     (loop (enumerator-move-next! enum)
                                        (cons t res)))
                                  (length res))) 7))))
   
   (test "cloning enumerators works correctly on hash-bag-enumerators"
      (let* ((bag (hash-set 1 4 2 3))
             (enumer (make-hash-set-enumerator bag))
             (cln (enumerator-copy enumer)))
         (assert-equal? (enumerator->list enumer)
            (enumerator->list cln))))


   ;;;; enumerable tests
   
   (test "hash-set is an enumerable"
      (assert-true (enumerable? (make-hash-set)))
      (assert-false (enumerable? 5)))
   
   (test "enumerable-for-each on hash-sets work"
      (let ((count 0))
         (enumerable-for-each  (lambda (e) (set! count (+ count 1))) (hash-set 1 1 2 3))
         (assert= count 3)))

   (test "enumerable-map on hash-sets work"
      (let ((res (enumerator->list (enumerable-map (lambda (e) (+ e 1)) (hash-set 1 2 3)))))
         (assert-true (and (collection-contains? res 2)
                           (collection-contains? res 3)
                           (collection-contains? res 4)))))
   
    (test "enumerable-filter on hash-sets work"
       (let ((res (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) (hash-set 1 2 3)))))
          (assert-true (and (collection-contains? res 2)
                            (collection-contains? res 3)))))

    (test "enumerable-fold on hash-sets work"
       (assert= (enumerable-fold + 0 (hash-set 1 2 3)) 6))

    (test "enumerable-any? odd? on (hash-set 1 2 3 4 5) returns #t"
       (assert-equal?  (enumerable-any? odd? (hash-set 1 2 3 4 5)) #t))
    
    (test "enumerable-every? odd? on (hash-set 1 2 3 4 5) returns #f"
       (assert-equal?  (enumerable-every? odd? (hash-set 1 2 3 4 5)) #f))

    (test "enumerable-every? odd? on (hash-set 1  3  5) returns #t"
       (assert-equal?  (enumerable-every? odd? (hash-set 1  3  5)) #t))

    (test "enumerable-skip 2 (hash-set 1 2 3 4 5) yields an enumerator with the first element 3"
       (let ((enum (enumerable-skip 2 (hash-set 1 2 3 4 5))))
          (assert-equal? (enumerator-current enum) 3)))

    (test "enumerable-skip-while odd? (hash-set 1 3 5 6 7) yields an enumerator with the first element 6"
       (let ((enum (enumerable-skip-while odd? (hash-set 1 3 5 6 7))))
          (assert-equal? (enumerator-current enum) 6)))
    
    (test "enumerable-append works on hash-sets"
      (let ((enumer (enumerable-append (hash-set 1 2 3) (hash-set 4 5 6))))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))))

    (test "enumerable-take works on hash-sets"
      (assert-equal? (enumerator->list (enumerable-take 2 (hash-set 1 2 3 4 5)))
         '(1 2)))

    (test "enumerable-take-while works on hash-sets"
       (assert-equal? (enumerator->list (enumerable-take-while odd? (hash-set 1 3 4 5)))
          '(1 3)))

    ;;;; collection tests    
   (test "a hash-set is a collection"
      (assert-true (collection? (hash-set 2 3))))

   (test "(hash-set 1 2 3) has length 3"
      (assert= (collection-length (hash-set 1 2 3)) 3))

   (test "collection-contains? works on hash-set"
      (assert-true (collection-contains? (hash-set 1 2 3) 1)))

   (test "collection-empty? works on an empty hash-set"
      (assert-true (collection-empty? (hash-set))))


   ;;;; hash-set is mutable
   (test "hash-set is mutable"
      (assert-true (collection-mutable? (hash-set))))


   ;;;; hash-set extendable tests
   (test "hash-set is extendable"
      (assert-true (collection-extendable? (hash-set))))

   (test "collection-extend! works on hash-sets"
      (let ((set (hash-set)))
         (assert-false (hash-set-contains? set 1))
         (collection-extend! set 1)
         (assert-true (hash-set-contains?  set 1))))
   
   )