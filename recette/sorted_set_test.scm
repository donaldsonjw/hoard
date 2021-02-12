(module sorted-set-test
   (library btest
            hoard)
   (export sorted-set-tests))



(define-test-suite sorted-set-tests

   (test "sorted-set? works"
      (assert-true (sorted-set? (sorted-set :comparator +number-comparator+)))
      (assert-false (sorted-set? (list 1 2 3))))


   (test "sorted-set works"
      (let ((set (sorted-set :comparator +number-comparator+ 1 2 3 4)))
         (assert-true (sorted-set-contains? set 1))
         (assert-true (sorted-set-contains? set 2))
         (assert-true (sorted-set-contains? set 3))
         (assert-true (sorted-set-contains? set 4))
         (assert-false (sorted-set-contains? set 5))))

   (test "make-sorted-set works"
      (let ((set (make-sorted-set :comparator +number-comparator+)))
         (sorted-set-insert! set 1)
         (sorted-set-insert! set 2)
         (sorted-set-insert! set 3)
         (sorted-set-insert! set 4)
         (assert-true (sorted-set-contains? set 1))
         (assert-true (sorted-set-contains? set 2))
         (assert-true (sorted-set-contains? set 3))
         (assert-true (sorted-set-contains? set 4))
         (assert-false (sorted-set-contains? set 5))))

   (test "sorted-set with explicit comparator works"
      (let ((set (sorted-set :comparator +string-ci-comparator+ "A" "b" "C")))
         (assert-true (sorted-set-contains? set "a"))
         (assert-true (sorted-set-contains? set "B"))
         (assert-true (sorted-set-contains? set "C"))
         (assert-false (sorted-set-contains? set "d"))))
         

   (test "sorted-set-insert! works"
      (let ((set (sorted-set :comparator +number-comparator+)))
         (sorted-set-insert! set 4)
         (assert-true (sorted-set-contains? set 4))
         (sorted-set-insert! set 5)
         (assert-true (sorted-set-contains? set 5))))


   (test "sorted-set-delete! works"
      (let ((set (sorted-set :comparator +number-comparator+ 1 2 2)))
         (assert-true (sorted-set-contains? set 2))
         (sorted-set-delete! set 2)
         (assert-false (sorted-set-contains? set 2))
         (assert-true (sorted-set-contains? set 1))
         (sorted-set-delete! set 1)
         (assert-false (sorted-set-contains? set 1))))

   (test "sorted-set-length works"
      (let ((set (sorted-set :comparator +number-comparator+ 1 2 2 2 3)))
         (assert-equal? (sorted-set-length set) 3)))


   (test "sorted-set-empty? works"
      (let ((set (sorted-set :comparator +number-comparator+)))
         (assert-true (sorted-set-empty? set))
         (sorted-set-insert! set 4)
         (assert-false (sorted-set-empty? set))))

   (test "sorted-set-copy works"
      (let* ((s1 (sorted-set :comparator +number-comparator+ 1 2 3 4))
            (s2 (sorted-set-copy s1)))
         (assert-true (and (sorted-set-contains? s2 1)
                           (sorted-set-contains? s2 2)
                           (sorted-set-contains? s2 3)
                           (sorted-set-contains? s2 4)))
         (assert-equal? (sorted-set-length s1)
            (sorted-set-length s2))
         (sorted-set-delete! s2 1)
         (assert-false (equal? (sorted-set-length s1)
                          (sorted-set-length s2)))
         (assert-false (sorted-set-contains? s2 1))))

   ;;; set protocol tests
   (test "set? works"
      (assert-true (set? (sorted-set :comparator +number-comparator+)))
      (assert-false (set? (vector 1 2 3))))

   (test "set-insert! works"
      (let ((set (sorted-set :comparator +number-comparator+)))
         (set-insert! set 4)
         (assert-true (set-contains? set 4))
         (set-insert! set 5)
         (assert-true (set-contains? set 5))))


   (test "set-delete! works"
      (let ((set (sorted-set :comparator +number-comparator+ 1 2 2)))
         (assert-true (set-contains? set 2))
         (set-delete! set 2)
         (assert-false (set-contains? set 2))
         (assert-true (set-contains? set 1))
         (set-delete! set 1)
         (assert-false (set-contains? set 1))))

   (test "set-length works"
      (let ((set (sorted-set :comparator +number-comparator+ 1 2 2 2 3)))
         (assert-equal? (set-length set) 3)))


   (test "set-empty? works"
      (let ((set (sorted-set :comparator +number-comparator+)))
         (assert-true (set-empty? set))
         (set-insert! set 4)
         (assert-false (set-empty? set))))


   (test "set-copy works"
      (let* ((s1 (sorted-set :comparator +number-comparator+ 1 2 3 4))
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
      (let* ((set1 (sorted-set :comparator +number-comparator+ 1 2 3))
             (set2 (sorted-set :comparator +number-comparator+ 2 3 4)))
         (set-union! set1 set2)
         (assert-equal? (set-length set1) 4)
         (assert-true (and (set-contains? set1 1)
                           (set-contains? set1 2)
                           (set-contains? set1 3)
                           (set-contains? set1 4)))
         (assert-equal? (set-length set2) 3)))
   

   (test "set-union works"
      (let* ((set1 (sorted-set :comparator +number-comparator+ 1 2 3))
             (set2 (sorted-set :comparator +number-comparator+ 2 3 4))
             (set3 (set-union set1 set2)))
         (assert-equal? (set-length set3) 4)
         (assert-true (and (set-contains? set3 1)
                           (set-contains? set3 2)
                           (set-contains? set3 3)
                           (set-contains? set3 4)))
         (assert-equal? (set-length set1) 3)
         (assert-equal? (set-length set2) 3)))


   (test "set-intersect! works"
      (let* ((set1 (sorted-set :comparator +number-comparator+ 1 2 3))
             (set2 (sorted-set :comparator +number-comparator+ 2 3 4)))
         (set-intersect! set1 set2)
         (assert-equal? (set-length set1) 2)
         (assert-true (and (set-contains? set1 2)
                           (set-contains? set1 3)))
         (assert-equal? (set-length set2) 3)))
   

   (test "set-intersect works"
      (let* ((set1 (sorted-set :comparator +number-comparator+ 1 2 3))
             (set2 (sorted-set :comparator +number-comparator+ 2 3 4))
             (set3 (set-intersect set1 set2)))
         (assert-equal? (set-length set3) 2)
         (assert-true (and (set-contains? set3 2)
                           (set-contains? set3 3)))
         (assert-equal? (set-length set1) 3)
         (assert-equal? (set-length set2) 3)))

   (test "set-difference! works"
      (let* ((set1 (sorted-set :comparator +number-comparator+ 1 2 3))
             (set2 (sorted-set :comparator +number-comparator+ 2 3 4)))
         (set-difference! set1 set2)
         (assert-equal? (set-length set1) 1)
         (assert-true (set-contains? set1 1))
         (assert-equal? (set-length set2) 3)))
   

   (test "set-difference works"
      (let* ((set1 (sorted-set :comparator +number-comparator+ 1 2 3))
             (set2 (sorted-set :comparator +number-comparator+ 2 3 4))
             (set3 (set-difference set1 set2)))
         (assert-equal? (set-length set3) 1)
         (assert-true (set-contains? set3 1))
         (assert-equal? (set-length set1) 3)
         (assert-equal? (set-length set2) 3)))
   
   ;;;; sorted-set-enumerator tests
   (test "sorted-set-enumerator throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((enumer  (make-sorted-set-enumerator (sorted-set :comparator +number-comparator+ 1 1 2 3 4))))
         (assert-exception-thrown (enumerator-current enumer)
            &error)))
   
  
   (test "sorted-set-enumerator immediately returns false when empty"
      (let ((enumer (make-sorted-set-enumerator (sorted-set :comparator +number-comparator+))))
         (assert-false (enumerator-move-next! enumer))))

   (test "sorted-bag-enumerator for (sorted-set  :comparator +number-comparator+ 1 1 2 7 3 6 4)  return 8 items"
      (let ((bag (sorted-set :comparator +number-comparator+ 1 1 2 7 3 6 4 5)))
         (let ((enum (make-sorted-set-enumerator bag)))
            (assert-equal? (let loop ((cont (enumerator-move-next! enum))
                                      (res '()))
                              (if cont
                                  (let ((t (enumerator-current enum))) 
                                     (loop (enumerator-move-next! enum)
                                        (cons t res)))
                                  (length res))) 7))))
   
   (test "cloning enumerators works correctly on sorted-bag-enumerators"
      (let* ((bag (sorted-set :comparator +number-comparator+ 1 4 2 3))
             (enumer (make-sorted-set-enumerator bag))
             (cln (enumerator-copy enumer)))
         (assert-equal? (enumerator->list enumer)
            (enumerator->list cln))))


   ;;;; enumerable tests
   
   (test "sorted-set is an enumerable"
      (assert-true (enumerable? (make-sorted-set :comparator +number-comparator+)))
      (assert-false (enumerable? 5)))
   
   (test "enumerable-for-each on sorted-sets work"
      (let ((count 0))
         (enumerable-for-each  (lambda (e) (set! count (+ count 1))) (sorted-set :comparator +number-comparator+ 1 1 2 3))
         (assert= count 3)))

   (test "enumerable-map on sorted-sets work"
      (let ((res (enumerator->list (enumerable-map (lambda (e) (+ e 1))
                                      (sorted-set :comparator +number-comparator+ 1 2 3)))))
         (assert-true (and (collection-contains? res 2)
                           (collection-contains? res 3)
                           (collection-contains? res 4)))))
   
    (test "enumerable-filter on sorted-sets work"
       (let ((res (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1)))
                                       (sorted-set :comparator +number-comparator+ 1 2 3)))))
          (assert-true (and (collection-contains? res 2)
                            (collection-contains? res 3)))))

    (test "enumerable-fold on sorted-sets work"
       (assert= (enumerable-fold + 0 (sorted-set :comparator +number-comparator+ 1 2 3)) 6))

    (test "enumerable-any? odd? on (sorted-set :comparator +number-comparator+ 1 2 3 4 5) returns #t"
       (assert-equal?  (enumerable-any? odd? (sorted-set :comparator +number-comparator+ 1 2 3 4 5)) #t))
    
    (test "enumerable-every? odd? on (sorted-set :comparator +number-comparator+ 1 2 3 4 5) returns #f"
       (assert-equal?  (enumerable-every? odd? (sorted-set :comparator +number-comparator+ 1 2 3 4 5)) #f))

    (test "enumerable-every? odd? on (sorted-set :comparator +number-comparator+ 1  3  5) returns #t"
       (assert-equal?  (enumerable-every? odd? (sorted-set :comparator +number-comparator+ 1  3  5)) #t))

    (test "enumerable-skip 2 (sorted-set :comparator +number-comparator+ 1 2 3 4 5) yields an enumerator with the first element 3"
       (let ((enum (enumerable-skip 2 (sorted-set :comparator +number-comparator+ 1 2 3 4 5))))
          (assert-equal? (enumerator-current enum) 3)))

    (test "enumerable-skip-while odd? (sorted-set :comparator +number-comparator+ 1 3 5 6 7) yields an enumerator with the first element 6"
       (let ((enum (enumerable-skip-while odd? (sorted-set :comparator +number-comparator+ 1 3 5 6 7))))
          (assert-equal? (enumerator-current enum) 6)))
    
    (test "enumerable-append works on sorted-sets"
       (let ((enumer (enumerable-append (sorted-set :comparator +number-comparator+ 1 2 3)
                        (sorted-set :comparator +number-comparator+ 4 5 6))))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))))

    (test "enumerable-take works on sorted-sets"
      (assert-equal? (enumerator->list (enumerable-take 2 (sorted-set :comparator +number-comparator+ 1 2 3 4 5)))
         '(1 2)))

    (test "enumerable-take-while works on sorted-sets"
       (assert-equal? (enumerator->list (enumerable-take-while odd?
                                           (sorted-set :comparator +number-comparator+ 1 3 4 5)))
          '(1 3)))

    ;;;; collection tests    
   (test "a sorted-set is a collection"
      (assert-true (collection? (sorted-set :comparator +number-comparator+ 2 3))))

   (test "(sorted-set 1 2 3) has length 3"
      (assert= (collection-length (sorted-set :comparator +number-comparator+ 1 2 3)) 3))

   (test "collection-contains? works on sorted-set"
      (assert-true (collection-contains? (sorted-set :comparator +number-comparator+ 1 2 3) 1)))

   (test "collection-empty? works on an empty sorted-set"
      (assert-true (collection-empty? (sorted-set :comparator +number-comparator+))))


   ;;;; sorted-set is mutable
   (test "sorted-set is mutable"
      (assert-true (collection-mutable? (sorted-set :comparator +number-comparator+))))

   ;;;; sorted-set extendable tests
   (test "sorted-set is extendable"
      (assert-true (collection-extendable? (sorted-set :comparator +number-comparator+))))

   (test "collection-extend! works on sorted-sets"
      (let ((set (sorted-set :comparator +number-comparator+)))
         (assert-false (collection-contains? set 1))
         (collection-extend! set 1)
         (assert-true (collection-contains?  set 1))))
   
)