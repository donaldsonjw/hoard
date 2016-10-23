(module enumerator-test
   (library btest hoard)
   (export enumerator-tests))


(define-test-suite enumerator-tests

   (test "list-enumerator throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((lst-enum (collection-enumerator (list))))
         (assert-exception-thrown (enumerator-current lst-enum)
            &error)))
   
   (test "list-enumerator for immediately returns false on empty lst"
      (let ((lst-enum (collection-enumerator (list))))
         (assert-false (enumerator-move-next! lst-enum))))

   (test "list-enumerator for '#(1 2 3) return 3 items"
      (let ((lst-enum (collection-enumerator (list 1 2 3))))
         (assert-equal? (let loop ((cont (enumerator-move-next! lst-enum))
                                   (res '()))
                           (if cont
                               (let ((t (enumerator-current lst-enum))) 
                                  (loop (enumerator-move-next! lst-enum)
                                     (cons t res)))
                               res)) (list 3 2 1))))

   (test "vector-enumerator throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((vec-enum (collection-enumerator (vector))))
         (assert-exception-thrown (enumerator-current vec-enum)
            &error)))
   
   
   (test "vector-enumerator for immediately returns false on empty vector"
      (let ((vec-enum (collection-enumerator (vector))))
         (assert-false (enumerator-move-next! vec-enum))))

   (test "vector-enumerator for '#(1 2 3) return 3 items"
      (let ((vec-enum (collection-enumerator (vector 1 2 3))))
         (assert-equal? (let loop ((cont (enumerator-move-next! vec-enum))
                                   (res '()))
                           (if cont
                               (let ((t (enumerator-current vec-enum))) 
                                  (loop (enumerator-move-next! vec-enum)
                                     (cons t res)))
                               res)) (list 3 2 1))))


   (test "string-enumerator throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((str-enum (collection-enumerator (string))))
         (assert-exception-thrown (enumerator-current str-enum)
            &error)))
   
  
   (test "string-enumerator for string immediately returns false on empty string"
      (let ((str-enum (collection-enumerator (string))))
         (assert-false (enumerator-move-next! str-enum))))

   (test "string-enumerator for \"abc\" return 3 items"
      (let ((str-enum (collection-enumerator "abc")))
         (assert-equal? (let loop ((cont (enumerator-move-next! str-enum))
                                   (res '()))
                           (if cont
                               (let ((t (enumerator-current str-enum))) 
                                  (loop (enumerator-move-next! str-enum)
                                     (cons t res)))
                               res)) (list #\c #\b #\a))))


    (test "hashtable-enumerator throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((hash-enum (collection-enumerator (create-hashtable))))
         (assert-exception-thrown (enumerator-current hash-enum)
            &error)))
   
  
   (test "hashtable-enumerator for hashtable immediately returns false on empty hashtable"
      (let ((hash-enum (collection-enumerator (create-hashtable))))
         (assert-false (enumerator-move-next! hash-enum))))

   (test "hash-enumerator for (hash (a . 1) (b . 2) (c . 3))  return 3 items"
       (let ((hash (create-hashtable)))
         (hashtable-put! hash 'a 1)
         (hashtable-put! hash 'b 2)
         (hashtable-put! hash 'c 3)
         (let ((hash-enum (collection-enumerator hash)))
            (assert-equal? (let loop ((cont (enumerator-move-next! hash-enum))
                                      (res '()))
                              (if cont
                                  (let ((t (enumerator-current hash-enum))) 
                                     (loop (enumerator-move-next! hash-enum)
                                        (cons t res)))
                                  res)) (list 1 2 3)))))
   
   (test "cloning enumerators works correctly on list-enumerators"
      (let* ((enumer (get-enumer (list 1 2 3 4)))
             (cln (enumerator-clone enumer)))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4))
         (assert-equal? (enumerator->list cln) '(1 2 3 4))))

   (test "cloning filter enumerators works correctly"
      (let* ((enumer (enumerable-filter (lambda (x) (< x 2)) (list 1 2 3 4)))
             (cln (enumerator-clone enumer)))
         (assert-equal? (enumerator->list enumer) '(1))
         (assert-equal? (enumerator->list cln) '(1))))
   
   
   (test "cloning append enumerators works correctly"
      (let* ((enumer (enumerable-append (list 1 2 3) (list 4 5 6)))
             (cln (enumerator-clone enumer)))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))
         (assert-equal? (enumerator->list cln) '(1 2 3 4 5 6))))

   (test "cloning map enumerators works correctly"
      (let* ((enumer (enumerable-map (lambda (x) (+ x 2)) (list 1 2 3)))
             (cln (enumerator-clone enumer)))
         (assert-equal? (enumerator->list enumer) '(3 4 5))
         (assert-equal? (enumerator->list cln) '(3 4 5))))
   
   
   (test "filtering a cloned enumerator works"
      (let* ((enumer (get-enumer (list 4 2 3 1)))
             (cln (enumerator-clone enumer)))
         (assert-equal? (enumerator->list (enumerable-filter (lambda (x) (<= x 2)) enumer)) '(2 1))
         (assert-equal? (enumerator->list (enumerable-filter (lambda (x) (> x 2)) cln)) '(4 3))))


   (test "enumerator->list works"
      (assert-equal? (enumerator->list (get-enumer (list 1 2 3)))
         '(1 2 3)))

   (test "enumerator->vector works"
      (assert-equal? (enumerator->vector (get-enumer (list 1 2 3)))
         '#(1 2 3)))

   (test "enumerator->string works"
      (assert-equal? (enumerator->string (get-enumer "abc"))
         "abc"))


         

   )




