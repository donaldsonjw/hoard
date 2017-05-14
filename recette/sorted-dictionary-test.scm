(module sorted-dictionary-test
   (library btest
            hoard)
   (export sorted-dictionary-tests))

(define-test-suite sorted-dictionary-tests

   (test "sorted-dictionary? works"
      (assert-true (sorted-dictionary? (make-sorted-dictionary :comparator +string-comparator+)))
      (assert-false (sorted-dictionary? (list)))
      (assert-false (sorted-dictionary? (create-hashtable))))

   (test "sorted-dictionary works"
      (let ((dict (sorted-dictionary :comparator +string-comparator+ (=> "a" 1) (=> "b" 2) (=> "c" 3))))
         (assert-true (sorted-dictionary? dict))))

   (test "sorted-dictionary-get works"
      (let ((dict (sorted-dictionary :comparator +string-comparator+ (=> "a" 1) (=> "b" 2) (=> "c" 3))))
         (assert-equal? (sorted-dictionary-get dict "a") 1)
         (assert-equal? (sorted-dictionary-get dict "b") 2)
         (assert-equal? (sorted-dictionary-get dict "c") 3)
         (assert-false (sorted-dictionary-get dict "d"))))

   (test "sorted-dictionary-put! works"
      (let ((dict (sorted-dictionary :comparator +string-comparator+)))
         (sorted-dictionary-put! dict "a" 1)
         (assert-equal? (sorted-dictionary-get dict "a") 1)
         (sorted-dictionary-put! dict "b" 2)
         (assert-equal? (sorted-dictionary-get dict "b") 2)
         (sorted-dictionary-put! dict "c" 3)
         (assert-equal? (sorted-dictionary-get dict "c") 3)
         (assert-false (sorted-dictionary-get dict "d"))))

   (test "sorted-dictionary-update!  works"
      (let ((dict (sorted-dictionary :comparator +number-comparator+)))
         (sorted-dictionary-update! dict 1 2 (lambda (x) (+ x 1)))
         (assert-equal? (sorted-dictionary-get dict 1) 2)
         (sorted-dictionary-update! dict 1 0 (lambda (x) (+ x 1)))
         (assert-equal? (sorted-dictionary-get dict 1) 3)))
   

   (test "sorted-dictionary-remove! works"
      (let ((dict (sorted-dictionary :comparator +string-comparator+ (=> "a" 1) (=> "b" 2) (=> "c" 3))))
         (assert-true (sorted-dictionary-contains? dict "a"))
         (sorted-dictionary-remove! dict "a")
         (assert-false (sorted-dictionary-contains? dict "a"))
         (assert-true (sorted-dictionary-contains? dict "b"))
         (sorted-dictionary-remove! dict "b")
         (assert-false (sorted-dictionary-contains? dict "b"))
         (assert-true (sorted-dictionary-contains? dict "c"))
         (sorted-dictionary-remove! dict "c")
         (assert-false (sorted-dictionary-contains? dict "c"))
         (assert-true (sorted-dictionary-empty? dict))))
         
         
   (test "sorted-dictionary-contains? works"
      (let ((dict (sorted-dictionary :comparator +string-comparator+ (=> "a" 1) (=> "b" 2) (=> "c" 3))))
         (assert-true (sorted-dictionary-contains? dict "a"))
         (assert-true (sorted-dictionary-contains? dict "b"))
         (assert-true (sorted-dictionary-contains? dict "c"))
         (assert-false (sorted-dictionary-contains? dict "d"))))

   (test "sorted-dictionary-empty? works"
      (let ((dict (sorted-dictionary :comparator +string-comparator+)))
         (assert-true (sorted-dictionary-empty? dict))
         (sorted-dictionary-put! dict "a" 1)
         (assert-false (sorted-dictionary-empty? dict))))

   (test "sorted-dictionary-copy works"
      (let* ((dict1 (sorted-dictionary :comparator +string-comparator+ (=> "a" 1) (=> "b" 2) (=> "c" 3)))
             (dict2 (sorted-dictionary-copy dict1)))
         (assert-true (sorted-dictionary-contains? dict2 "a"))
         (assert-true (sorted-dictionary-contains? dict2 "b"))
         (assert-true (sorted-dictionary-contains? dict2 "c"))
         (assert-equal? (sorted-dictionary-length dict1)
            (sorted-dictionary-length dict2))
         (sorted-dictionary-remove! dict2 "a")
         (assert-false (sorted-dictionary-contains? dict2 "a"))
         (assert-false (equal? (sorted-dictionary-length dict2)
                          (sorted-dictionary-length dict1)))))

   ;;; collection tests
   ;;; need to return to this
   (test "an sorted-dictionary is a collection"
      (assert-true (collection? (sorted-dictionary :comparator +string-comparator+))))

   (test "(sorted-dictionary :comparator +string-comparator+ (=> "a" 1) (=> "b" 2) (=> "c" 3)) has a length of 3"
      (let ((dict (sorted-dictionary :comparator +string-comparator+ (=> "a" 1) (=> "b" 2) (=> "c" 3))))
         (assert-equal? (collection-length dict) 3)))

   (test "collection-contains works on sorted-dictionary"
      (let ((dict (sorted-dictionary :comparator +string-comparator+ (=> "a" 1) (=> "b" 2) (=> "c" 3))))
         (assert-true (collection-contains? dict 1))
         (assert-true (collection-contains? dict 2))
         (assert-true (collection-contains? dict 3))))

   (test "collection-empty? works on sorted-dictionary"
      (let ((dict (sorted-dictionary :comparator +string-comparator+)))
         (assert-true (collection-empty? dict))
         (sorted-dictionary-put! dict "a" 1)
         (assert-false (collection-empty? dict))
         (sorted-dictionary-remove! dict "a")
         (assert-true (collection-empty? dict))))

   (test "collection-copy works"
      (let* ((dict1 (sorted-dictionary :comparator +string-comparator+ (=> "a" 1) (=> "b" 2) (=> "c" 3)))
             (dict2 (collection-copy dict1)))
         (assert-true (dictionary-contains? dict2 "a"))
         (assert-true (dictionary-contains? dict2 "b"))
         (assert-true (dictionary-contains? dict2 "c"))
         (assert-equal? (collection-length dict1)
            (collection-length dict2))
         (dictionary-remove! dict2 "a")
         (assert-false (dictionary-contains? dict2 "a"))
         (assert-false (equal? (collection-length dict2)
                          (collection-length dict1)))))


   ;;; dictionary tests
   (test "dictionary? works"
      (assert-true (dictionary? (sorted-dictionary :comparator +string-comparator+)))
      (assert-false (dictionary? (list))))

   (test "dictionary-get and dictionary-put!  works"
      (let ((dict (sorted-dictionary :comparator +char-comparator+)))
         (dictionary-put! dict #\a 2)
         (assert-equal? (dictionary-get dict #\a) 2)
         (dictionary-put! dict #\b 3)
         (assert-equal? (dictionary-get dict #\b) 3)
         (dictionary-put! dict #\a 1)
         (assert-equal? (dictionary-get dict #\a) 1)))

   (test "dictionary-update! works"
      (let ((dict (sorted-dictionary :comparator +number-comparator+)))
         (dictionary-update! dict 1 2 (lambda (x) (+ x 1)))
         (assert-equal? (dictionary-get dict 1) 2)
         (dictionary-update! dict 1 0 (lambda (x) (+ x 1)))
         (assert-equal? (dictionary-get dict 1) 3)))
   

   (test "dictionary-remove! works"
      (let ((dict (sorted-dictionary :comparator +char-comparator+)))
         (dictionary-put! dict #\a 1)
         (dictionary-put! dict #\b 2)
         (dictionary-put! dict #\c 3)
         (dictionary-remove! dict #\a)
         (assert-false (dictionary-get dict #\a)) 
         (dictionary-remove! dict #\b)
         (assert-false (dictionary-get dict #\b))))

   (test "dictionary-contains? works"
      (let ((dict (sorted-dictionary :comparator +char-comparator+ (=> #\a 1) (=> #\b 2) (=> #\c 3))))
         (assert-true (dictionary-contains? dict #\a))
         (assert-false (dictionary-contains? dict #\d))
         (assert-true (dictionary-contains? dict #\c))))
   
   (test "dictionary-empty? works"
      (let ((dict (sorted-dictionary :comparator +char-comparator+)))
         (assert-true (dictionary-empty? dict))
         (dictionary-put! dict #\a 1)
         (assert-false (dictionary-empty? dict))))

   (test "dictionary-length works"
      (let ((dict (sorted-dictionary :comparator +char-comparator+)))
         (assert-equal? (dictionary-length dict) 0)
         (dictionary-put! dict #\a 1)
         (assert-equal? (dictionary-length dict) 1)
         (dictionary-remove! dict #\a)
         (assert-equal? (dictionary-length dict) 0)))

   (test "dictionary-copy works"
      (let* ((dict1 (sorted-dictionary :comparator +string-comparator+ (=> "a" 1) (=> "b" 2) (=> "c" 3)))
             (dict2 (dictionary-copy dict1)))
         (assert-true (dictionary-contains? dict2 "a"))
         (assert-true (dictionary-contains? dict2 "b"))
         (assert-true (dictionary-contains? dict2 "c"))
         (assert-equal? (dictionary-length dict1)
            (dictionary-length dict2))
         (dictionary-remove! dict2 "a")
         (assert-false (dictionary-contains? dict2 "a"))
         (assert-false (equal? (dictionary-length dict2)
                          (dictionary-length dict1)))))


   ;;; enumerable tests 
   (test "enumerable-for-each on sorted-dictionary works"
      (let ((count 0)
            (dict (sorted-dictionary :comparator +string-comparator+)))
         (sorted-dictionary-put! dict "a" 1)
         (sorted-dictionary-put! dict "b" 2)
         (sorted-dictionary-put! dict "c" 3)
         (enumerable-for-each (lambda (e) (set! count (+ count 1))) '(1 2 3))
         (assert= count 3)))
   
    (test "enumerable-map on sorted-dictionary works"
       (let ((dict (sorted-dictionary :comparator +string-comparator+
                      (=> "a" 1) (=> "b" 2) (=> "c" 3))))
         (assert-equal? (enumerator->list (enumerable-map (lambda (e) (+ e 1)) dict))
            '(2 3 4))))

    (test "enumerable-filter on sorted-dictionary works"
      (let ((dict (sorted-dictionary :comparator +string-comparator+
                     (=> "a" 1) (=> "b" 2) (=> "c" 3))))
         (assert-equal? (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) dict))
            '(2 3))))

    (test "enumerable-fold on orderd-dictionary works"
       (let ((dict (sorted-dictionary :comparator +string-comparator+
                      (=> "a" 1) (=> "b" 2) (=> "c" 3))))
          (assert= (enumerable-fold + 0 dict) 6)))
    
    (test "enumerable-any? works on sorted-dictionary"
       (assert-equal?  (enumerable-any? odd? (sorted-dictionary :comparator +string-comparator+
                                                (=> "a" 1) (=> "b" 2) (=> "c" 3))) #t)
       (assert-equal?  (enumerable-any? odd? (sorted-dictionary :comparator +string-comparator+
                                                (=> "a" 2) (=> "b" 4) (=> "c" 6))) #f))

    (test "enumerable-every? works on sorted-dictionary"
       (assert-equal?  (enumerable-every? odd? (sorted-dictionary :comparator +string-comparator+
                                                (=> "a" 1) (=> "b" 3) (=> "c" 5))) #t)
       (assert-equal?  (enumerable-every? odd? (sorted-dictionary :comparator +string-comparator+
                                                  (=> "a" 2) (=> "b" 5) (=> "c" 7))) #f))

    (test "enumerable-skip works on sorted-dictionary"
       (let* ((dict (sorted-dictionary :comparator +string-comparator+
                      (=> "a" 1) (=> "b" 3) (=> "c" 5)))
              (enum (enumerable-skip 2 dict)))
          (assert-equal? (enumerator-current enum) 5)))

    (test "enumerable-skip-while works on sorted-dictionary"
      (let* ((dict (sorted-dictionary :comparator +string-comparator+
                      (=> "a" 1) (=> "b" 3) (=> "c" 6)))
             (enum (enumerable-skip-while odd? dict)))
         (assert-equal? (enumerator-current enum) 6)))

    (test "enumerable-append works on sorted-dictionaries"
      (let ((enumer (enumerable-append (sorted-dictionary :comparator +string-comparator+
                                          (=> "a" 1) (=> "b" 2) (=> "c" 3))
                       (sorted-dictionary :comparator +string-comparator+
                          (=> "d" 4) (=> "e" 5) (=> "f" 6)))))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))))

    (test "enumerable-take works on sorted-dictionary"
       (assert-equal? (enumerator->list (enumerable-take 2 (sorted-dictionary :comparator +string-comparator+
                                                              (=> "a" 1) (=> "b" 2) (=> "c" 3) (=> "d" 4))))
          '(1 2)))

    (test "enumerable-take-while works on sorted-dictionary"
       (assert-equal? (enumerator->list (enumerable-take-while odd? (sorted-dictionary :comparator +string-comparator+
                                                                       (=> "a" 1) (=> "c" 2) (=> "b" 3) (=> "d" 4))))
          '(1 3)))

    ;;; dictionary enumerable tests
    (test "sorted-dictionary is dictionary-enumerable"
       (assert-true (dictionary-enumerable? (sorted-dictionary :comparator +string-comparator+))))

    (test "dictionary-enumerable-for-each works on sorted-dictionary"
      (let ((dict (sorted-dictionary :comparator +symbol-comparator+ (=> 'a 1) (=> 'b 2) (=> 'c 3))))
         (let ((res '()))
            (dictionary-enumerable-for-each (lambda (k v)
                                               (set! res (cons (list k v) res))) dict)
            (assert-equal? res '((c 3) (b 2) (a 1))))
         ))
    
    (test "dictionary-enumerable-map works on sorted-dictionary"
      (let* ((dict (sorted-dictionary :comparator +symbol-comparator+ (=> 'a 1) (=> 'b 2) (=> 'c 3)))
             (res (dictionary-enumerator->list (dictionary-enumerable-map
                                                  (lambda (k v)
                                                     (=> k (+ v 1))) dict))))
         (assert-equal? res (list (=> 'a 2) (=> 'b 3) (=> 'c  4)))))


       (test "dictionary-enumerable-fold works on hashtables"
          (let* ((dict (sorted-dictionary :comparator +symbol-comparator+ (=> 'a 1) (=> 'b 2) (=> 'c 3)))
                 (res (dictionary-enumerable-fold (lambda (s k v) (+ s v)) 0
                         dict)))
             (assert-equal? res 6)))

       (test "dictionary-enumerable-every? works with sorted-dictionary"
          (let ((dict (sorted-dictionary :comparator +symbol-comparator+ (=> 'a 1) (=> 'b 2) (=> 'c 3))))
             (assert-true (dictionary-enumerable-every? (lambda (k v) (<= v 3)) dict))
             (assert-false (dictionary-enumerable-every? (lambda (k v) (string<? (symbol->string k) "b")) dict))))
       
       (test "dictionary-enumerable-append works on sorted-dictionary"
          (let* ((dict1 (sorted-dictionary :comparator +symbol-comparator+ (=> 'a 1) (=> 'b 2) (=> 'c 3)))
              (dict2 (sorted-dictionary :comparator +symbol-comparator+ (=> 'd 4) (=> 'e 5) (=> 'f 6)))
              (res (dictionary-enumerator->hashtable (dictionary-enumerable-append dict1 dict2))))
          (dictionary-enumerable-for-each (lambda (k v) (assert-equal? (dictionary-get res k) v)) dict1)
          (dictionary-enumerable-for-each (lambda (k v) (assert-equal? (dictionary-get res k) v)) dict2)))

       ;;; extendable collection tests
       (test "sorted-dictionary is extendable"
          (assert-true (collection-extendable? (sorted-dictionary :comparator +symbol-comparator+))))

       (test "collection-extend! works for sorted-dictionary"
          (let ((dict (sorted-dictionary :comparator +symbol-comparator+)))
             (collection-extend! dict (=> 'a 1))
             (collection-extend! dict (=> 'b 2))
             (collection-extend! dict (=> 'c 3))
             (assert-equal? (dictionary-get dict 'a) 1)
             (assert-equal? (dictionary-get dict 'b) 2)
             (assert-equal? (dictionary-get dict 'c) 3)))

       ;;; sorted-dictionary indexable methods
       (test "sorted-dictionary is indexable"
          (assert-true (collection-indexable? (sorted-dictionary :comparator +number-comparator+)))
          (assert-false (collection-indexable? 5)))
       
       (test "collection-ref for sorted-dictionary works"
          (let ((dict (sorted-dictionary :comparator +string-comparator+)))
         (collection-set! dict "k1" 1)
         (collection-set! dict "k2" 2)
         (assert-equal? (collection-ref dict "k2")
            2)))

        (test "collection-ref throws exception for invalid index on sorted-dictionary"
           (assert-exception-thrown
              (collection-ref (sorted-dictionary :comparator +number-comparator+) 4) &invalid-index-exception))

        
        (test "collection-set! works on sorted dictionaries"
           (let ((t (sorted-dictionary :comparator +string-comparator+)))
              (collection-set! t "key" 5)
              (collection-set! t "key" 4)
              (assert-equal? (collection-ref t "key") 4)))


        (test "collection-slice works on sorted dictionaries"
           (let ((dict (sorted-dictionary :comparator +string-comparator+)))
              (collection-set! dict "a" 1)
              (collection-set! dict "b" 2)
              (collection-set! dict "c" 1)
              (collection-set! dict "d" 1)
              (assert-equal? (enumerator->list (collection-slice dict '("b" "c")))
                 '(2 1))))
        
       
       )