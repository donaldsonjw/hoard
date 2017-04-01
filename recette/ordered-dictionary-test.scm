(module ordered-dictionary-test
   (library btest
            hoard)
   (export ordered-dictionary-tests))

(define-test-suite ordered-dictionary-tests

   (test "ordered-dictionary? works"
      (assert-true (ordered-dictionary? (make-ordered-dictionary :comparator +string-comparator+)))
      (assert-false (ordered-dictionary? (list)))
      (assert-false (ordered-dictionary? (create-hashtable))))

   (test "ordered-dictionary works"
      (let ((dict (ordered-dictionary :comparator +string-comparator+ (=> "a" 1) (=> "b" 2) (=> "c" 3))))
         (assert-true (ordered-dictionary? dict))))

   (test "ordered-dictionary-get works"
      (let ((dict (ordered-dictionary :comparator +string-comparator+ (=> "a" 1) (=> "b" 2) (=> "c" 3))))
         (assert-equal? (ordered-dictionary-get dict "a") 1)
         (assert-equal? (ordered-dictionary-get dict "b") 2)
         (assert-equal? (ordered-dictionary-get dict "c") 3)
         (assert-false (ordered-dictionary-get dict "d"))))

   (test "ordered-dictionary-put! works"
      (let ((dict (ordered-dictionary :comparator +string-comparator+)))
         (ordered-dictionary-put! dict "a" 1)
         (assert-equal? (ordered-dictionary-get dict "a") 1)
         (ordered-dictionary-put! dict "b" 2)
         (assert-equal? (ordered-dictionary-get dict "b") 2)
         (ordered-dictionary-put! dict "c" 3)
         (assert-equal? (ordered-dictionary-get dict "c") 3)
         (assert-false (ordered-dictionary-get dict "d"))))

   (test "ordered-dictionary-remove! works"
      (let ((dict (ordered-dictionary :comparator +string-comparator+ (=> "a" 1) (=> "b" 2) (=> "c" 3))))
         (assert-true (ordered-dictionary-contains? dict "a"))
         (ordered-dictionary-remove! dict "a")
         (assert-false (ordered-dictionary-contains? dict "a"))
         (assert-true (ordered-dictionary-contains? dict "b"))
         (ordered-dictionary-remove! dict "b")
         (assert-false (ordered-dictionary-contains? dict "b"))
         (assert-true (ordered-dictionary-contains? dict "c"))
         (ordered-dictionary-remove! dict "c")
         (assert-false (ordered-dictionary-contains? dict "c"))
         (assert-true (ordered-dictionary-empty? dict))))
         
         
   (test "ordered-dictionary-contains? works"
      (let ((dict (ordered-dictionary :comparator +string-comparator+ (=> "a" 1) (=> "b" 2) (=> "c" 3))))
         (assert-true (ordered-dictionary-contains? dict "a"))
         (assert-true (ordered-dictionary-contains? dict "b"))
         (assert-true (ordered-dictionary-contains? dict "c"))
         (assert-false (ordered-dictionary-contains? dict "d"))))

   (test "ordered-dictionary-empty? works"
      (let ((dict (ordered-dictionary :comparator +string-comparator+)))
         (assert-true (ordered-dictionary-empty? dict))
         (ordered-dictionary-put! dict "a" 1)
         (assert-false (ordered-dictionary-empty? dict))))

   ;;; collection tests
   ;;; need to return to this
   (test "an ordered-dictionary is a collection"
      (assert-true (collection? (ordered-dictionary :comparator +string-comparator+))))

   (test "(ordered-dictionary :comparator +string-comparator+ (=> "a" 1) (=> "b" 2) (=> "c" 3)) has a length of 3"
      (let ((dict (ordered-dictionary :comparator +string-comparator+ (=> "a" 1) (=> "b" 2) (=> "c" 3))))
         (assert-equal? (collection-length dict) 3)))

   (test "collection-contains works on ordered-dictionary"
      (let ((dict (ordered-dictionary :comparator +string-comparator+ (=> "a" 1) (=> "b" 2) (=> "c" 3))))
         (assert-true (collection-contains? dict 1))
         (assert-true (collection-contains? dict 2))
         (assert-true (collection-contains? dict 3))))

   (test "collection-empty? works on ordered-dictionary"
      (let ((dict (ordered-dictionary :comparator +string-comparator+)))
         (assert-true (collection-empty? dict))
         (ordered-dictionary-put! dict "a" 1)
         (assert-false (collection-empty? dict))
         (ordered-dictionary-remove! dict "a")
         (assert-true (collection-empty? dict))))


   ;;; dictionary tests
   (test "dictionary? works"
      (assert-true (dictionary? (ordered-dictionary :comparator +string-comparator+)))
      (assert-false (dictionary? (list))))

   (test "dictionary-get and dictionary-put!  works"
      (let ((dict (ordered-dictionary :comparator +char-comparator+)))
         (dictionary-put! dict #\a 2)
         (assert-equal? (dictionary-get dict #\a) 2)
         (dictionary-put! dict #\b 3)
         (assert-equal? (dictionary-get dict #\b) 3)
         (dictionary-put! dict #\a 1)
         (assert-equal? (dictionary-get dict #\a) 1)))

   (test "dictionary-remove! works"
      (let ((dict (ordered-dictionary :comparator +char-comparator+)))
         (dictionary-put! dict #\a 1)
         (dictionary-put! dict #\b 2)
         (dictionary-put! dict #\c 3)
         (dictionary-remove! dict #\a)
         (assert-false (dictionary-get dict #\a)) 
         (dictionary-remove! dict #\b)
         (assert-false (dictionary-get dict #\b))))

   (test "dictionary-contains? works"
      (let ((dict (ordered-dictionary :comparator +char-comparator+ (=> #\a 1) (=> #\b 2) (=> #\c 3))))
         (assert-true (dictionary-contains? dict #\a))
         (assert-false (dictionary-contains? dict #\d))
         (assert-true (dictionary-contains? dict #\c))))
   
   (test "dictionary-empty? works"
      (let ((dict (ordered-dictionary :comparator +char-comparator+)))
         (assert-true (dictionary-empty? dict))
         (dictionary-put! dict #\a 1)
         (assert-false (dictionary-empty? dict))))

   (test "dictionary-length works"
      (let ((dict (ordered-dictionary :comparator +char-comparator+)))
         (assert-equal? (dictionary-length dict) 0)
         (dictionary-put! dict #\a 1)
         (assert-equal? (dictionary-length dict) 1)
         (dictionary-remove! dict #\a)
         (assert-equal? (dictionary-length dict) 0)))

   ;;; enumerable tests 
   (test "enumerable-for-each on ordered-dictionary works"
      (let ((count 0)
            (dict (ordered-dictionary :comparator +string-comparator+)))
         (ordered-dictionary-put! dict "a" 1)
         (ordered-dictionary-put! dict "b" 2)
         (ordered-dictionary-put! dict "c" 3)
         (enumerable-for-each (lambda (e) (set! count (+ count 1))) '(1 2 3))
         (assert= count 3)))
   
    (test "enumerable-map on ordered-dictionary works"
       (let ((dict (ordered-dictionary :comparator +string-comparator+
                      (=> "a" 1) (=> "b" 2) (=> "c" 3))))
         (assert-equal? (enumerator->list (enumerable-map (lambda (e) (+ e 1)) dict))
            '(2 3 4))))

    (test "enumerable-filter on ordered-dictionary works"
      (let ((dict (ordered-dictionary :comparator +string-comparator+
                     (=> "a" 1) (=> "b" 2) (=> "c" 3))))
         (assert-equal? (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) dict))
            '(2 3))))

    (test "enumerable-fold on orderd-dictionary works"
       (let ((dict (ordered-dictionary :comparator +string-comparator+
                      (=> "a" 1) (=> "b" 2) (=> "c" 3))))
          (assert= (enumerable-fold + 0 dict) 6)))
    
    (test "enumerable-any? works on ordered-dictionary"
       (assert-equal?  (enumerable-any? odd? (ordered-dictionary :comparator +string-comparator+
                                                (=> "a" 1) (=> "b" 2) (=> "c" 3))) #t)
       (assert-equal?  (enumerable-any? odd? (ordered-dictionary :comparator +string-comparator+
                                                (=> "a" 2) (=> "b" 4) (=> "c" 6))) #f))

    (test "enumerable-every? works on ordered-dictionary"
       (assert-equal?  (enumerable-every? odd? (ordered-dictionary :comparator +string-comparator+
                                                (=> "a" 1) (=> "b" 3) (=> "c" 5))) #t)
       (assert-equal?  (enumerable-every? odd? (ordered-dictionary :comparator +string-comparator+
                                                  (=> "a" 2) (=> "b" 5) (=> "c" 7))) #f))

    (test "enumerable-skip works on ordered-dictionary"
       (let* ((dict (ordered-dictionary :comparator +string-comparator+
                      (=> "a" 1) (=> "b" 3) (=> "c" 5)))
              (enum (enumerable-skip 2 dict)))
          (assert-equal? (enumerator-current enum) 5)))

    (test "enumerable-skip-while works on ordered-dictionary"
      (let* ((dict (ordered-dictionary :comparator +string-comparator+
                      (=> "a" 1) (=> "b" 3) (=> "c" 6)))
             (enum (enumerable-skip-while odd? dict)))
         (assert-equal? (enumerator-current enum) 6)))

    (test "enumerable-append works on ordered-dictionaries"
      (let ((enumer (enumerable-append (ordered-dictionary :comparator +string-comparator+
                                          (=> "a" 1) (=> "b" 2) (=> "c" 3))
                       (ordered-dictionary :comparator +string-comparator+
                          (=> "d" 4) (=> "e" 5) (=> "f" 6)))))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))))

    (test "enumerable-take works on ordered-dictionary"
       (assert-equal? (enumerator->list (enumerable-take 2 (ordered-dictionary :comparator +string-comparator+
                                                              (=> "a" 1) (=> "b" 2) (=> "c" 3) (=> "d" 4))))
          '(1 2)))

    (test "enumerable-take-while works on ordered-dictionary"
       (assert-equal? (enumerator->list (enumerable-take-while odd? (ordered-dictionary :comparator +string-comparator+
                                                                       (=> "a" 1) (=> "c" 2) (=> "b" 3) (=> "d" 4))))
          '(1 3)))

    ;;; dictionary enumerable tests
    (test "ordered-dictionary is dictionary-enumerable"
       (assert-true (dictionary-enumerable? (ordered-dictionary :comparator +string-comparator+))))

    (test "dictionary-enumerable-for-each works on ordered-dictionary"
      (let ((dict (ordered-dictionary :comparator +symbol-comparator+ (=> 'a 1) (=> 'b 2) (=> 'c 3))))
         (let ((res '()))
            (dictionary-enumerable-for-each (lambda (k v)
                                               (set! res (cons (list k v) res))) dict)
            (assert-equal? res '((c 3) (b 2) (a 1))))
         ))
    
    (test "dictionary-enumerable-map works on ordered-dictionary"
      (let* ((dict (ordered-dictionary :comparator +symbol-comparator+ (=> 'a 1) (=> 'b 2) (=> 'c 3)))
             (res (dictionary-enumerator->list (dictionary-enumerable-map
                                                  (lambda (k v)
                                                     (=> k (+ v 1))) dict))))
         (assert-equal? res (list (=> 'a 2) (=> 'b 3) (=> 'c  4)))))


       (test "dictionary-enumerable-fold works on hashtables"
          (let* ((dict (ordered-dictionary :comparator +symbol-comparator+ (=> 'a 1) (=> 'b 2) (=> 'c 3)))
                 (res (dictionary-enumerable-fold (lambda (s k v) (+ s v)) 0
                         dict)))
             (assert-equal? res 6)))

       (test "dictionary-enumerable-every? works with ordered-dictionary"
          (let ((dict (ordered-dictionary :comparator +symbol-comparator+ (=> 'a 1) (=> 'b 2) (=> 'c 3))))
             (assert-true (dictionary-enumerable-every? (lambda (k v) (<= v 3)) dict))
             (assert-false (dictionary-enumerable-every? (lambda (k v) (string<? (symbol->string k) "b")) dict))))
       
       (test "dictionary-enumerable-append works on ordered-dictionary"
          (let* ((dict1 (ordered-dictionary :comparator +symbol-comparator+ (=> 'a 1) (=> 'b 2) (=> 'c 3)))
              (dict2 (ordered-dictionary :comparator +symbol-comparator+ (=> 'd 4) (=> 'e 5) (=> 'f 6)))
              (res (dictionary-enumerator->hashtable (dictionary-enumerable-append dict1 dict2))))
          (dictionary-enumerable-for-each (lambda (k v) (assert-equal? (dictionary-get res k) v)) dict1)
          (dictionary-enumerable-for-each (lambda (k v) (assert-equal? (dictionary-get res k) v)) dict2)))

       ;;; extendable collection tests
       (test "ordered-dictionary is extendable"
          (assert-true (collection-extendable? (ordered-dictionary :comparator +symbol-comparator+))))

       (test "collection-extend! works for ordered-dictionary"
          (let ((dict (ordered-dictionary :comparator +symbol-comparator+)))
             (collection-extend! dict (=> 'a 1))
             (collection-extend! dict (=> 'b 2))
             (collection-extend! dict (=> 'c 3))
             (assert-equal? (dictionary-get dict 'a) 1)
             (assert-equal? (dictionary-get dict 'b) 2)
             (assert-equal? (dictionary-get dict 'c) 3)))


       
       
       )