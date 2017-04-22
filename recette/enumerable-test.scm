(module enumerable-test
   (library btest hoard)
   (export enumerable-tests))

(define-test-suite enumerable-tests

   (test "enumerable-for-each on lists works"
      (let ((count 0))
         (enumerable-for-each  (lambda (e) (set! count (+ count 1))) '(1 2 3))
         (assert= count 3)))

   (test "enumerable-for-each on vectors works"
      (let ((count 0))
         (enumerable-for-each (lambda (e) (set! count (+ count 1))) '#(1 2 3))
         (assert= count 3)))

   (test "enumerable-for-each on hashtables works"
      (let ((count 0)
            (hash (create-hashtable)))
         (hashtable-put! hash 'a 1)
         (hashtable-put! hash 'b 2)
         (hashtable-put! hash 'c 3)
         (enumerable-for-each (lambda (e) (set! count (+ count 1))) '(1 2 3))
         (assert= count 3)))

   (test "enumerable-for-each on a string works"
      (let ((count 0))
         (enumerable-for-each (lambda (e) (set! count (+ count 1))) "abc")
         (assert= count 3)))

 
   (test "enumerable-map on lists works"
      (assert-equal? (enumerator->list (enumerable-map (lambda (e) (+ e 1)) '(1 2 3)))
         '(2 3 4)))

   (test "enumerable-map on vectors works"
         (assert-equal? (enumerator->list (enumerable-map (lambda (e) (+ e 1)) '#(1 2 3))) '(2 3 4)))

   (test "enumerable-map on hashtables works"
      (let ((hash (create-hashtable)))
         (hashtable-put! hash 'a 1)
         (hashtable-put! hash 'b 2)
         (hashtable-put! hash 'c 3)
         (assert-equal? (enumerator->list (enumerable-map (lambda (e) (+ e 1)) hash))
            '(4 3 2))))
            
   (test "enumerable-map on a string works"
         (assert-equal? (enumerator->list (enumerable-map (lambda (e) (char-upcase e)) "abc"))
            '(#\A #\B #\C)))


   (test "enumerable-filter on lists works"
      (assert-equal? (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) '(1 2 3)))
         '(2 3 )))

   (test "enumerable-filter on vectors works"
         (assert-equal? (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) '#(1 2 3))) '(2 3)))

   (test "enumerable-filter on hashtables works"
      (let ((hash (create-hashtable))
            (hash2 (create-hashtable)))
         (hashtable-put! hash 'a 1)
         (hashtable-put! hash 'b 2)
         (hashtable-put! hash 'c 3)
         (assert-equal? (enumerator->list (enumerable-filter (lambda (e) (not (=  e 1))) hash))
            '(3 2))))
            
   (test "enumerable-filter on a string works"
         (assert-equal? (enumerator->list (enumerable-filter (lambda (e) (not (equal? e #\a))) "abc"))
            '(#\b #\c)))

   (test "enumerable-fold on a list works"
      (assert= (enumerable-fold + 0 '(1 2 3) ) 6))


   (test "enumerable-fold on a vector works"
      (assert= (enumerable-fold + 0 '#(1 2 3) ) 6))

   (test "enumerable-fold on hashtable works"
    (let ((hash (create-hashtable)))
       (hashtable-put! hash 'a 1)
       (hashtable-put! hash 'b 2)
       (hashtable-put! hash 'c 3)
       (assert= (enumerable-fold + 0 hash) 6)))

   (test "enumerable-fold on a string works"
      (assert= (enumerable-fold (lambda (s v)
                                         (let ((i (- (char->integer v) (char->integer #\a))))
                                            (+ i s))) 0 "abc")
         3))


   (test "enumerable-any? odd? on '(1 2 3 4 5) returns #t"
      (assert-equal?  (enumerable-any? odd? '(1 2 3 4 5)) #t))

   (test "enumerable-any? even? on '#(1  3  5) returns #f"
      (assert-equal?  (enumerable-any? even? '(1  3  5)) #f))

   (test "enumerable-any? (lambda (x y z) (if (= (+ x y z) 5) 5)) '(1 2 3) '(1 2 3) '(2 1 3) returns 5"
       (assert-equal? (enumerable-any? (lambda (x y z) (if (= (+ x y z) 5) 5 #f)) '(1 2 3) '(1 2 3) '(2 1 3)) 5))

   (test "enumerable-every? odd? on '(1 2 3 4 5) returns #f"
      (assert-equal?  (enumerable-every? odd? '(1 2 3 4 5)) #f))

   (test "enumerable-every? odd? on '#(1  3  5) returns #t"
      (assert-equal?  (enumerable-every? odd? '(1  3  5)) #t))

   (test "enumerable-skip 2 '(1 2 3 4 5) yields an enumerator with the first element 3"
      (let ((enum (enumerable-skip 2 '(1 2 3 4 5))))
         (assert-equal? (enumerator-current enum) 3)))

   (test "enumerable-skip 2 '#(1 2 3 4 5) yields an enumerator with the first element 3"
      (let ((enum (enumerable-skip 2 '#(1 2 3 4 5))))
         (assert-equal? (enumerator-current enum) 3)))

   (test "enumerable-skip 2 \"abcde\" yields an enumerator with the first element #\c"
      (let ((enum (enumerable-skip 2 "abcde")))
         (assert-equal? (enumerator-current enum) #\c)))

    (test "enumerable-skip 2 '(1 2 3 4 5) '(6 7 8 9 10) yields an enumerator with the first element (3 8)"
       (let ((enum (enumerable-skip 2 '(1 2 3 4 5) '(6 7 8 9 10))))
          (assert-equal? (enumerator-current enum) '(3 8))))

   (test "enumerable-skip 2 '#(1 2 3 4 5) '#(6 7 8 9 10) yields an enumerator with the first element (3 8)"
      (let ((enum (enumerable-skip 2 '#(1 2 3 4 5) '#(6 7 8 9 10))))
         (assert-equal? (enumerator-current enum) '(3 8))))

   (test "enumerable-skip 2 \"abcde\" \"fghij\" yields an enumerator with the first element (#\c #\h)"
      (let ((enum (enumerable-skip 2 "abcde" "fghij")))
         (assert-equal? (enumerator-current enum) '(#\c #\h))))

   (test "enumerable-skip-while odd? '(1 3 5 6 7) yields an enumerator with the first element 6"
      (let ((enum (enumerable-skip-while odd? '(1 3 5 6 7))))
         (assert-equal? (enumerator-current enum) 6)))

   (test "enumerable-skip-while odd? '#(1 3 5 6 7) yields an enumerator with the first element 6"
      (let ((enum (enumerable-skip-while odd? '#(1 3 5 6 7))))
         (assert-equal? (enumerator-current enum) 6)))
   
   (test "enumerable-skip-while char-upper-case? \"ABCdefg\" yields an enumerator with the first element #\d"
      (let ((enum (enumerable-skip-while char-upper-case? "ABCdefg")))
         (assert-equal? (enumerator-current enum) #\d)))

   (test "enumerable-append works on lists"
      (let ((enumer (enumerable-append (list 1 2 3) (list 4 5 6))))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))))
   
   (test "enumerable-append works on vectors"
      (let ((enumer (enumerable-append (vector 1 2 3) (vector 4 5 6))))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))))

   (test "enumerable-append works on lists"
      (let ((enumer (enumerable-append (list 1 2 3) (list 4 5 6))))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))))

   (test "enumerable-append works on strings"
      (let ((enumer (enumerable-append "dog" "food")))
         (assert-equal? (enumerator->list enumer) '(#\d #\o #\g #\f #\o #\o #\d))))

   (test "enumerable-append works on a mixture of lists and vectors"
      (let ((enumer (enumerable-append (list 1 2 3) (vector 4 5 6))))
         (assert-equal? (enumerator->list enumer) '(1 2 3 4 5 6))))

   (test "enumerable-take works on lists"
      (assert-equal? (enumerator->list (enumerable-take 2 '(1 2 3 4 5)))
         '(1 2)))

   (test "enumerable-take works on vectors"
      (assert-equal? (enumerator->list (enumerable-take 2 '#(1 2 3 4 5)))
         '(1 2)))

   (test "enumerable-take works on strings"
      (assert-equal? (enumerator->list (enumerable-take 2 "abcde"))
         '(#\a  #\b)))

   (test "enumerable-take-while works on lists"
      (assert-equal? (enumerator->list (enumerable-take-while odd? '(1 3 4 5)))
         '(1 3)))

   (test "enumerable-take-while works on vectors"
      (assert-equal? (enumerator->list (enumerable-take-while odd? '#(1 3 4 5)))
         '(1 3)))

   (test "enumerable-take-while works on strings"
      (assert-equal? (enumerator->list (enumerable-take-while char-lower-case?  "abCDE"))
         '(#\a  #\b)))

   (test "enumerable-collect works"
      (assert-equal? (enumerable-collect (vector 1 2 3 4) +stretchy-vector-collector+)
         (stretchy-vector 1 2 3 4)))

   (test "enumerable-collect works with +sum-collector+"
      (assert-equal? (enumerable-collect (vector 1 2 3 4) +sum-collector+)
         10)
      (assert-equal? (enumerable-collect (stretchy-vector 1 2 3 4) +sum-collector+)
         10)
      (assert-equal? (enumerable-collect (list 1 2 3 4) +sum-collector+)
         10)))





