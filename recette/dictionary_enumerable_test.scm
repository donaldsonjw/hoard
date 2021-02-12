(module dictionary-enumerable-test
   (library btest hoard)
   (export dictionary-enumerable-tests))

(define-test-suite dictionary-enumerable-tests

   (test "hashtable is dictionary-enumerable"
      (assert-true (dictionary-enumerable? (hashtable))))

   (test "vector is dictionary-enumerable"
      (assert-true (dictionary-enumerable? (vector))))

   (test "string is dictionary-enumerable"
      (assert-true (dictionary-enumerable? (string))))
   
   (test "dictionary-enumerable-for-each works on hashtables"
      (let ((dict (hashtable (=> 'a 1) (=> 'b 2) (=> 'c 3))))
         (let ((res '()))
            (dictionary-enumerable-for-each (lambda (k v)
                                               (set! res (cons (list k v) res))) dict)
            (assert-equal? res '((c 3) (b 2) (a 1))))))

   (test "dictionary-enumerable-for-each works on vectors"
      (let ((vec (vector 1 2 3)))
         (let ((res '()))
            (dictionary-enumerable-for-each (lambda (k v)
                                               (set! res (cons (list k v) res))) vec)
            (assert-equal? res '((2 3) (1 2) (0 1))))))

   (test "dictionary-enumerable-for-each works on strings"
      (let ((str "abc"))
         (let ((res '()))
            (dictionary-enumerable-for-each (lambda (k v)
                                               (set! res (cons (list k v) res))) str)
            (assert-equal? res '((2 #\c) (1 #\b) (0 #\a))))))

   (test "dictionary-enumerable-map works on hashtables"
      (let* ((dict (hashtable (=> 'a 1) (=> 'b 2) (=> 'c 3)))
             (res (dictionary-enumerator->list (dictionary-enumerable-map
                                                  (lambda (k v)
                                                     (=> k (+ v 1))) dict))))
         (assert-equal? res (list (=> 'a  2) (=> 'b 3)  (=> 'c 4)))))

   (test "dictionary-enumerable-map works on vectors"
      (let* ((dict (vector 1 2 3))
             (res (dictionary-enumerator->vector (dictionary-enumerable-map
                                                  (lambda (k v)
                                                     (=> k (+ v 1))) dict))))
         (assert-equal? res (vector (=> 0 2) (=> 1 3) (=> 2 4)))))

   (test "dictionary-enumerable-map works on strings"
      (let* ((dict "abc")
             (res (dictionary-enumerator->list (dictionary-enumerable-map
                                                  (lambda (k v)
                                                     (=> k (char-upcase v)))
                                                  dict))))
         (assert-equal? res (list (=> 0 #\A) (=> 1 #\B) (=> 2 #\C)))))


   (test "dictionary-enumerable-filter works on hashtables"
      (let* ((dict (hashtable (=> 'a 1) (=> 'b 2) (=> 'c 3) (=> 'd 4)))
             (res (dictionary-enumerator->list
                     (dictionary-enumerable-filter (lambda (k v) (even? v)) dict))))
         (assert-equal? res (list (=> 'b  2)(=> 'd  4) ))))

   (test "dictionary-enumerable-filter works on vectors"
      (let* ((dict (vector #\a #\b #\c #\d #\e))
             (res (dictionary-enumerator->list
                     (dictionary-enumerable-filter (lambda (k v) (even? k)) dict))))
         (assert-equal? res (list (=> 0 #\a) (=> 2  #\c) (=> 4  #\e)))))

   
   (test "dictionary-enumerable-filter works on strings"
      (let* ((dict "ab cd")
             (res (dictionary-enumerator->list
                     (dictionary-enumerable-filter (lambda (k v) (not (char-whitespace? v))) dict))))
         (assert-equal? res (list (=> 0  #\a) (=> 1  #\b) (=> 3  #\c) (=> 4 #\d)))))


   (test "dictionary-enumerable-fold works on hashtables"
      (let* ((dict (hashtable (=> 'a 1) (=> 'b 2) (=> 'c 3)))
             (res (dictionary-enumerable-fold (lambda (s k v) (+ s v)) 0
                     dict)))
         (assert-equal? res 6)))

   (test "dictionary-enumerable-fold works on vectors"
      (let* ((dict (vector 1 2 3))
             (res (dictionary-enumerable-fold (lambda (s k v) (+ s v)) 0
                     dict)))
         (assert-equal? res 6)))

    (test "dictionary-enumerable-fold works on strings"
      (let* ((dict "abc")
             (res (dictionary-enumerable-fold (lambda (s k v) (string-append s (string (char-upcase v)))) ""
                     dict)))
         (assert-equal? res "ABC")))

    (test "dictionary-enumerable-any? works with hashtables"
       (let ((dict (hashtable (=> 'a 1) (=> 'b 2) (=> 'c 3))))
          (assert-true (dictionary-enumerable-any? (lambda (k v) (>= v 3)) dict))
          (assert-false (dictionary-enumerable-any? (lambda (k v) (eq? k 'd)) dict))))
   
    (test "dictionary-enumerable-any? works with vectors"
       (let ((dict (vector 'a 'b 'c)))
          (assert-false (dictionary-enumerable-any? (lambda (k v) (>= k 3)) dict))
          (assert-true (dictionary-enumerable-any? (lambda (k v) (eq? v 'b)) dict))))

    (test "dictionary-enumerable-any? works with strings"
       (let ((dict "abc"))
          (assert-true (dictionary-enumerable-any? (lambda (k v) (>= k 2)) dict))
          (assert-false (dictionary-enumerable-any? (lambda (k v) (equal? v #\d)) dict))))

    (test "dictionary-enumerable-every? works with hashtable"
       (let ((dict (hashtable (=> 'a 1) (=> 'b 2) (=> 'c 3))))
          (assert-true (dictionary-enumerable-every? (lambda (k v) (<= v 3)) dict))
          (assert-false (dictionary-enumerable-every? (lambda (k v) (string<? (symbol->string k) "b")) dict))))

    (test "dictionary-enumerable-every? works with vectors"
       (let ((dict (vector 'a 'b 'c)))
          (assert-false (dictionary-enumerable-every? (lambda (k v) (<= k 1)) dict))
          (assert-true (dictionary-enumerable-every? (lambda (k v) (string<? (symbol->string v) "d")) dict))))
    
    (test "dictionary-enumerable-every? works with strings"
       (let ((dict "abc"))
          (assert-false (dictionary-enumerable-every? (lambda (k v) (<= k 1)) dict))
          (assert-true (dictionary-enumerable-every? (lambda (k v) (string<? (string v) "d")) dict))))

    (test "dictionary-enumerable-append works on hashtables"
       (let* ((dict1 (hashtable (=> 'a 1) (=> 'b 2) (=> 'c 3)))
              (dict2 (hashtable (=> 'd 4) (=> 'e 5) (=> 'f 6)))
              (res (dictionary-enumerator->hashtable (dictionary-enumerable-append dict1 dict2))))
          (dictionary-enumerable-for-each (lambda (k v) (assert-equal? (hashtable-get res k) v)) dict1)
          (dictionary-enumerable-for-each (lambda (k v) (assert-equal? (hashtable-get res k) v)) dict2)))

    (test "dictionary-enumerable-append works on vectors"
       (let* ((dict1 (vector 'a 'b 'c))
              (dict2 (vector 'd 'e 'f))
              (res (dictionary-enumerator->vector (dictionary-enumerable-append dict1 dict2))))
          (assert-equal? res (vector (=> 0  'a) (=> 1  'b) (=> 2 'c) (=> 0  'd) (=> 1  'e) (=> 2 'f)))))

    (test "dictionary-enumerable-append works on strings"
       (let* ((dict1 "abc")
              (dict2 "def")
              (res (dictionary-enumerator->vector (dictionary-enumerable-append dict1 dict2))))
          (assert-equal? res (vector (=> 0  #\a) (=> 1 #\b) (=> 2  #\c) (=> 0  #\d) (=> 1 #\e) (=> 2 #\f)))))
   
    
   
   )