(module dictionary-enumerator-test
   (library btest hoard)
   (export dictionary-enumerator-tests))


(define-test-suite dictionary-enumerator-tests

   (test "dictionary-enumerator for hashtable throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((hash-enum (collection-enumerator (create-hashtable))))
         (assert-exception-thrown (dictionary-enumerator-current hash-enum)
            &error)))
   
   (test "dictionary-enumerator for hashtable immediately returns false on empty hashtable"
      (let ((hash-enum (collection-enumerator (create-hashtable))))
         (assert-false (dictionary-enumerator-move-next! hash-enum))))

   (test "dictionary-enumerator for (hashtable '(a 1) '(b 2) '(c 3))  return 3 items"
       (let ((hash (hashtable '(a 1) '(b 2) '(c 3))))
         (let ((hash-enum (collection-enumerator hash)))
            (assert-equal? (let loop ((cont (dictionary-enumerator-move-next! hash-enum))
                                      (res '()))
                              (if cont
                                  (let ((t (dictionary-enumerator-current hash-enum))) 
                                     (loop (dictionary-enumerator-move-next! hash-enum)
                                        (cons t res)))
                                  res)) (list 1 2 3)))))

   (test "dictionary-enumerator enumerates over keys correctly"
      (let ((hash (create-hashtable)))
         (hashtable-put! hash 'a 1)
         (hashtable-put! hash 'b 2)
         (hashtable-put! hash 'c 3)
         (let ((hash-enum (collection-enumerator hash)))
            (assert-equal? (let loop ((cont (dictionary-enumerator-move-next! hash-enum))
                                      (res '()))
                              (if cont
                                  (let ((t (dictionary-enumerator-key hash-enum))) 
                                     (loop (dictionary-enumerator-move-next! hash-enum)
                                        (cons t res)))
                                  res)) (list 'a 'b 'c)))))

   (test "dictionary-enumerator-clone works for hashtable-enumerators"
      (let* ((hash (hashtable '(a 1) '(b 2) '(c 3)))
             (enumer1 (dictionary-enumerator hash))
             (enumer2 (dictionary-enumerator-clone enumer1)))
         (dictionary-enumerator-move-next! enumer1)
         (dictionary-enumerator-move-next! enumer1)
         (assert-equal? (dictionary-enumerator-key enumer1) 'b)
         (assert-equal? (dictionary-enumerator-value enumer1) 2)
         (dictionary-enumerator-move-next! enumer2)
         (assert-equal? (dictionary-enumerator-key enumer2) 'c)
         (assert-equal? (dictionary-enumerator-value enumer2) 3)))

   (test "dictionary-enumerator for vector throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((enum (collection-enumerator (vector))))
         (assert-exception-thrown (dictionary-enumerator-current enum)
            &error)))
   
   (test "dictionary-enumerator for vector immediately returns false on empty hashtable"
      (let ((enum (collection-enumerator (vector))))
         (assert-false (dictionary-enumerator-move-next! enum))))

   (test "dictionary-enumerator for (vector 'a 'b 'c)  returns 3 items"
      (let ((enum (collection-enumerator (vector 'a 'b 'c))))
         (assert-equal? (let loop ((cont (dictionary-enumerator-move-next! enum))
                                   (res '()))
                           (if cont
                               (let ((t (dictionary-enumerator-current enum))) 
                                  (loop (dictionary-enumerator-move-next! enum)
                                     (cons t res)))
                               (reverse! res))) (list 'a 'b 'c))))

   (test "dictionary-enumerator enumerates over keys correctly (vector 'a 'b 'c)"
      (let ((enum (collection-enumerator (vector 'a 'b 'c))))
         (assert-equal? (let loop ((cont (dictionary-enumerator-move-next! enum))
                                   (res '()))
                           (if cont
                               (let ((t (dictionary-enumerator-key enum))) 
                                  (loop (dictionary-enumerator-move-next! enum)
                                     (cons t res)))
                               (reverse! res))) (list 0 1 2))))

   (test "dictionary-enumerator-clone works for vector-enumerators"
      (let* ((vec (vector 'a 'b 'c))
             (enumer1 (dictionary-enumerator vec))
             (enumer2 (dictionary-enumerator-clone enumer1)))
         (dictionary-enumerator-move-next! enumer1)
         (dictionary-enumerator-move-next! enumer1)
         (assert-equal? (dictionary-enumerator-key enumer1) 1)
         (assert-equal? (dictionary-enumerator-value enumer1) 'b)
         (dictionary-enumerator-move-next! enumer2)
         (assert-equal? (dictionary-enumerator-key enumer2) 0)
         (assert-equal? (dictionary-enumerator-value enumer2) 'a)))


   (test "dictionary-enumerator for \"abc\"  returns 3 items"
      (let ((enum (collection-enumerator "abc")))
         (assert-equal? (let loop ((cont (dictionary-enumerator-move-next! enum))
                                   (res '()))
                           (if cont
                               (let ((t (dictionary-enumerator-current enum))) 
                                  (loop (dictionary-enumerator-move-next! enum)
                                     (cons t res)))
                               (reverse! res))) (list #\a #\b #\c))))

   (test "dictionary-enumerator enumerates over keys correctly  \"abc\""
      (let ((enum (collection-enumerator "abc")))
         (assert-equal? (let loop ((cont (dictionary-enumerator-move-next! enum))
                                   (res '()))
                           (if cont
                               (let ((t (dictionary-enumerator-key enum))) 
                                  (loop (dictionary-enumerator-move-next! enum)
                                     (cons t res)))
                               (reverse! res))) (list 0 1 2))))

   (test "dictionary-enumerator-clone works for string dictionary enumerators"
      (let* ((vec "abc")
             (enumer1 (dictionary-enumerator vec))
             (enumer2 (dictionary-enumerator-clone enumer1)))
         (dictionary-enumerator-move-next! enumer1)
         (dictionary-enumerator-move-next! enumer1)
         (assert-equal? (dictionary-enumerator-key enumer1) 1)
         (assert-equal? (dictionary-enumerator-value enumer1) #\b)
         (dictionary-enumerator-move-next! enumer2)
         (assert-equal? (dictionary-enumerator-key enumer2) 0)
         (assert-equal? (dictionary-enumerator-value enumer2) #\a)))


   (test "dictionary-enumerator->hashtable works"
      (let* ((dict (hashtable '(a 1) '(b 2) '(c 3)))
             (res (dictionary-enumerator->hashtable
                     (dictionary-enumerable-map (lambda (k v)
                                                   (cons k (+ v 1)))
                        dict))))
         (assert-equal? (hashtable-get res 'a) 2)
         (assert-equal? (hashtable-get res 'b) 3)
         (assert-equal? (hashtable-get res 'c) 4)))

   (test "dictionary-enumerator->list works"
      (let* ((dict (hashtable '(a 1) '(b 2) '(c 3)))
             (res (dictionary-enumerator->list
                     (dictionary-enumerable-map (lambda (k v)
                                                   (cons k (+ v 1)))
                        dict))))
         (assert-equal? res '((c . 4) (b . 3) (a . 2)))))


   (test "dictionary-enumerator->vector works"
      (let* ((dict (hashtable '(a 1) '(b 2) '(c 3)))
             (res (dictionary-enumerator->vector
                     (dictionary-enumerable-map (lambda (k v)
                                                   (cons k (+ v 1)))
                        dict))))
         (assert-equal? res '#((c . 4) (b . 3) (a . 2)))))

         

   
   )
