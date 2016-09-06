(module dictionary-enumerator-test
   (library btest hoard)
   (export dictionary-enumerator-tests))


(define-test-suite dictionary-enumerator-tests

   (test "hashtable-dictionary-enumerator throws an exception if enumerator-move-next is not called before enumerator-current"
      (let ((hash-enum (collection-enumerator (create-hashtable))))
         (assert-exception-thrown (dictionary-enumerator-current hash-enum)
            &error)))
   
  
   (test "hashtable-dictionary-enumerator for hashtable immediately returns false on empty hashtable"
      (let ((hash-enum (collection-enumerator (create-hashtable))))
         (assert-false (dictionary-enumerator-move-next! hash-enum))))

   (test "hashtable-dictionary-enumerator for (hash (a . 1) (b . 2) (c . 3))  return 3 items"
       (let ((hash (create-hashtable)))
         (hashtable-put! hash 'a 1)
         (hashtable-put! hash 'b 2)
         (hashtable-put! hash 'c 3)
         (let ((hash-enum (collection-enumerator hash)))
            (assert-equal? (let loop ((cont (dictionary-enumerator-move-next! hash-enum))
                                      (res '()))
                              (if cont
                                  (let ((t (dictionary-enumerator-current hash-enum))) 
                                     (loop (dictionary-enumerator-move-next! hash-enum)
                                        (cons t res)))
                                  res)) (list 1 2 3)))))

   (test "hashtable-dictionary-enumerator enumerates over keys correctly"
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
   
   )
