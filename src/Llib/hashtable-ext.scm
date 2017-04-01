(module hoard/hashtable-ext
   (import hoard/exceptions
           hoard/association)
   (export (hashtable #!key
                  (size 128)
                  (max-bucket-length 10)
                  (eqtest #f)
                  (hash #f)
                  (weak 'none)
                  (max-length 16384)
                  (bucket-expansion 1.2)
                  #!rest list-of-associations)))



(define-inline (list-of-associations? loa)
   (and (list? loa)
        (every association? loa)))

(define (hashtable #!key
                  (size 128)
                  (max-bucket-length 10)
                  (eqtest #f)
                  (hash #f)
                  (weak 'none)
                  (max-length 16384)
                  (bucket-expansion 1.2)
                  #!rest list-of-associations)
   (if (list-of-associations? list-of-associations)
       (let ((res (create-hashtable :size size
                     :max-bucket-length max-bucket-length
                     :eqtest eqtest
                     :hash hash
                     :weak weak
                     :max-length max-length
                     :bucket-expansion bucket-expansion)))
          (for-each (lambda (kv) (hashtable-put! res (=>key kv) (=>value kv)))
             list-of-associations)
          res)
       (raise-invalid-argument-exception :proc "hashtable"
          :args list-of-associations
          :msg "values to include in hashtable must be lists associations")))