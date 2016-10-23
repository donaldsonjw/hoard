(module hoard/hashtable-ext
   (import hoard/exceptions)
   (export (hashtable #!key
                  (size 128)
                  (max-bucket-length 10)
                  (eqtest #f)
                  (hash #f)
                  (weak 'none)
                  (max-length 16384)
                  (bucket-expansion 1.2)
                  #!rest list-of-key-values)))



(define-inline (list-of-key-values? lop)
   (and (list? lop)
        (every (lambda (x) (and (list? x)
                                (= (length x) 2)))
           lop)))

(define (hashtable #!key
                  (size 128)
                  (max-bucket-length 10)
                  (eqtest #f)
                  (hash #f)
                  (weak 'none)
                  (max-length 16384)
                  (bucket-expansion 1.2)
                  #!rest list-of-key-values)
   (if (list-of-key-values? list-of-key-values)
       (let ((res (create-hashtable :size size
                     :max-bucket-length max-bucket-length
                     :eqtest eqtest
                     :hash hash
                     :weak weak
                     :max-length max-length
                     :bucket-expansion bucket-expansion)))
          (for-each (lambda (kv) (hashtable-put! res (car kv) (cadr kv)))
             list-of-key-values)
          res)
       (raise-invalid-argument-exception :proc "hashtable"
          :args list-of-key-values
          :msg "values to include in hashtable must be lists of the form (key value)")))