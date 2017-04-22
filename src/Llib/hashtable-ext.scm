(module hoard/hashtable-ext
   (import hoard/exceptions
           hoard/comparator
           hoard/association)
   (export (hashtable #!key
                  (size 128)
                  (max-bucket-length 10)
                  (eqtest #f)
                  (hash #f)
                  (comparator #f)
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
                  (comparator #f)
                  (weak 'none)
                  (max-length 16384)
                  (bucket-expansion 1.2)
                  #!rest list-of-associations)
   (when (not (list-of-associations? list-of-associations))
      (raise-invalid-argument-exception :proc "hashtable"
          :args list-of-associations
          :msg "values to include in hashtable must be lists associations"))
   (when (and comparator (not (comparator-hashable? comparator)))
      (raise-invalid-argument-exception :proc "hashtable"
         :args comparator
         :msg "comparator must be hashable"))
   (let ((res (create-hashtable :size size
                 :max-bucket-length max-bucket-length
                 :eqtest (if comparator (lambda (x y) (comparator=? comparator x y)) eqtest)
                 :hash   (if comparator (lambda (x) (comparator-hash comparator x)) hash)
                 :weak weak
                 :max-length max-length
                 :bucket-expansion bucket-expansion)))
      (for-each (lambda (kv) (hashtable-put! res (=>key kv) (=>value kv)))
         list-of-associations)
      res)
   )