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
                  #!rest list-of-associations)
           (hashtable-comparator table)
           (hashtable-copy table)
           ( hashtable-buckets hash)
           +hashtable-buckets-index+))



(define (list-of-associations? loa)
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


(define +hashtable-hash-index+ 4)
(define +hashtable-equal-index+ 3)
(define +hashtable-max-bucket-len-index+ 1)
(define +hashtable-buckets-index+ 2)
(define +hashtable-weak-index+ 5)
(define +hashtable-max-length-index+ 6)
(define +hashtable-bucket-expansion-index+ 7)

(define (hashtable-max-bucket-len hash)
   (struct-ref hash +hashtable-max-bucket-len-index+))

(define (hashtable-buckets hash)
   (struct-ref hash +hashtable-buckets-index+))

(define (hashtable-weak hash)
   (struct-ref hash +hashtable-weak-index+))

(define (hashtable-max-length hash)
   (struct-ref hash +hashtable-max-length-index+))

(define (hashtable-bucket-expansion hash)
   (struct-ref hash +hashtable-bucket-expansion-index+))

(define (hashtable-hash-proc hash)
   (let ((res (struct-ref hash +hashtable-hash-index+)))
      (if (procedure? res)
          res
          get-hashnumber)))

(define (buckets-copy buckets)
   (vector-map list-copy buckets))

(define (hashtable-equal-proc hash)
   (let ((res (struct-ref hash +hashtable-equal-index+)))
      (if (procedure? res)
          res
          ;;; the following lambda will need to be updated if hashtable-equal? in runtime/Llib/hash.sch is
          ;;; changed. 
          (lambda (obj1 obj2)
             (cond ((eq? obj1 obj2)
                    #t)
                   ((string? obj1)
                    (if (string? obj2)
                        (string=? obj1 obj2)
                        #f))
                   (else
                    #f))))))


(define (hashtable-comparator table)
   (if (not (hashtable? table))
       (raise-invalid-argument-exception :proc "hashtable-comparator"
          :args table
          :msg "must pass a hashtable")
       (make-comparator :type? (lambda (x) #t)
          :equal? (hashtable-equal-proc table)
          :hash (hashtable-hash-proc table))))


(define (hashtable-copy table)
   (if (not (hashtable? table))
       (raise-invalid-argument-exception :proc "hashtable-copy"
          :args table
          :msg "must pass a hashtable")
       (let ((buckets (hashtable-buckets table)))
          (list->struct (list '%hashtable
                           (hashtable-size table)
                           (hashtable-max-bucket-len table)
                           (buckets-copy buckets)
                           (hashtable-equal-proc table)
                           (hashtable-hash-proc table)
                           (hashtable-weak table)
                           (hashtable-max-length table)
                           (hashtable-bucket-expansion table))))))