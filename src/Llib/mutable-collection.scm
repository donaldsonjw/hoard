(module hoard/mutable-collection
   (import hoard/exceptions)
   (export (generic mutable-collection? obj)
           (generic collection-element-set! obj index val)))





;;;; mutable-collection protocol

(define-generic (mutable-collection? obj)
   (if (or (list? obj)
           (vector? obj)
           (hashtable? obj)
           (string? obj))
       #t
       (raise-unsupported-operation-exception proc: mutable-collection?
          obj: obj)))



(define-inline (make-element-set!-procedure set len)
   (lambda (obj index val)
      (if (and (integer? index) (<= index (len obj)))
          (set obj index val)
          (raise-invalid-index-exception proc: "collection-element-set!"
             index: index))))



(define-generic (collection-element-set! obj index val)
   (cond ((list? obj)
          ((make-element-set!-procedure list-set! length)
           obj index val))
         ((vector? obj)
          ((make-element-set!-procedure vector-set! vector-length)
           obj index val))
         ((string? obj)
          ((make-element-set!-procedure string-set! string-length)
           obj index val))
         ((hashtable? obj)
          (hashtable-put! obj index val))
         (else
          (raise-unsupported-operation-exception proc: "collection-element-set!"
             obj: obj))))

