(module hoard/indexable
   (export (generic collection-indexable? obj)
           (generic collection-ref obj key)
           (generic collection-set! obj key val)))


;;; indexable collections
(define-generic (collection-indexable? obj)
   (or (list? obj)
       (vector? obj)
       (hashtable? obj)
       (string? obj)))


(define-generic (collection-ref obj key)
   (cond ((list? obj)
          (list-ref obj key))
         ((vector? obj)
          (vector-ref obj key))
         ((hashtable? obj)
          (hashtable-get obj key))
         ((string? obj)
          (string-ref obj key))
         (else
          (error "collection-ref" "unsupported type" obj))))


(define-generic (collection-set! obj key val)
   (cond ((list? obj)
          (list-ref obj key))
         ((vector? obj)
          (vector-ref obj key))
         ((hashtable? obj)
          (hashtable-get obj key))
         ((string? obj)
          (string-ref obj key))
         (else
          (error "collection-set!" "unsupported type" obj))))

