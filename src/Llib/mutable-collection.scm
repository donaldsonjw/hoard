(module hoard/mutable-collection
   (import hoard/exceptions)
   (export (generic collection-mutable? obj)))





;;;; mutable-collection protocol

(define-generic (collection-mutable? obj)
   (if (or (list? obj)
           (vector? obj)
           (hashtable? obj)
           (string? obj))
       #t
       (raise-unsupported-operation-exception proc: "collection-mutable?"
          obj: obj)))




