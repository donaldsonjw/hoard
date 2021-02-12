(module hoard/dictionary-enumerable
   (import hoard/exceptions
           hoard/enumerator
           hoard/dictionary-enumerator)
   (export (generic dictionary-enumerable? obj)
           (generic dictionary-enumerable-enumerator obj)
           ( dictionary-enum-or-enumer? obj)
           ( get-dictionary-enumer obj)))


;;;; dictionary enumerable protocol
(define-generic (dictionary-enumerable? obj)
   (or (hashtable? obj)
       (vector? obj)
       (string? obj)
       (alist? obj)))

(define-generic (dictionary-enumerable-enumerator obj)
   (cond ((hashtable? obj)
          (instantiate::%hashtable-enumerator (hash obj)))
         ((vector? obj)
          (instantiate::%vector-enumerator (vec obj)))
         ((string? obj)
          (instantiate::%string-enumerator (str obj)))
         ((alist? obj)
          (instantiate::%alist-enumerator (curr obj)))
         (else
          (raise-unsupported-operation-exception :proc "dictionary-enumerable-enumerator"
             :obj obj))))

(define (dictionary-enum-or-enumer? obj)
   (or (dictionary-enumerable? obj)
       (dictionary-enumerator? obj)))

(define (get-dictionary-enumer obj)
   (if (dictionary-enumerable? obj)
       (dictionary-enumerable-enumerator obj)
       obj))