(module hoard/dictionary-enumerable
   (import hoard/exceptions
           hoard/enumerator
           hoard/dictionary-enumerator)
   (export (generic dictionary-enumerable? obj)
           (generic dictionary-enumerable-enumerator obj)
           (inline dictionary-enum-or-enumer? obj)
           (inline get-dictionary-enumer obj)))


;;;; dictionary enumerable protocol
(define-generic (dictionary-enumerable? obj)
   (or (hashtable? obj)
       (vector? obj)
       (string? obj)))

(define-generic (dictionary-enumerable-enumerator obj)
   (cond ((hashtable? obj)
          (instantiate::%hashtable-enumerator (hash obj)))
         ((vector? obj)
          (instantiate::%vector-enumerator (vec obj)))
         ((string? obj)
          (instantiate::%string-enumerator (str obj)))
         (else
          (raise-unsupported-operation-exception :proc "dictionary-enumerable-enumerator"
             :obj obj))))

(define-inline (dictionary-enum-or-enumer? obj)
   (or (dictionary-enumerable? obj)
       (dictionary-enumerator? obj)))

(define-inline (get-dictionary-enumer obj)
   (if (dictionary-enumerable? obj)
       (dictionary-enumerable-enumerator obj)
       obj))