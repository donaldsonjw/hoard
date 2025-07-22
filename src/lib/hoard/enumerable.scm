(module hoard/enumerable
   (include "enumerable.sch")
   (import hoard/enumerator
           hoard/exceptions
           hoard/dictionary-enumerator)
   (export
      (generic enumerable? obj)
      (generic enumerable-enumerator obj)
      ( enum-or-enumer? obj)
      ( get-enumer obj)))



;;;; enumerable protocol
(define-generic (enumerable? obj)
   (or (list? obj)
       (vector? obj)
       (hashtable? obj)
       (string? obj)))


(define-generic (enumerable-enumerator obj)
   (cond ((list? obj)
          (instantiate::%list-enumerator (curr obj)))
         ((vector? obj)
          (instantiate::%vector-enumerator (vec obj)))
         ((string? obj)
          (instantiate::%string-enumerator (str obj)))
         ((hashtable? obj)
          (instantiate::%hashtable-enumerator (hash obj)))
         (else
          (raise-invalid-argument-exception proc: "enumerable-enumerator"
             args: obj))))


(define (enum-or-enumer? obj)
   (or (enumerable? obj) (enumerator? obj)))

(define (get-enumer obj)
   (if (enumerable? obj)
       (enumerable-enumerator obj)
       obj))


