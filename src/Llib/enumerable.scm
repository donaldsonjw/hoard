(module hoard/enumerable
   (include "enumerable.sch")
   (import hoard/enumerator
           hoard/dictionary-enumerator)
   (export
      (generic enumerable? obj)
      (generic enumerable-enumerator obj)
      (inline enum-or-enumer? obj)
      (inline get-enumer obj)))



;;;; enumerable protocol
(define-generic (enumerable? obj)
   (or (list? obj)
       (vector? obj)
       (hashtable? obj)
       (string? obj)))


(define-generic (enumerable-enumerator obj)
   (cond ((list? obj)
          (instantiate::list-enumerator (curr obj)))
         ((vector? obj)
          (instantiate::vector-enumerator (vec obj)))
         ((string? obj)
          (instantiate::string-enumerator (str obj)))
         ((hashtable? obj)
          (instantiate::hashtable-dictionary-enumerator (hash obj)))
         (else
          (error "enumerable-enumerator" "invalid argument" obj))))


(define-inline (enum-or-enumer? obj)
   (or (enumerable? obj) (enumerator? obj)))

(define-inline (get-enumer obj)
   (if (enumerable? obj)
       (enumerable-enumerator obj)
       obj))


