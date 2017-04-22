(module hoard/comparable
   (import hoard/comparator
           hoard/exceptions)
   (export (generic comparable? x)
           (generic comparable-comparator obj)))

;;;; comparable protocol
(define-generic (comparable? obj)
   (or (string? obj)
       (keyword? obj)
       (ucs2-string? obj)
       (symbol? obj)
       (number? obj)
       (char? obj)
       (ucs2? obj)))

(define-generic (comparable-comparator obj)
   (cond ((string? obj)
          +string-comparator+)
         ((ucs2-string? obj)
          +ucs2-string-comparator+)
         ((symbol? obj)
          +symbol-comparator+)
         ((number? obj)
          +number-comparator+)
         ((char? obj)
          +char-comparator+)
         ((ucs2? obj)
          +ucs2-comparator+)
         (else
          (raise-unsupported-operation-exception :proc "comparable-comparator"
             :obj obj))))