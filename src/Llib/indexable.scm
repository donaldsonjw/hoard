(module hoard/indexable
   (include "enumerable.sch")
   (import hoard/exceptions
           hoard/enumerable
           hoard/enumerator)
   (export (generic collection-indexable? obj)
           (generic collection-ref obj key #!optional (default +collection-unspecified+))
           (generic collection-set! obj key val)
           +collection-unspecified+
           (inline specified? v)
           (collection-slice coll keys)))

(define +collection-unspecified+ (vector #unspecified))

(define-inline (specified? v)
   (not (eq? v +collection-unspecified+)))

;;; indexable collections
(define-generic (collection-indexable? obj)
   (or (list? obj)
       (vector? obj)
       (hashtable? obj)
       (string? obj)))



(define-inline (make-ref-procedure ref len)
   (lambda (obj index #!key default)
      (if (and (integer? index) (<= index (len obj)))
          (ref obj index)
          (if (specified? default)  
              default
              (raise-invalid-index-exception proc: "collection-element"
                 index: index)))))

(define-generic (collection-ref obj index #!optional (default +collection-unspecified+))
   (cond ((list? obj)
          ((make-ref-procedure list-ref length)
           obj index :default default))
         ((vector? obj)
          ((make-ref-procedure vector-ref vector-length)
           obj index :default default))
         ((string? obj)
          ((make-ref-procedure string-ref string-length)
           obj index :default default))
         ((hashtable? obj)
          (if (hashtable-contains? obj index)
              (hashtable-get obj index)
              (if (specified? default)
                  default
                  (raise-invalid-index-exception proc: "collection-ref"
                                                 index: index))))
         (else
          (raise-unsupported-operation-exception proc: "collection-ref"
             obj: obj))))



(define-inline (make-set!-procedure set len)
   (lambda (obj index val)
      (if (and (integer? index) (<= index (len obj)))
          (set obj index val)
          (raise-invalid-index-exception proc: "collection-element-set!"
             index: index))))



(define-generic (collection-set! obj index val)
   (cond ((list? obj)
          ((make-set!-procedure list-set! length)
           obj index val))
         ((vector? obj)
          ((make-set!-procedure vector-set! vector-length)
           obj index val))
         ((string? obj)
          ((make-set!-procedure string-set! string-length)
           obj index val))
         ((hashtable? obj)
          (hashtable-put! obj index val))
         (else
          (raise-unsupported-operation-exception proc: "collection-set!"
             obj: obj))))

(define (collection-slice coll keys)
   (if (and (collection-indexable? coll)
            (enum-or-enumer? keys))
       (enumerable-map (lambda (k) (collection-ref coll k)) keys)
       (raise-invalid-argument-exception proc: "collection-slice" args: (list coll keys))))