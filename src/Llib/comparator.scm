;;;; a srfi-128 inspired comparator implementation
(module hoard/comparator
   (import hoard/exceptions)
   (export
      (class %parameterized-comparator
         type?
         lessthan?
         equal?
         hash)
      (generic comparator? obj)
      (generic comparator-ordered? comp)
      (generic comparator-hashable? comp)
      (generic comparator-type? comp x1)
      (generic comparator=? comp x1 x2)
      (generic comparator<? comp x1 x2)
      (generic comparator>? comp x1 x2)
      (generic comparator<=? comp x1 x2)
      (generic comparator>=? comp x1 x2)
      (generic comparator-hash comp x)      
      +number-comparator+
      +char-comparator+
      +ucs2-comparator+
      +string-comparator+
      +string-ci-comparator+
      +symbol-comparator+
      +char-ci-comparator+
      +ucs2-ci-comparator+
      +symbol-ci-comparator+
      +ucs2-string-comparator+
      +uscs2-string-ci-comparator+
      +keyword-comparator+
      +keyword-ci-comparator+))

;;;; utility functions
(define-inline (compose f g)
   (lambda (x) (f (g x))))

;;;; comparator protocol
(define-generic (comparator? obj)
   #f)

(define-generic (comparator-ordered? comp))

(define-generic (comparator-hashable? comp))

(define-generic (comparator-type? comp x1))

(define-generic (comparator=? comp x1 x2))

(define-generic (comparator<? comp x1 x2))

(define-generic (comparator>? comp x1 x2))

(define-generic (comparator<=? comp x1 x2))

(define-generic (comparator>=? comp x1 x2))

(define-generic (comparator-hash comp x))


;;;; utility function for creating a comparator
(define (make-comparator #!key type? equal? lessthan? hash)
   (if (or (and type?  equal? hash)
           (and type? equal? lessthan?))
       (instantiate::%parameterized-comparator
          (type? type?)
          (equal? equal?)
          (lessthan? lessthan?)
          (hash hash))
       (raise-invalid-argument-exception :proc "make-comparator"
          :msg "you must pass valid procedures for type? equal? lessthan? and hash"
          :args (list type? equal? lessthan? hash))))

(define-method (comparator? comp::%parameterized-comparator)
   #t)

(define-method (comparator-ordered? comp::%parameterized-comparator)
   (procedure? (-> comp lessthan?)))

(define-method (comparator-hashable? comp::%parameterized-comparator)
   (procedure? (-> comp hash)))

(define-method (comparator-type? comp::%parameterized-comparator x)
   ((-> comp type?) x))

(define-method (comparator=? comp::%parameterized-comparator x1 x2)
   (if (not (and ((-> comp type?) x1)
                 ((-> comp type?) x2)))
       (raise-invalid-argument-exception :proc "comparator=?"
          :msg "invalid argument types"
          :args (list x1 x2))
       ((-> comp equal?) x1 x2)))

(define-method (comparator<? comp::%parameterized-comparator x1 x2)
   (if (not (and ((-> comp type?) x1)
                 ((-> comp type?) x2)))
       (raise-invalid-argument-exception :proc "comparator=<"
          :msg "invalid argument types"
          :args (list x1 x2))
       ((-> comp lessthan?) x1 x2)))

(define-method (comparator>? comp::%parameterized-comparator x1 x2)
   (if (not (and ((-> comp type?) x1)
                 ((-> comp type?) x2)))
       (raise-invalid-argument-exception :proc "comparator>?"
          :msg "invalid argument types"
          :args (list x1 x2))
       ((-> comp lessthan?) x2 x1)))

(define-method (comparator<=? comp::%parameterized-comparator x1 x2)
   (if (not (and ((-> comp type?) x1)
                 ((-> comp type?) x2)))
       (raise-invalid-argument-exception :proc "comparator<=?"
          :msg "invalid argument types"
          :args (list x1 x2))
       (not ((-> comp lessthan?) x2 x1))))

(define-method (comparator>=? comp::%parameterized-comparator x1 x2)
   (if (not (and ((-> comp type?) x1)
                 ((-> comp type?) x2)))
       (raise-invalid-argument-exception :proc "comparator>=?"
          :msg "invalid argument types"
          :args (list x1 x2))
       (not ((-> comp lessthan?) x1 x2)))) 
             
(define-method (comparator-hash comp::%parameterized-comparator x)
   (if (not ((-> comp type?) x))
       (raise-invalid-argument-exception :proc "comparator-hash"
          :msg "invalid argument types"
          :args (list x))
       ((-> comp hash) x)))



;;;; string comparator implementation
(define +string-comparator+
   (make-comparator  :type? string? :equal? string=? :lessthan? string<?
      :hash get-hashnumber))

;;;; case-insensitive string comparator implementation
(define +string-ci-comparator+
   (make-comparator  :type? string? :equal? string-ci=? :lessthan? string-ci<?
      :hash (compose get-hashnumber string-upcase)))


;;;; string comparator implementation
(define +ucs2-string-comparator+
   (make-comparator  :type? ucs2-string? :equal? ucs2-string=? :lessthan? ucs2-string<?
      :hash get-hashnumber))

;;;; case-insensitive string comparator implementation
(define +uscs2-string-ci-comparator+
   (make-comparator  :type? ucs2-string? :equal? ucs2-string-ci=? :lessthan? ucs2-string-ci<?
      :hash (compose get-hashnumber ucs2-string-upcase)))


;;;; symbol comparator implementation
(define +symbol-comparator+
   (make-comparator  :type? symbol? :equal? eq?
      :lessthan? (lambda (x y) (string<? (symbol->string x)
                                  (symbol->string y)))
      :hash (compose get-hashnumber string->symbol)))


;;;; symbol case-insensitive comparator implementation
(define +symbol-ci-comparator+
   (make-comparator  :type? symbol?
      :equal? (lambda (x y)
                 (string-ci=? (symbol->string x)
                    (symbol->string y)))
      :lessthan? (lambda (x y) (string<? (symbol->string x)
                                  (symbol->string y)))
      :hash (compose get-hashnumber (compose string-upcase symbol->string))))

;;;; keyword comparator implementation
(define +keyword-comparator+
   (make-comparator  :type? keyword? :equal? eq?
      :lessthan? (lambda (x y) (string<? (keyword->string x)
                                  (keyword->string y)))
      :hash get-hashnumber))


;;;; keyword case-insensitive comparator implementation
(define +keyword-ci-comparator+
   (make-comparator  :type? keyword?
      :equal? (lambda (x y)
                 (string-ci=? (keyword->string x)
                    (keyword->string y)))
      :lessthan? (lambda (x y) (string<? (keyword->string x)
                                  (keyword->string y)))
      :hash (compose get-hashnumber (compose string-upcase keyword->string))))


(define (number-hash::long x)
   (cond ((and (bignum? x)
               (< x (maxvalelong)))
          (get-hashnumber (bignum->elong x)))
         ((and (flonum? x)
               (integer? x))
          (get-hashnumber (flonum->elong x)))
         (else
          (get-hashnumber x))))

          

;;; number comparator implementation
(define +number-comparator+
   (make-comparator  :type? number?
      :equal? =
      :lessthan? <
      :hash number-hash))

;;; char comparator implementation
(define +char-comparator+
   (make-comparator  :type? char?
      :equal? char=?
      :lessthan? char<?
      :hash get-hashnumber))

;;; case insensitive char comparator
(define +char-ci-comparator+
   (make-comparator  :type? char?
      :equal? char-ci=?
      :lessthan? char-ci<?
      :hash (compose get-hashnumber char-upcase)))

;;; ucs2 comparator implementation
(define +ucs2-comparator+
   (make-comparator  :type? ucs2?
      :equal? ucs2=?
      :lessthan? ucs2<?
      :hash get-hashnumber))


;;; ucs2 comparator implementation
(define +ucs2-ci-comparator+
   (make-comparator  :type? ucs2?
      :equal? ucs2-ci=?
      :lessthan? ucs2-ci<?
      :hash (compose get-hashnumber ucs2-upcase)))



;;; default comparator










