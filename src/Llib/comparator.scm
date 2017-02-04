;;;; a srfi-128 inspired comparator implementation
(module hoard/comparator
   (static
      (class %number-comparator)
      (class %char-comparator)
      (class %ucs2-comparator)
      (class %string-comparator)
      (class %string-ci-comparator))
   (export
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
      +string-ci-comparator+))

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


;;;; string comparator implementation
(define +string-comparator+ (instantiate::%string-comparator))

(define-method (comparator? obj::%string-comparator)
   #t)

(define-method (comparator-ordered? comp::%string-comparator)
   #t)

(define-method (comparator-hashable? comp::%string-comparator)
   #t)

(define-method (comparator-type? comp::%string-comparator x)
   (string? x))

(define-method (comparator=? comp::%string-comparator x1 x2)
   (and (string? x1)
        (string? x2)
        (string=? x1 x2)))

(define-method (comparator<? comp::%string-comparator x1 x2)
   (and (string? x1) 
        (string? x2)
        (string<? x1 x2)))

(define-method (comparator>? comp::%string-comparator x1 x2)
   (and (string? x1) 
        (string? x2)
        (string>? x1 x2)))

(define-method (comparator<=? comp::%string-comparator x1 x2)
   (and (string? x1) 
        (string? x2)
        (string<=? x1 x2)))

(define-method (comparator>=? comp::%string-comparator x1 x2)
   (and (string? x1) 
        (string? x2)
        (string>=? x1 x2)))
             
(define-method (comparator-hash comp::%string-comparator x)
   (and (string? x)
        (get-hashnumber x)))

;;;; case-insensitive string comparator implementation
(define +string-ci-comparator+ (instantiate::%string-ci-comparator))

(define-method (comparator? obj::%string-ci-comparator)
   #t)

(define-method (comparator-ordered? comp::%string-ci-comparator)
   #t)

(define-method (comparator-hashable? comp::%string-ci-comparator)
   #t)

(define-method (comparator-type? comp::%string-ci-comparator x)
   (string? x))

(define-method (comparator=? comp::%string-ci-comparator x1 x2)
   (and (string? x1)
        (string? x2)
        (string-ci=? x1 x2)))

(define-method (comparator<? comp::%string-ci-comparator x1 x2)
   (and (string? x1) 
        (string? x2)
        (string-ci<? x1 x2)))

(define-method (comparator>? comp::%string-ci-comparator x1 x2)
   (and (string? x1) 
        (string? x2)
        (string-ci>? x1 x2)))

(define-method (comparator<=? comp::%string-ci-comparator x1 x2)
   (and (string? x1) 
        (string? x2)
        (string-ci<=? x1 x2)))

(define-method (comparator>=? comp::%string-ci-comparator x1 x2)
   (and (string? x1) 
        (string? x2)
        (string-ci>=? x1 x2)))
             
(define-method (comparator-hash comp::%string-ci-comparator x)
   (and (string? x)
        (get-hashnumber (string-downcase x))))

;;; number comparator implementation
(define +number-comparator+ (instantiate::%number-comparator))

(define-method (comparator? obj::%number-comparator)
   #t)

(define-method (comparator-ordered? comp::%number-comparator)
   #t)

(define-method (comparator-hashable? comp::%number-comparator)
   #t)

(define-method (comparator-type? comp::%number-comparator x)
   (number? x))

(define-method (comparator=? comp::%number-comparator x1 x2)
   (and (number? x1)
        (number? x2)
        (= x1 x2)))

(define-method (comparator<? comp::%number-comparator x1 x2)
   (and (number? x1) 
        (number? x2)
        (< x1 x2)))

(define-method (comparator>? comp::%number-comparator x1 x2)
   (and (number? x1) 
        (number? x2)
        (> x1 x2)))

(define-method (comparator<=? comp::%number-comparator x1 x2)
   (and (number? x1) 
        (number? x2)
        (<= x1 x2)))

(define-method (comparator>=? comp::%number-comparator x1 x2)
   (and (number? x1) 
        (number? x2)
        (>= x1 x2)))
             
(define-method (comparator-hash comp::%number-comparator x)
   (and (number? x)
        (cond ((and (bignum? x)
                    (< x (maxvalelong)))
               (get-hashnumber (bignum->elong x)))
              ((and (flonum? x)
                    (integer? x))
               (get-hashnumber (flonum->elong x)))
              (else
               (get-hashnumber x)))))


;;; char comparator implementation
(define +char-comparator+ (instantiate::%char-comparator))

(define-method (comparator? obj::%char-comparator)
   #t)

(define-method (comparator-ordered? comp::%char-comparator)
   #t)

(define-method (comparator-hashable? comp::%char-comparator)
   #t)

(define-method (comparator-type? comp::%char-comparator x)
   (char? x))

(define-method (comparator=? comp::%char-comparator x1 x2)
   (and (char? x1)
        (char? x2)
        (char=? x1 x2)))

(define-method (comparator<? comp::%char-comparator x1 x2)
   (and (char? x1) 
        (char? x2)
        (char<? x1 x2)))

(define-method (comparator>? comp::%char-comparator x1 x2)
   (and (char? x1) 
        (char? x2)
        (char>? x1 x2)))

(define-method (comparator<=? comp::%char-comparator x1 x2)
   (and (char? x1) 
        (char? x2)
        (char<=? x1 x2)))

(define-method (comparator>=? comp::%char-comparator x1 x2)
   (and (char? x1) 
        (char? x2)
        (char>=? x1 x2)))
             
(define-method (comparator-hash comp::%char-comparator x)
   (and (char? x)
        (get-hashnumber x)))

;;; ucs2 comparator implementation
(define +ucs2-comparator+ (instantiate::%ucs2-comparator))

(define-method (comparator? obj::%ucs2-comparator)
   #t)

(define-method (comparator-ordered? comp::%ucs2-comparator)
   #t)

(define-method (comparator-hashable? comp::%ucs2-comparator)
   #t)

(define-method (comparator-type? comp::%ucs2-comparator x)
   (ucs2? x))

(define-method (comparator=? comp::%ucs2-comparator x1 x2)
   (and (ucs2? x1)
        (ucs2? x2)
        (ucs2=? x1 x2)))

(define-method (comparator<? comp::%ucs2-comparator x1 x2)
   (and (ucs2? x1) 
        (ucs2? x2)
        (ucs2<? x1 x2)))

(define-method (comparator>? comp::%ucs2-comparator x1 x2)
   (and (ucs2? x1) 
        (ucs2? x2)
        (ucs2>? x1 x2)))

(define-method (comparator<=? comp::%ucs2-comparator x1 x2)
   (and (ucs2? x1) 
        (ucs2? x2)
        (ucs2<=? x1 x2)))

(define-method (comparator>=? comp::%ucs2-comparator x1 x2)
   (and (ucs2? x1) 
        (ucs2? x2)
        (ucs2>=? x1 x2)))
             
(define-method (comparator-hash comp::%ucs2-comparator x)
   (and (ucs2? x)
        (get-hashnumber x)))











