(module hoard/association
   (import hoard/comparator)
   (export
      (class %association
         key
         value)
      (class %association-comparator
         key-comparator)
      (=> key value)
      (=>key assoc::%association)
      (=>value assoc::%association)
      (pair->association p::pair)
      (association? obj)
      (make-association-comparator comp)
      (=>key! assoc::%association key)
      (=>value! assoc::%association val)))

(define (association? obj)
   (isa? obj %association))

(define (pair->association p::pair)
   (=> (car p) (cdr p)))

(define (=> key value)
   (instantiate::%association (key key)
                              (value value)))

(define (=>key assoc::%association)
   (-> assoc key))

(define (=>value assoc::%association)
   (-> assoc value))

(define (=>key! assoc::%association key)
   (set! (-> assoc key) key))

(define (=>value! assoc::%association val)
   (set! (-> assoc value) val))

;;; association comparator implementation
(define (make-association-comparator comp)
   (instantiate::%association-comparator (key-comparator comp)))

(define-method (comparator? obj::%association-comparator)
   #t)

(define-method (comparator-ordered? comp::%association-comparator)
   (comparator-ordered? (-> comp key-comparator))) 

(define-method (comparator-hashable? comp::%association-comparator)
   (comparator-hashable? (-> comp key-comparator)))

(define-method (comparator-type? comp::%association-comparator x)
   (and (isa? x %association)
        (let ((t::%association x))
           (comparator-type? (-> comp key-comparator) (-> t key)))))

(define-method (comparator=? comp::%association-comparator x1 x2)
   (and (association? x1)
        (association? x2)
        (let ((t1::%association x1)
              (t2::%association x2))
           (comparator=? (-> comp key-comparator)
              (-> t1 key) (-> t2 key)))))

(define-method (comparator<? comp::%association-comparator x1 x2)
   (and (association? x1)
        (association? x2)
        (let ((t1::%association x1)
              (t2::%association x2))
           (comparator<? (-> comp key-comparator)
                         (-> t1 key) (-> t2 key)))))

(define-method (comparator>? comp::%association-comparator x1 x2)
   (and (association? x1)
        (association? x2)
        (let ((t1::%association x1)
              (t2::%association x2))
           (comparator>? (-> comp key-comparator)
              (-> t1 key) (-> t2 key))))) 
       
(define-method (comparator<=? comp::%association-comparator x1 x2)
   (and (association? x1)
        (association? x2)
        (let ((t1::%association x1)
              (t2::%association x2))
           (comparator<=? (-> comp key-comparator)
              (-> t1 key) (-> t2 key))))) 
  
(define-method (comparator>=? comp::%association-comparator x1 x2)
   (and (association? x1)
        (association? x2)
        (let ((t1::%association x1)
              (t2::%association x2))
           (comparator>=? (-> comp key-comparator)
              (-> t1 key) (-> t2 key)))))

(define-method (comparator-hash comp::%association-comparator x)
   (and (association? x)
        (let ((t::%association x))
           (comparator-hash (-> comp key-comparator) t))))


