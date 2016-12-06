(module hoard/contiguous-stack
   (import hoard/stack
           hoard/enumerator
           hoard/enumerable
           hoard/collection
           hoard/mutable-collection
           hoard/exceptions)
   (export
      (class %contiguous-stack
         (top (default 0))
         store)
      (class %contiguous-stack-enumerator
         stk::%contiguous-stack
         (curr-idx (default #unspecified)))
      
      (inline make-contiguous-stack #!key capacity)
      (inline contiguous-stack? obj)
      (inline contiguous-stack-capacity stk::%contiguous-stack)
      (inline contiguous-stack-push! stk::%contiguous-stack item)
      (inline contiguous-stack #!key capacity #!rest rest)
      (inline contiguous-stack-pop! stk::%contiguous-stack)
      (inline contiguous-stack-top stk::%contiguous-stack)
      (inline contiguous-stack-empty? stk::%contiguous-stack)
      (inline contiguous-stack-length stk::%contiguous-stack)))


(define-inline (contiguous-stack? obj)
   (isa? obj %contiguous-stack))

(define-inline (make-contiguous-stack #!key capacity)
   (instantiate::%contiguous-stack (store (make-vector capacity))))

(define-inline (contiguous-stack-empty? stk::%contiguous-stack)
   (= (-> stk top) 0))

(define-inline (contiguous-stack-length stk::%contiguous-stack)
   (-> stk top))

(define-inline (contiguous-stack #!key capacity #!rest rest)
   (if (> (length rest) capacity)
       (raise-invalid-argument-exception :proc "contiguous-stack"
          :msg "capacity must be equal to or larger than the number of objects used to create the stack"
          :args rest)
       (let ((stk (instantiate::%contiguous-stack (store (make-vector capacity)))))
          (for-each (lambda (itm) (contiguous-stack-push! stk itm))
             (reverse! rest))
          stk)))

(define-inline (contiguous-stack-capacity stk::%contiguous-stack)
   (vector-length (-> stk store)))

(define-inline (contiguous-stack-push! stk::%contiguous-stack item)
   (if (>= (-> stk top) (contiguous-stack-capacity stk))
       (raise-invalid-state-exception :proc "contiguous-stack-push!"
          :msg "cannot push an item on a full stack"
          :obj stk)
       (begin
          (vector-set! (-> stk store) (-> stk top) item)
          (set! (-> stk top) (+ (-> stk top) 1)))))


(define-inline (contiguous-stack-pop! stk::%contiguous-stack)
   (if (contiguous-stack-empty? stk)
       (raise-invalid-state-exception :proc "contiguous-stack-pop!"
          :msg "cannot pop an item off of a empty stack"
          :obj stk)
       (let ((res (vector-ref (-> stk store) (- (-> stk top) 1))))
          (set! (-> stk top) (- (-> stk top) 1))
          res)))

(define-inline (contiguous-stack-top stk::%contiguous-stack)
   (if (contiguous-stack-empty? stk)
       (raise-invalid-state-exception :proc "contiguous-stack-top"
          :msg "cannot retrieve the top item of an empty stack"
          :obj stk)
       (vector-ref (-> stk store) (- (-> stk top) 1))))
       

;;;; implementation of generic stack protocol
(define-method (stack? obj::%contiguous-stack)
   #t)

(define-method (stack-push! obj::%contiguous-stack val)
   (contiguous-stack-push! obj val))

(define-method (stack-pop! obj::%contiguous-stack)
   (contiguous-stack-pop! obj))

(define-method (stack-top obj::%contiguous-stack)
   (contiguous-stack-top obj))

(define-method (stack-length obj::%contiguous-stack)
   (contiguous-stack-length obj))

(define-method (stack-fixed-capacity? obj::%contiguous-stack)
   #t)

(define-method (stack-capacity obj::%contiguous-stack)
   (contiguous-stack-capacity obj))

(define-method (stack-empty? obj::%contiguous-stack)
   (contiguous-stack-empty? obj))

;;;; contiguous stack enumerator
(define-method (enumerator? enumerator::%contiguous-stack-enumerator)
   #t)

(define-method (enumerator-move-next! enumerator::%contiguous-stack-enumerator)
   (cond ((eq? (-> enumerator curr-idx) #unspecified)
          (set! (-> enumerator curr-idx) (- (-> enumerator stk top) 1))
          (not (contiguous-stack-empty? (-> enumerator stk))))
         ((>= (- (-> enumerator curr-idx) 1) 0)
          (set! (-> enumerator curr-idx) (- (-> enumerator curr-idx) 1))
          #t)
         (else 
          #f)))

(define-method (enumerator-clone enumerator::%contiguous-stack-enumerator)
   (duplicate::%contiguous-stack-enumerator enumerator))

(define-method (enumerator-current enumerator::%contiguous-stack-enumerator)
   ;;; not started
   (if (eq? (-> enumerator curr-idx) #unspecified)
       (raise-invalid-state-exception :proc "enumerator-current"
          :msg "invalid state; enumerator-move-next must be called before enumerator-current"
          :obj enumerator)
       (vector-ref (-> enumerator stk store) (-> enumerator curr-idx))))

;;;; collection protocol
(define-method (collection? obj::%contiguous-stack)
   #t)

(define-method (collection-length obj::%contiguous-stack)
   (contiguous-stack-length obj))

(define-method (collection-enumerator obj::%contiguous-stack)
   (instantiate::%contiguous-stack-enumerator (stk obj)))

(define-method (collection-contains? obj::%contiguous-stack itm)
   (let ((enumer (collection-enumerator obj)))
      (bind-exit (return)
         (let loop ((cont (enumerator-move-next! enumer)))
            (if cont
                (if (equal? (enumerator-current enumer) itm)
                    (return #t)
                    (loop (enumerator-move-next! enumer)))
                (return #f))))))
         
(define-method (collection-empty? obj::%contiguous-stack)
   (contiguous-stack-empty? obj))

(define-method (collection-mutable? obj::%contiguous-stack)
   #t)


;;;; enumerable protocol
;;;; enumerable protocol implementation

(define-method (enumerable? obj::%contiguous-stack)
   #t)

(define-method (enumerable-enumerator obj::%contiguous-stack)
   (instantiate::%contiguous-stack-enumerator (stk obj)))
