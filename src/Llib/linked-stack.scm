(module hoard/linked-stack
   (import hoard/stack
           hoard/exceptions
           hoard/collection
           hoard/mutable-collection
           hoard/enumerable
           hoard/enumerator)
   (export
      (class %linked-stack
         (length (default 0))
         (items (default '())))
      (inline linked-stack? obj)
      (inline make-linked-stack)
      (inline linked-stack-push! stk::%linked-stack val)
      (inline linked-stack-pop! stk::%linked-stack)
      (inline linked-stack-top stk::%linked-stack)
      (inline linked-stack-empty? stk::%linked-stack)
      (inline linked-stack #!rest objs)
      (inline linked-stack-length stk::%linked-stack)))

(define-inline (linked-stack? obj)
   (isa? obj %linked-stack))

(define-inline (make-linked-stack)
   (instantiate::%linked-stack))

(define-inline (linked-stack #!rest objs)
   (instantiate::%linked-stack (items objs)
                               (length (length objs))))

(define-inline (linked-stack-empty? stk::%linked-stack)
   (= (-> stk length) 0))

(define-inline (linked-stack-push! stk::%linked-stack val)
   (set! (-> stk items) (cons val (-> stk items)))
   (set! (-> stk length) (+ (-> stk length) 1)))

(define-inline (linked-stack-pop! stk::%linked-stack)
   (if (not (linked-stack-empty? stk))
       (let ((res (car (-> stk items))))
          (set! (-> stk items) (cdr (-> stk items)))
          (set! (-> stk length) (- (-> stk length) 1))
          res)
       (raise-invalid-state-exception :proc "linked-stack-pop!"
          :msg "cannot pop an item off of an empty stack"
          :obj stk)))

(define-inline (linked-stack-top stk::%linked-stack)
   (if (not (linked-stack-empty? stk))
       (car (-> stk items))
       (raise-invalid-state-exception :proc "linked-stack-top"
          :msg "cannot pop an item off of an empty stack"
          :obj stk)))

(define-inline (linked-stack-length stk::%linked-stack)
   (-> stk length))


;;;; implementation of generic stack protocol

(define-method (stack? obj::%linked-stack)
   #t)

(define-method (stack-push! obj::%linked-stack val)
   (linked-stack-push! obj val))

(define-method (stack-pop! obj::%linked-stack)
   (linked-stack-pop! obj))

(define-method (stack-top obj::%linked-stack)
   (linked-stack-top obj))

(define-method (stack-length obj::%linked-stack)
   (linked-stack-length obj))

;;;; collection protocol

(define-method (collection? obj::%linked-stack)
   #t)

(define-method (collection-empty? obj::%linked-stack)
   (linked-stack-empty? obj))

(define-method (collection-length obj::%linked-stack)
   (linked-stack-length obj))

(define-method (collection-enumerator obj::%linked-stack)
   (collection-enumerator (-> obj items)))

(define-method (collection-contains? obj::%linked-stack itm)
   (collection-contains? (-> obj items) itm))

(define-method (collection-mutable? obj::%linked-stack)
   #t)

;;;; enumerable protocol implementation
(define-method (enumerable? obj::%linked-stack)
   #t)

(define-method (enumerable-enumerator obj::%linked-stack)
   (enumerable-enumerator (-> obj items)))





