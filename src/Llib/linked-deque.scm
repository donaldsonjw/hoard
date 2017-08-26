(module hoard/linked-deque
   (include "enumerable.sch")
   (import hoard/deque
           hoard/queue
           hoard/collection
           hoard/enumerable
           hoard/mutable-collection
           hoard/extendable
           hoard/enumerator
           hoard/exceptions)
   (export
      (class %linked-deque
         (front (default +deque-node-nil+))
         (back (default +deque-node-nil+))
         (length (default 0)))
      +deque-node-nil+
      (make-linked-deque)
      (linked-deque #!rest objs)
      (linked-deque-last deque::%linked-deque)
      (linked-deque-first deque::%linked-deque)
      (linked-deque? obj)
      (linked-deque-dequeue! deque::%linked-deque)
      (linked-deque-empty? deque::%linked-deque)
      (linked-deque-enqueue! deque::%linked-deque item)
      (linked-deque-length deque::%linked-deque)
      (linked-deque-enqueue-front! deque::%linked-deque item)
      (linked-deque-dequeue-back! deque::%linked-deque)
      (linked-deque-copy deque::%linked-deque))

   (static
      (class %deque-node
         val
         (prev::%deque-node (default +deque-node-nil+))
         (next::%deque-node (default +deque-node-nil+)))
      (class %linked-deque-enumerator
             (started (default #f))
             curr::%deque-node)))

(define +deque-node-nil+
   (class-nil %deque-node))

(define (make-linked-deque)
   (instantiate::%linked-deque))

(define (linked-deque #!rest objs)
   (let ((deque (make-linked-deque)))
      (for-each (lambda (obj)
                   (linked-deque-enqueue! deque obj))
         objs)
       deque))

(define (linked-deque? obj)
   (isa? obj %linked-deque))

(define (linked-deque-enqueue! deque::%linked-deque item)
   (let ((new-node::%deque-node (instantiate::%deque-node (val item))))
      (if (> (-> deque length) 0)
          (let ((back-node::%deque-node (-> deque back)))
             (set! (-> back-node next) new-node)
             (set! (-> new-node prev) back-node))
          (set! (-> deque front) new-node))
      (set! (-> deque back) new-node)
      (set! (-> deque length) (+ (-> deque length) 1))))

(define (linked-deque-enqueue-front! deque::%linked-deque item)
   (let ((new-node::%deque-node (instantiate::%deque-node (val item))))
      (if (> (-> deque length) 0)
          (let ((front-node::%deque-node (-> deque front)))
             (set! (-> front-node prev) new-node)
             (set! (-> new-node next) front-node))
          (set! (-> deque back) new-node))
      (set! (-> deque front) new-node)
      (set! (-> deque length) (+ (-> deque length) 1))))

(define (linked-deque-first deque::%linked-deque)
   (if (> (-> deque length) 0)
       (let ((front::%deque-node (-> deque front)))
          (-> front val))
       (raise-invalid-state-exception :proc "linked-deque-first"
          :msg "invalid state; cannot obtain the first item from an empty linked-deque" :obj '())))


(define (linked-deque-last deque::%linked-deque)
   (if (> (-> deque length) 0)
       (let ((back::%deque-node (-> deque back)))
          (-> back val))
       (raise-invalid-state-exception :proc "linked-deque-last"
          :msg "invalid state; cannot obtain the last item from an empty linked-deque" :obj '())))


(define (linked-deque-dequeue! deque::%linked-deque)
   (if (> (-> deque length) 0)
       (let* ((front-node::%deque-node (-> deque front))
              (next-node::%deque-node (-> front-node next)))
          (set! (-> deque front) next-node)
          (when (> (-> deque length) 1)
             (set! (-> next-node prev) +deque-node-nil+))
          (set! (-> deque length) (- (-> deque length) 1))
          (-> front-node val))
       (raise-invalid-state-exception :proc "linked-deque-dequeue!"
          :msg "invalid state; cannot dequeue from an empty linked-deque" :obj '())))


(define (linked-deque-dequeue-back! deque::%linked-deque)
   (if (> (-> deque length) 0)
       (let* ((back-node::%deque-node (-> deque back))
              (prev-node::%deque-node (-> back-node prev)))
          (set! (-> deque back) prev-node)
          (when (> (-> deque length) 1)
             (set! (-> prev-node next) +deque-node-nil+))
          (set! (-> deque length) (- (-> deque length) 1))
          (-> back-node val))
       (raise-invalid-state-exception :proc "linked-deque-dequeue-back!"
          :msg "invalid state; cannot dequeue-back from an empty linked-deque" :obj '())))


(define (linked-deque-empty? deque::%linked-deque)
   (= (-> deque length) 0))

(define (linked-deque-length deque::%linked-deque)
   (-> deque length))


(define (deque-node-copy node::%deque-node)
   (let loop  ((first +deque-node-nil+)
               (curr::%deque-node node)
               (res::%deque-node +deque-node-nil+))
      (if (not (eq? curr +deque-node-nil+))
          (let ((new-node::%deque-node (instantiate::%deque-node (val (-> curr val)))))
             (when (not (eq? res +deque-node-nil+))
                (set! (-> res next) new-node)
                (set! (-> new-node prev) res))
             (loop (if (eq? first +deque-node-nil+) new-node first)
                (-> curr next)
                new-node))
             (cons first res))))

(define (linked-deque-copy deque::%linked-deque)
   (let ((front&back (deque-node-copy (-> deque front))))
      (instantiate::%linked-deque (length (-> deque length))
                               (front (car front&back))
                               (back (cdr front&back)))))

;;;; queue protocol
(define-method (queue? obj::%linked-deque)
   #t)

(define-method (queue-enqueue! obj::%linked-deque item)
   (linked-deque-enqueue! obj item))

(define-method (queue-dequeue! obj::%linked-deque)
   (linked-deque-dequeue! obj))

(define-method (queue-first obj::%linked-deque)
   (linked-deque-first obj))

(define-method (queue-length obj::%linked-deque)
   (linked-deque-length obj))

(define-method (queue-empty? obj::%linked-deque)
   (linked-deque-empty? obj))

(define-method (queue-fixed-capacity? obj::%linked-deque)
   #f)

(define-method (queue-capacity obj::%linked-deque)
  #unspecified)


(define-method (queue-copy obj::%linked-deque)
   (linked-deque-copy obj))


;;;; deque protocol implementation
(define-method (deque? obj::%linked-deque)
   #t)

(define-method (deque-empty? deque::%linked-deque)
   (linked-deque-empty? deque))
   
(define-method (deque-first deque::%linked-deque)
   (linked-deque-first deque))

(define-method (deque-last deque::%linked-deque)
   (linked-deque-last deque))

(define-method (deque-enqueue! deque::%linked-deque item)
   (linked-deque-enqueue! deque item))

(define-method (deque-enqueue-front! deque::%linked-deque item)
   (linked-deque-enqueue-front! deque item))

(define-method (deque-dequeue! deque::%linked-deque)
   (linked-deque-dequeue! deque))

(define-method (deque-dequeue-back! deque::%linked-deque)
   (linked-deque-dequeue-back! deque))

(define-method (deque-fixed-capacity? deque::%linked-deque)
   #f)

(define-method (deque-capacity deque::%linked-deque)
   #unspecified)

(define-method (deque-copy deque::%linked-deque)
   (linked-deque-copy deque))

(define-method (deque-length deque::%linked-deque)
   (linked-deque-length deque))

;;;; linked-deque-enumerator
(define (make-linked-dequeu-enumerator deque::%linked-deque)
  (instantiate::%linked-deque-enumerator (curr (-> deque front))))

(define-method (enumerator? enumer::%linked-deque-enumerator)
  #t)


(define-method (enumerator-move-next! enumer::%linked-deque-enumerator)
  (cond ((and (eq? #f (-> enumer started))
              (not (eq? (-> enumer curr)
                        +deque-node-nil+)))
         (set! (-> enumer started) #t)
         #t)
        ((not (eq? (-> enumer curr)
                   +deque-node-nil+))
         (set! (-> enumer curr) (-> enumer curr next))
         (not (eq? (-> enumer curr)
                   +deque-node-nil+)))
        (else
         #f)))


(define-method (enumerator-copy enumer::%linked-deque-enumerator)
  (duplicate::%linked-deque-enumerator enumer))

(define-method (enumerator-current enumer::%linked-deque-enumerator)
  (if (not (-> enumer started))
      (raise-invalid-state-exception :proc "enumerator-current"
                                     :msg "invalid state; enumerator-move-next! must be called before enumerator-current"
                                     :obj enumer)
      (-> enumer curr val)))


;;;; collection protocol
(define-method (collection? obj::%linked-deque)
   #t)

(define-method (collection-length obj::%linked-deque)
   (linked-deque-length obj))

(define-method (collection-enumerator obj::%linked-deque)
   (make-linked-dequeu-enumerator obj))

(define-method (collection-contains? obj::%linked-deque itm)
   (enumerable-any? (lambda (deque-item) (equal? itm deque-item))
      obj))

(define-method (collection-empty? obj::%linked-deque)
   (linked-deque-empty? obj))

(define-method (collection-copy obj::%linked-deque)
   (linked-deque-copy obj))

(define-method (collection-mutable? obj::%linked-deque)
   #t)

;;;; extendable protocol implementation
(define-method (collection-extendable? obj::%linked-deque)
   #t)

(define-method (collection-extend! obj::%linked-deque itm)
   (linked-deque-enqueue! obj itm))


;;;; enumerable protocol implementation
(define-method (enumerable? obj::%linked-deque)
   #t)

(define-method (enumerable-enumerator obj::%linked-deque)
   (make-linked-dequeu-enumerator obj))


        

