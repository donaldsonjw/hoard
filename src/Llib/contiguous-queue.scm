(module hoard/contiguous-queue
   (import hoard/collection
           hoard/mutable-collection
           hoard/queue
           hoard/enumerator
           hoard/enumerable
           hoard/exceptions)
   (export (class %contiguous-queue
              length
              store
              (front (default 0))
              (back (default 0)))
           (class %contiguous-queue-enumerator
              (curr-idx (default #unspecified))
              q::%contiguous-queue)
           (inline make-contiguous-queue #!key capacity)
           (inline contiguous-queue #!key capacity #!rest objs)
           (inline contiguous-queue-empty? q::%contiguous-queue)
           (inline contiguous-queue-length q::%contiguous-queue)
           (inline contiguous-queue-enqueue! q::%contiguous-queue item)
           (inline contiguous-queue? obj)
           (inline contiguous-queue-capacity q::%contiguous-queue)
           (inline contiguous-queue-dequeue! q::%contiguous-queue)
           (inline contiguous-queue-first q::%contiguous-queue)
           (inline contiguous-queue-copy q::%contiguous-queue)))


(define-inline (contiguous-queue? obj)
   (isa? obj %contiguous-queue))

(define-inline (make-contiguous-queue #!key capacity)
   (instantiate::%contiguous-queue (length 0)
                                   (store (make-vector capacity))))

(define-inline (contiguous-queue-capacity q::%contiguous-queue)
   (vector-length (-> q store)))

(define-inline (contiguous-queue #!key capacity #!rest objs)
   (if (> (length objs) capacity)
       (raise-invalid-argument-exception :proc "contiguous queue"
          :msg "capacity must be equal to or larger than the number of objects used to create the queue"
          :args objs)
       (let ((q (instantiate::%contiguous-queue (length 0)
                                                (store (make-vector capacity)))))
          (for-each (lambda (v) (contiguous-queue-enqueue! q v)) objs)
          q)))

(define-inline (contiguous-queue-empty? q::%contiguous-queue)
   (= (-> q length) 0))

(define-inline (contiguous-queue-length q::%contiguous-queue)
   (-> q length)) 

(define-inline (contiguous-queue-enqueue! q::%contiguous-queue item)
   (define (next-index curr capacity)
      (modulo (+ curr 1) capacity))
   (if (= (contiguous-queue-length q) (contiguous-queue-capacity q))
       (raise-invalid-state-exception :proc "contiguous-queue-enqueue!"
          :msg "cannot enqueue an item on a full queue"
          :obj q)
       (begin
          (vector-set! (-> q store) (-> q back) item)
          (set! (-> q length) (+ (-> q length) 1))
          (set! (-> q back) (next-index (-> q back) (contiguous-queue-capacity q))))))

(define-inline (contiguous-queue-dequeue! q::%contiguous-queue)
   (define (next-index curr capacity)
      (modulo (+ curr 1) capacity))
   (if (= (contiguous-queue-length q) 0)
       (raise-invalid-state-exception :proc "contiguous-queue-dequeue!"
          :msg "cannot dequeue an item from a full queue"
          :obj q)
       (let ((res (vector-ref (-> q store) (-> q front))))
          (set! (-> q length) (- (-> q length) 1))
          (set! (-> q front) (next-index (-> q front) (contiguous-queue-capacity q)))
          res)))


(define-inline (contiguous-queue-first q::%contiguous-queue)
   (if (= (contiguous-queue-length q) 0)
       (raise-invalid-state-exception :proc "contiguous-queue-first"
          :msg "cannot obtain the first item from a full queue"
          :obj q)
       (vector-ref (-> q store) (-> q front))))

(define-inline (contiguous-queue-copy q::%contiguous-queue)
   (instantiate::%contiguous-queue (length (-> q length))
                                   (store (vector-copy (-> q store)
                                             0 (vector-length (-> q store))))
                                   (front (-> q front))
                                   (back (-> q back))))

;;;; %contiguous-queue-enumerator implementation

(define-method (enumerator? enumerator::%contiguous-queue-enumerator)
   #t)

(define-method (enumerator-move-next! enumerator::%contiguous-queue-enumerator)
   (define (next-index curr capacity)
      (modulo (+ curr 1) capacity))
   (cond ((eq? (-> enumerator curr-idx) #unspecified)
          (set! (-> enumerator curr-idx) (-> enumerator q front))
          (not (contiguous-queue-empty? (-> enumerator q))))
         ((not (= (next-index (-> enumerator curr-idx)
                     (contiguous-queue-capacity (-> enumerator q)))
                  (-> enumerator q back)))
          (set! (-> enumerator curr-idx) (next-index (-> enumerator curr-idx)
                                            (contiguous-queue-capacity (-> enumerator q))))
          #t)
         (else 
          #f)))

(define-method (enumerator-copy enumerator::%contiguous-queue-enumerator)
   (duplicate::%contiguous-queue-enumerator enumerator))

(define-method (enumerator-current enumerator::%contiguous-queue-enumerator)
   ;;; not started
   (if (eq? (-> enumerator curr-idx) #unspecified)
       (raise-invalid-state-exception :proc "enumerator-current"
          :msg "invalid state; enumerator-move-next must be called before enumerator-current"
          :obj enumerator)
       (vector-ref (-> enumerator q store) (-> enumerator curr-idx))))

;;;; queue protocol
(define-method (queue? obj::%contiguous-queue)
   #t)

(define-method (queue-enqueue! obj::%contiguous-queue item)
   (contiguous-queue-enqueue! obj item))

(define-method (queue-dequeue! obj::%contiguous-queue)
   (contiguous-queue-dequeue! obj))

(define-method (queue-first obj::%contiguous-queue)
   (contiguous-queue-first obj))

(define-method (queue-length obj::%contiguous-queue)
   (contiguous-queue-length obj))

(define-method (queue-empty? obj::%contiguous-queue)
   (contiguous-queue-empty? obj))

(define-method (queue-fixed-capacity? obj::%contiguous-queue)
   #t)

(define-method (queue-capacity obj::%contiguous-queue)
  (contiguous-queue-capacity obj))


(define-method (queue-copy obj::%contiguous-queue)
   (contiguous-queue-copy obj))

;;;; collection protocol
(define-method (collection? obj::%contiguous-queue)
   #t)

(define-method (collection-length obj::%contiguous-queue)
   (contiguous-queue-length obj))

(define-method (collection-enumerator obj::%contiguous-queue)
   (instantiate::%contiguous-queue-enumerator (q obj)))

(define-method (collection-contains? obj::%contiguous-queue itm)
   (let ((enumer (collection-enumerator obj)))
      (bind-exit (return)
         (let loop ((cont (enumerator-move-next! enumer)))
            (if cont
                (if (equal? (enumerator-current enumer) itm)
                    (return #t)
                    (loop (enumerator-move-next! enumer)))
                (return #f))))))
         
(define-method (collection-empty? obj::%contiguous-queue)
   (contiguous-queue-empty? obj))

(define-method (collection-copy obj::%contiguous-queue)
   (contiguous-queue-copy obj))

(define-method (collection-mutable? obj::%contiguous-queue)
   #t)
   
;;;; enumerable protocol implementation

(define-method (enumerable? obj::%contiguous-queue)
   #t)

(define-method (enumerable-enumerator obj::%contiguous-queue)
   (instantiate::%contiguous-queue-enumerator (q obj)))

