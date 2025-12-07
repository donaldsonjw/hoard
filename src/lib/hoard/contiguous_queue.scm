(module hoard/contiguous-queue
   (import hoard/collection
           hoard/ring-buffer
           hoard/mutable-collection
           hoard/extendable
           hoard/queue
           hoard/enumerator
           hoard/enumerable
           hoard/exceptions)
   (export (class %contiguous-queue::%ring-buffer)
           (class %contiguous-queue-enumerator
              (curr-idx (default #unspecified))
              q::%contiguous-queue)
           (make-contiguous-queue #!key capacity)
           (contiguous-queue #!key capacity #!rest objs)
           (contiguous-queue-empty? q::%contiguous-queue)
           (contiguous-queue-length q::%contiguous-queue)
           (contiguous-queue-enqueue! q::%contiguous-queue item)
           (contiguous-queue? obj)
           (contiguous-queue-capacity q::%contiguous-queue)
           (contiguous-queue-dequeue! q::%contiguous-queue)
           (contiguous-queue-first q::%contiguous-queue)
           (contiguous-queue-copy q::%contiguous-queue)))


(define (contiguous-queue? obj)
   (isa? obj %contiguous-queue))

(define (make-contiguous-queue #!key capacity)
   (if (and (integer? capacity)
            (> capacity 0))
       (instantiate::%contiguous-queue (length 0)
                                  (store (make-vector capacity)))
       (raise-invalid-argument-exception :proc "make-contiguous-queue"
          :msg "capacity must be a positive number"
          :args capacity)))

(define (contiguous-queue-capacity q::%contiguous-queue)
   (ring-buffer-capacity q))

(define (contiguous-queue #!key capacity #!rest objs)
   (cond  ((or (not (number? capacity))
               (<= capacity 0))
           (raise-invalid-argument-exception :proc "contiguous-queue"
              :msg "capacity must be a positivie number"))
          ((> (length objs) capacity)
              (raise-invalid-argument-exception :proc "contiguous-queue"
                                                :msg "capacity must be equal to or larger than the number of objects used to create the contiguous-queue"
                                                :args objs)) 
        (else (let ((rb (make-contiguous-queue :capacity capacity)))
                 (for-each (lambda (v) (ring-buffer-push-back! rb v)) objs)
                 rb))))

(define (contiguous-queue-empty? q::%contiguous-queue)
   (ring-buffer-empty? q))

(define (contiguous-queue-length q::%contiguous-queue)
   (ring-buffer-length q)) 

(define (contiguous-queue-enqueue! q::%contiguous-queue item)
   (ring-buffer-push-back! q item))

(define (contiguous-queue-dequeue! q::%contiguous-queue)
   (ring-buffer-pop-front! q))


(define (contiguous-queue-first q::%contiguous-queue)
   (ring-buffer-front q))

(define (contiguous-queue-copy q::%contiguous-queue)
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

;;;; extendable protocol implementation
(define-method (collection-extendable? obj::%contiguous-queue)
   #t)

(define-method (collection-extend! obj::%contiguous-queue itm)
   (contiguous-queue-enqueue! obj itm))

;;;; enumerable protocol implementation

(define-method (enumerable? obj::%contiguous-queue)
   #t)

(define-method (enumerable-enumerator obj::%contiguous-queue)
   (instantiate::%contiguous-queue-enumerator (q obj)))

