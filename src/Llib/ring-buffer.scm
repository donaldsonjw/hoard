(module hoard/ring-buffer
   (import hoard/exceptions
           hoard/queue
           hoard/deque
           hoard/collection
           hoard/extendable
           hoard/enumerable
           hoard/mutable-collection
           hoard/enumerator)
   (export
      (class %ring-buffer
         length
         store
         (front (default 0))
         (back (default 0)))
      (class %ring-buffer-enumerator
              (curr-idx (default #unspecified))
              rb::%ring-buffer)
      (make-ring-buffer #!key capacity)
      (ring-buffer #!key capacity #!rest objs)
      (ring-buffer-length rg::%ring-buffer)
      (ring-buffer-capacity rb::%ring-buffer)
      (ring-buffer-front rb::%ring-buffer)
      (ring-buffer-back rb::%ring-buffer)
      (ring-buffer-pop-front! rb::%ring-buffer)
      (ring-buffer-pop-back! rb::%ring-buffer)
      (ring-buffer-empty? rb::%ring-buffer)
      (ring-buffer-push-front! rb::%ring-buffer item)
      (ring-buffer-push-back! rb::%ring-buffer item)
      (ring-buffer? obj)
      (ring-buffer-copy rb::%ring-buffer)))


(define (ring-buffer? obj)
   (isa? obj %ring-buffer))

(define (make-ring-buffer #!key capacity)
   (if (and (integer? capacity)
            (> capacity 0))
       (instantiate::%ring-buffer (length 0)
                                  (store (make-vector capacity)))
       (raise-invalid-argument-exception :proc "make-ring-buffer"
          :msg "capacity must be a positive number"
          :args capacity)))

(define (ring-buffer #!key capacity #!rest objs)
   (cond  ((or (not (number? capacity))
               (<= capacity 0))
           (raise-invalid-argument-exception :proc "ring-buffer"
              :msg "capacity must be a positivie number"))
          ((> (length objs) capacity)
              (raise-invalid-argument-exception :proc "ring-buffer"
                                                :msg "capacity must be equal to or larger than the number of objects used to create the ring buffer"
                                                :args objs)) 
        (else (let ((rb (make-ring-buffer :capacity capacity)))
                 (for-each (lambda (v) (ring-buffer-push-back! rb v)) objs)
                 rb))))


(define (ring-buffer-capacity rb::%ring-buffer)
   (vector-length (-> rb store)))

(define (ring-buffer-length rb::%ring-buffer)
   (-> rb length))

(define (ring-buffer-empty? rb::%ring-buffer)
   (= (-> rb length) 0))

(define (ring-buffer-push-back! rb::%ring-buffer item)
   (define (next-index curr capacity)
      (modulo (+ curr 1) capacity))
   (if (= (ring-buffer-length rb) (ring-buffer-capacity rb))
       (raise-invalid-state-exception :proc "ring-buffer-push-back!"
          :msg "cannot push back an item on a full ring-buffer"
          :obj rb)
       (begin
          (vector-set! (-> rb store) (-> rb back) item)
          (set! (-> rb length) (+ (-> rb length) 1))
          (set! (-> rb back) (next-index (-> rb back) (ring-buffer-capacity rb))))))

(define (ring-buffer-push-front! rb::%ring-buffer item)
   (define (prev-index curr capacity)
      (modulo (- curr 1) capacity))
   (if (= (ring-buffer-length rb) (ring-buffer-capacity rb))
       (raise-invalid-state-exception :proc "ring-buffer-push-front!"
          :msg "cannot push front an item on a full ring-buffer"
          :obj rb)
       (let ((idx (prev-index (-> rb front) (ring-buffer-capacity rb))))
          (vector-set! (-> rb store) idx item)
          (set! (-> rb length) (+ (-> rb length) 1))
          (set! (-> rb front) idx))))

(define (ring-buffer-front rb::%ring-buffer)
   (if (ring-buffer-empty? rb)
       (raise-invalid-state-exception :proc "ring-buffer-front"
          :msg "cannot obtain the front item from an empty queue")
       (vector-ref (-> rb store) (-> rb front))))

(define (ring-buffer-back rb::%ring-buffer)
   (define (prev-index curr capacity)
      (modulo (- curr 1) capacity))
   (if (ring-buffer-empty? rb)
       (raise-invalid-state-exception :proc "ring-buffer-front"
          :msg "cannot obtain the back item from an ring buffer")
       (vector-ref (-> rb store) (prev-index (-> rb back) (ring-buffer-capacity rb)))))


(define (ring-buffer-pop-front! rb::%ring-buffer)
   (define (next-index curr capacity)
      (modulo (+ curr 1) capacity))
   (if (ring-buffer-empty? rb)
       (raise-invalid-state-exception :proc "ring-buffer-pop-front!"
          :msg "cannot pop the front item from an empty ring buffer")
       (let ((res (ring-buffer-front rb)))
          (set! (-> rb length) (- (-> rb length) 1))
          (set! (-> rb front) (next-index (-> rb front) (ring-buffer-capacity rb)))
          res)))


(define (ring-buffer-pop-back! rb::%ring-buffer)
   (define (prev-index curr capacity)
      (modulo (- curr 1) capacity))
   (if (ring-buffer-empty? rb)
       (raise-invalid-state-exception :proc "ring-buffer-pop-back!"
          :msg "cannot pop the back item from an empty ring buffer")
       (let ((res (ring-buffer-back rb)))
          (set! (-> rb length) (- (-> rb length) 1))
          (set! (-> rb back) (prev-index (-> rb back) (ring-buffer-capacity rb)))
          res)))

(define (ring-buffer-copy rb::%ring-buffer)
   (instantiate::%ring-buffer (length (-> rb length))
                              (store (vector-copy (-> rb store)
                                        0 (vector-length (-> rb store))))
                              (front (-> rb front))
                              (back (-> rb back))))


;;;; ring-buffer enumerator implementation
(define-method (enumerator? enumerator::%ring-buffer-enumerator)
   #t)

(define-method (enumerator-move-next! enumerator::%ring-buffer-enumerator)
   (define (next-index curr capacity)
      (modulo (+ curr 1) capacity))
   (cond ((eq? (-> enumerator curr-idx) #unspecified)
          (set! (-> enumerator curr-idx) (-> enumerator rb front))
          (not (ring-buffer-empty? (-> enumerator rb))))
         ((not (= (next-index (-> enumerator curr-idx)
                     (ring-buffer-capacity (-> enumerator rb)))
                  (-> enumerator rb back)))
          (set! (-> enumerator curr-idx) (next-index (-> enumerator curr-idx)
                                            (ring-buffer-capacity (-> enumerator rb))))
          #t)
         (else 
          #f)))

(define-method (enumerator-copy enumerator::%ring-buffer-enumerator)
   (duplicate::%ring-buffer-enumerator enumerator))

(define-method (enumerator-current enumerator::%ring-buffer-enumerator)
   ;;; not started
   (if (eq? (-> enumerator curr-idx) #unspecified)
       (raise-invalid-state-exception :proc "enumerator-current"
          :msg "invalid state; enumerator-move-next must be called before enumerator-current"
          :obj enumerator)
       (vector-ref (-> enumerator rb store) (-> enumerator curr-idx))))

;;;; queue protocol
(define-method (queue? obj::%ring-buffer)
   #t)

(define-method (queue-enqueue! obj::%ring-buffer item)
   (ring-buffer-push-back! obj item))

(define-method (queue-dequeue! obj::%ring-buffer)
   (ring-buffer-pop-front! obj))

(define-method (queue-first obj::%ring-buffer)
   (ring-buffer-front obj))

(define-method (queue-length obj::%ring-buffer)
   (ring-buffer-length obj))

(define-method (queue-empty? obj::%ring-buffer)
   (ring-buffer-empty? obj))

(define-method (queue-fixed-capacity? obj::%ring-buffer)
   #t)

(define-method (queue-capacity obj::%ring-buffer)
  (ring-buffer-capacity obj))


(define-method (queue-copy obj::%ring-buffer)
   (ring-buffer-copy obj))


;;;; deque protocol implementation
(define-method (deque? obj::%ring-buffer)
   #t)

(define-method (deque-empty? deque::%ring-buffer)
   (ring-buffer-empty? deque))
   
(define-method (deque-first deque::%ring-buffer)
   (ring-buffer-front deque))

(define-method (deque-last deque::%ring-buffer)
   (ring-buffer-back deque))

(define-method (deque-enqueue! deque::%ring-buffer item)
   (ring-buffer-push-back! deque item))

(define-method (deque-enqueue-front! deque::%ring-buffer item)
   (ring-buffer-push-front! deque item))

(define-method (deque-dequeue! deque::%ring-buffer)
   (ring-buffer-pop-front! deque))

(define-method (deque-dequeue-back! deque::%ring-buffer)
   (ring-buffer-pop-back! deque))

(define-method (deque-length deque::%ring-buffer)
   (ring-buffer-length deque))

(define-method (deque-fixed-capacity? deque::%ring-buffer)
   #t)

(define-method (deque-capacity deque::%ring-buffer)
   (ring-buffer-capacity deque))

(define-method (deque-copy deque::%ring-buffer)
   (ring-buffer-copy deque))

;;;; collection protocol
(define-method (collection? obj::%ring-buffer)
   #t)

(define-method (collection-length obj::%ring-buffer)
   (ring-buffer-length obj))

(define-method (collection-enumerator obj::%ring-buffer)
   (instantiate::%ring-buffer-enumerator (rb obj)))

(define-method (collection-contains? obj::%ring-buffer itm)
   (let ((enumer (collection-enumerator obj)))
      (bind-exit (return)
         (let loop ((cont (enumerator-move-next! enumer)))
            (if cont
                (if (equal? (enumerator-current enumer) itm)
                    (return #t)
                    (loop (enumerator-move-next! enumer)))
                (return #f))))))
         
(define-method (collection-empty? obj::%ring-buffer)
   (ring-buffer-empty? obj))

(define-method (collection-copy obj::%ring-buffer)
   (ring-buffer-copy obj))

(define-method (collection-mutable? obj::%ring-buffer)
   #t)

;;;; extendable protocol implementation
(define-method (collection-extendable? obj::%ring-buffer)
   #t)

(define-method (collection-extend! obj::%ring-buffer itm)
   (ring-buffer-push-back! obj itm))

;;;; enumerable protocol implementation

(define-method (enumerable? obj::%ring-buffer)
   #t)

(define-method (enumerable-enumerator obj::%ring-buffer)
   (instantiate::%ring-buffer-enumerator (rb obj)))
