(module hoard/binary-heap
   (import hoard/priority-queue
           hoard/collection
           hoard/mutable-collection
           hoard/comparator
           hoard/enumerator
           hoard/enumerable
           hoard/exceptions)
   (export (class %binary-heap
              store
              (idx (default 0))
              comparator)
           (class %binary-heap-enumerator
              heap::%binary-heap
              (curr-idx (default #unspecified)))
           (inline binary-heap? obj)
           (inline make-binary-heap #!key capacity comparator)
           (inline binary-heap #!key capacity comparator #!rest vals)
           (inline binary-heap-enqueue! heap::%binary-heap itm)
           (inline binary-heap-dequeue! heap::%binary-heap)
           (inline binary-heap-length heap::%binary-heap)
           (inline binary-heap-first heap::%binary-heap)
           (inline binary-heap-capacity heap::%binary-heap)
           (inline binary-heap-empty? heap::%binary-heap)
           (inline binary-heap-bubble-down! vec start fin comparator)
           (inline binary-heap-heapify! vec len comparator)))


(define-inline (binary-heap? obj)
   (isa? obj %binary-heap))

(define-inline (make-binary-heap #!key capacity comparator)
   (when (or (not (integer? capacity))
             (not (comparator? comparator)))
      (raise-invalid-argument-exception :proc "make-binary-heap"
         :msg "capacity must be an integer and comparator a comparator object"
         :args (list :capacity capacity :comparator comparator)))
   (instantiate::%binary-heap (store (make-vector capacity))
                              (comparator comparator)))

(define-inline (binary-heap #!key capacity comparator #!rest vals)
   (when (or (not (integer? capacity))
             (not (comparator? comparator)))
      (raise-invalid-argument-exception :proc "binary-heap"
         :msg "capacity must be an integer and comparator a comparator object"
         :args (list :capacity capacity :comparator comparator)))   
   (let ((res::%binary-heap (instantiate::%binary-heap (store (make-vector capacity))
                                                       (comparator comparator))))
      (do ((i 0 (+ i 1))
           (lst vals (cdr lst)))
          ((null? lst))
          (vector-set! (-> res store) i (car lst)))
      (set! (-> res idx) (length vals))
      (binary-heap-heapify! (-> res store) (length vals) comparator)
      res))

(define-inline (binary-heap-heapify! vec len comparator)
   (do ((i (/fx len 2) (- i 1)))
       ((< i 0))
       (binary-heap-bubble-down! vec i len comparator))) 
   
(define-inline (binary-heap-empty? heap::%binary-heap)
   (= 0 (-> heap idx)))

(define-inline (binary-heap-length heap::%binary-heap)
   (-> heap idx))

(define-inline (binary-heap-enqueue! heap::%binary-heap itm)
   (define (parent-idx idx)
      (/fx (- idx 1) 2))
   (define (bubble-up! vec idx comparator)
      (let loop ((i idx)
                 (p (parent-idx idx)))
         (if (or (= i 0)
                 (comparator<? comparator (vector-ref vec p)
                    (vector-ref vec i)))
             #unspecified
             (begin
                (let ((temp (vector-ref vec i)))
                   (vector-set! vec i (vector-ref vec p))
                   (vector-set! vec p temp)
                   (loop p (parent-idx p)))))))
   (if (= (-> heap idx) (vector-length (-> heap store)))
       (raise-invalid-state-exception :proc "binary-heap-enqueue!"
          :msg "cannot enqueue an item on a full heap"
          :obj heap)
       (begin
          (vector-set! (-> heap store) (-> heap idx) itm)
          (bubble-up! (-> heap store) (-> heap idx) (-> heap comparator))
          (set! (-> heap idx) (+ (-> heap idx) 1)))))


(define-inline (binary-heap-bubble-down! vec start fin comparator)
   (define (left-child-idx idx)
      (+ (* 2 idx) 1))
   (define (right-child-idx idx)
      (+ (* 2 idx) 2))
   (let loop ((i start)
              (l (left-child-idx 0))
              (r (right-child-idx 0)))
      (let ((gtl  (and (< l fin)
                       (comparator<? comparator (vector-ref vec l)
                          (vector-ref vec i))))
            (gtr  (and (< r fin)
                       (comparator<? comparator (vector-ref vec r)
                          (vector-ref vec i)))))
         (if (or (>=  i fin)
                 (not (or gtl
                          gtr)))
             #unspecified
             (let ((temp (vector-ref vec i))
                   (ci (cond ((and gtl (not gtr))
                              l)
                             ((and gtr (not gtl))
                              r)
                             (else
                              (if (comparator<? comparator (vector-ref vec l)
                                     (vector-ref vec r))
                                  l r)))))
                (vector-set! vec i (vector-ref vec ci))
                   (vector-set! vec ci temp)
                   (loop ci
                      (left-child-idx ci)
                      (right-child-idx ci)))))))

(define-inline (binary-heap-dequeue! heap::%binary-heap)
   (if (= (-> heap idx) 0)
       (raise-invalid-state-exception :proc "binary-heap-dequeue!"
          :msg "cannot dequeue an item from an empty heap"
          :obj heap)
       (let ((res (vector-ref (-> heap store) 0)))
          (vector-set! (-> heap store) 0
             (vector-ref (-> heap store) (- (-> heap idx) 1)))
          (set! (-> heap idx) (- (-> heap idx) 1))
          (binary-heap-bubble-down! (-> heap store) 0 (-> heap idx) (-> heap comparator))
          res)))

(define-inline (binary-heap-first heap::%binary-heap)
   (if (= (-> heap idx) 0)
       (raise-invalid-state-exception :proc "binary-heap-first"
          :msg "cannot obtain the first item of an empty heap"
          :obj heap)
       (vector-ref (-> heap store) 0)))

(define-inline (binary-heap-capacity heap::%binary-heap)
   (vector-length (-> heap store)))

;;;; priority-queue protocol implementation
(define-method (priority-queue? obj::%binary-heap)
   #t)

(define-method (priority-queue-enqueue! pq::%binary-heap itm)
   (binary-heap-enqueue! pq itm))

(define-method (priority-queue-dequeue! pq::%binary-heap)
   (binary-heap-dequeue! pq))

(define-method (priority-queue-first pq::%binary-heap)
   (binary-heap-first pq))

(define-method (priority-queue-length pq::%binary-heap)
   (binary-heap-length pq))

(define-method (priority-queue-fixed-capacity? pq::%binary-heap)
   #t)

(define-method (priority-queue-capacity pq::%binary-heap)
   (binary-heap-capacity pq))

(define-method (priority-queue-empty? pq::%binary-heap)
   (binary-heap-empty? pq))

;;;; %binary-heap-enumerator implementation

(define-method (enumerator? enumerator::%binary-heap-enumerator)
   #t)

(define-method (enumerator-move-next! enumerator::%binary-heap-enumerator)
   (cond ((eq? (-> enumerator curr-idx) #unspecified)
          (set! (-> enumerator curr-idx) 0)
          (not (binary-heap-empty? (-> enumerator heap))))
         ((not (= (+ (-> enumerator curr-idx) 1)
                  (binary-heap-length (-> enumerator heap))))
          (set! (-> enumerator curr-idx) (+ (-> enumerator curr-idx) 1))
          #t)
         (else 
          #f)))


(define-method (enumerator-clone enumerator::%binary-heap-enumerator)
   (duplicate::%binary-heap-enumerator enumerator))

(define-method (enumerator-current enumerator::%binary-heap-enumerator)
   ;;; not started
   (if (eq? (-> enumerator curr-idx) #unspecified)
       (raise-invalid-state-exception :proc "enumerator-current"
          :msg "invalid state; enumerator-move-next must be called before enumerator-current"
          :obj enumerator)
       (vector-ref (-> enumerator heap store) (-> enumerator curr-idx))))

;;;; collection protocol implementation
(define-method (collection? obj::%binary-heap)
   #t)

(define-method (collection-length obj::%binary-heap)
   (binary-heap-length obj))

(define-method (collection-enumerator obj::%binary-heap)
   (instantiate::%binary-heap-enumerator (heap obj)))

(define-method (collection-contains? obj::%binary-heap itm)
   (let ((enumer (collection-enumerator obj)))
      (bind-exit (return)
         (let loop ((cont (enumerator-move-next! enumer)))
            (if cont
                (if (equal? (enumerator-current enumer) itm)
                    (return #t)
                    (loop (enumerator-move-next! enumer)))
                (return #f))))))
         
(define-method (collection-empty? obj::%binary-heap)
   (binary-heap-empty? obj))

(define-method (collection-mutable? obj::%binary-heap)
   #t)

;;;; enumerable protocol implementation
(define-method (enumerable? obj::%binary-heap)
   #t)

(define-method (enumerable-enumerator obj::%binary-heap)
   (instantiate::%binary-heap-enumerator (heap obj)))
