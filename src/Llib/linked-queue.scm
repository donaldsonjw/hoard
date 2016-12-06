(module hoard/linked-queue
   (import hoard/exceptions
           hoard/collection
           hoard/mutable-collection
           hoard/queue
           hoard/enumerable
           hoard/enumerator)
   (export
      (class %linked-queue
         (size (default 0))
         (first::pair-nil (default '()))
         (last::pair-nil (default '())))
      (inline linked-queue-length q::%linked-queue)
      (inline linked-queue-enqueue! q::%linked-queue item)
      (inline linked-queue-dequeue! q::%linked-queue)
      (inline linked-queue-first q::%linked-queue)
      (inline make-linked-queue)
      (inline linked-queue? q)
      (inline linked-queue #!rest objs)
      (inline linked-queue-empty? q::%linked-queue)))


(define-inline (make-linked-queue)
   (instantiate::%linked-queue))

(define-inline (linked-queue #!rest objs)
   (let ((q (make-linked-queue)))
      (for-each (lambda (i) (linked-queue-enqueue! q i)) objs)
      q))

(define-inline (linked-queue? q)
   (isa? q %linked-queue))

(define-inline (linked-queue-empty? q::%linked-queue)
   (and (= (-> q size) 0)
        (eq? (-> q first) '())
        (eq? (-> q last) '())))

(define-inline (linked-queue-length q::%linked-queue)
   (-> q size))

(define-inline (linked-queue-enqueue! q::%linked-queue item)
    (let ((new-last (list item)))
       (if (= (-> q size) 0)
           (begin
              (set! (-> q first) new-last)
              (set! (-> q last) new-last))
           (begin
              (set-cdr! (-> q last) new-last)
              (set! (-> q last) new-last)))
       (set!  (-> q size) (+ (-> q size) 1))))

(define-inline (linked-queue-dequeue! q::%linked-queue)
   (if (= (-> q size) 0)
       (raise-invalid-state-exception proc: "qeueu-dequeue!"
          :msg "can not dequeue from an empty queue"
          :obj q)
       (let ((res (car (-> q first))))
          (set! (-> q size) (- (-> q size) 1))
          (set! (-> q first) (cdr (-> q first)))
          (when (= (-> q size) 0)
             (set! (-> q last) (-> q first)))
          res)))

(define-inline (linked-queue-first q::%linked-queue)
   (if (= (-> q size) 0)
       (raise-invalid-state-exception proc: "qeueu-first"
          :msg "can not obtain the first item from an empty queue"
          :obj q)
       (car (-> q first))))


;;;; queue protocol
(define-method (queue? obj::%linked-queue)
   #t)

(define-method (queue-enqueue! obj::%linked-queue item)
   (linked-queue-enqueue! obj item))

(define-method (queue-dequeue! obj::%linked-queue)
   (linked-queue-dequeue! obj))

(define-method (queue-first obj::%linked-queue)
   (linked-queue-first obj))

(define-method (queue-length obj::%linked-queue)
   (linked-queue-length obj))

(define-method (queue-empty? obj::%linked-queue)
   (linked-queue-empty? obj))

(define-method (queue-fixed-capacity? obj::%linked-queue)
   #f)

(define-method (queue-capacity obj::%linked-queue)
   #unspecified)

;;;; collection protocol

(define-method (collection? obj::%linked-queue)
   #t)

(define-method (collection-length obj::%linked-queue)
   (linked-queue-length obj))

(define-method (collection-enumerator obj::%linked-queue)
   (collection-enumerator (-> obj first)))

(define-method (collection-contains? obj::%linked-queue itm)
   (collection-contains? (-> obj first) itm))

(define-method (collection-empty? obj::%linked-queue)
   (linked-queue-empty? obj))

(define-method (collection-mutable? obj::%linked-queue)
   #t)

;;;; enumerable protocol implementation
(define-method (enumerable? obj::%linked-queue)
   #t)

(define-method (enumerable-enumerator obj::%linked-queue)
   (enumerable-enumerator (-> obj first)))
