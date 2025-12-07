(module hoard/linked-queue
   (import hoard/exceptions
           hoard/collection
           hoard/mutable-collection
           hoard/extendable
           hoard/queue
           hoard/enumerable
           hoard/enumerator)
   (export
      (class %linked-queue
         (size (default 0))
         (first::pair-nil (default '()))
         (last::pair-nil (default '())))
      (linked-queue-length q::%linked-queue)
      (linked-queue-enqueue! q::%linked-queue item)
      (linked-queue-dequeue! q::%linked-queue)
      (linked-queue-first q::%linked-queue)
      (make-linked-queue)
      (linked-queue? q)
      (linked-queue #!rest objs)
      (linked-queue-empty? q::%linked-queue)
      (linked-queue-copy q::%linked-queue)))


(define (make-linked-queue)
   (instantiate::%linked-queue))

(define (linked-queue #!rest objs)
   (let ((q (make-linked-queue)))
      (for-each (lambda (i) (linked-queue-enqueue! q i)) objs)
      q))

(define (linked-queue? q)
   (isa? q %linked-queue))

(define (linked-queue-empty? q::%linked-queue)
   (and (= (-> q size) 0)
        (eq? (-> q first) '())
        (eq? (-> q last) '())))

(define (linked-queue-length q::%linked-queue)
   (-> q size))

(define (linked-queue-enqueue! q::%linked-queue item)
    (let ((new-last (list item)))
       (if (= (-> q size) 0)
           (begin
              (set! (-> q first) new-last)
              (set! (-> q last) new-last))
           (begin
              (set-cdr! (-> q last) new-last)
              (set! (-> q last) new-last)))
       (set!  (-> q size) (+ (-> q size) 1))))

(define (linked-queue-dequeue! q::%linked-queue)
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

(define (linked-queue-first q::%linked-queue)
   (if (= (-> q size) 0)
       (raise-invalid-state-exception proc: "qeueu-first"
          :msg "can not obtain the first item from an empty queue"
          :obj q)
       (car (-> q first))))

(define (linked-queue-copy q::%linked-queue)
   (define (find-last lst)
      (let loop ((lst lst))
         (if (pair? lst)
             (if (pair? (cdr lst))
                 (loop (cdr lst))
                 lst)
             lst)))
   (let ((storage (list-copy (-> q first))))
      (instantiate::%linked-queue (size (-> q size))
                                  (first storage)
                                  (last (find-last storage)))))

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

(define-method (queue-copy obj::%linked-queue)
   (linked-queue-copy obj))

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

(define-method (collection-copy obj::%linked-queue)
   (linked-queue-copy obj))

(define-method (collection-mutable? obj::%linked-queue)
   #t)

;;;; extendable protocol implementation
(define-method (collection-extendable? obj::%linked-queue)
   #t)

(define-method (collection-extend! obj::%linked-queue itm)
   (linked-queue-enqueue! obj itm))


;;;; enumerable protocol implementation
(define-method (enumerable? obj::%linked-queue)
   #t)

(define-method (enumerable-enumerator obj::%linked-queue)
   (enumerable-enumerator (-> obj first)))
