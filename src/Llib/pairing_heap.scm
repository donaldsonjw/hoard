(module hoard/pairing-heap
   (import hoard/priority-queue
           hoard/extendable
           hoard/enumerable
           hoard/comparator
           hoard/collection
           hoard/mutable-collection
           hoard/enumerator
           hoard/exceptions)
   (export
      (class %pairing-heap
         node::%pairing-node
         (length (default 0))
         comparator)
      (class %pairing-node
         elem
         (subheaps::pair-nil (default '())))
      (class %pairing-heap-enumerator
         (started (default #f))
         nodes::pair-nil)
      ( make-pairing-heap #!key comparator)
      ( pairing-heap #!key comparator #!rest vals)
      ( pairing-heap? obj)
      ( pairing-heap-empty? heap::%pairing-heap)
      ( pairing-heap-enqueue! heap::%pairing-heap itm)
      ( singleton-pairing-heap itm comparator)
      ( pairing-heap-merge! heap1::%pairing-heap heap2::%pairing-heap)
      ( pairing-heap-dequeue! heap::%pairing-heap)
      ( pairing-node-dequeue! node::%pairing-node comparator)
      ( pairing-node-merge-pairs! subheaps comparator)
      ( pairing-node-enqueue! node::%pairing-node itm comparator)
      ( pairing-node-merge! node1::%pairing-node node2::%pairing-node comparator)
      ( pairing-heap-first heap::%pairing-heap)
      ( pairing-heap-length heap::%pairing-heap)
      ( pairing-heap-copy heap::%pairing-heap)
      ( pairing-node-copy node::%pairing-node)))


(define (pairing-heap? obj)
   (isa? obj %pairing-heap))

(define (make-pairing-heap #!key comparator)
   (when (not (comparator? comparator))
      (raise-invalid-argument-exception :proc "make-pairing-heap"
         :msg "comparator must be a comparator object"
         :args (list :comparator comparator)))
   (instantiate::%pairing-heap (node (class-nil %pairing-node))
                               (comparator comparator)))

(define (pairing-heap #!key comparator #!rest vals)
   (when (not (comparator? comparator))
      (raise-invalid-argument-exception :proc "pairing-heap"
         :msg "comparator must be a comparator object"
         :args (list :comparator comparator)))
   (let ((res (instantiate::%pairing-heap (comparator comparator) (node (class-nil %pairing-node)))))
      (for-each (lambda (itm) (pairing-heap-enqueue! res itm)) vals)
      res))

(define (pairing-heap-length heap::%pairing-heap)
   (-> heap length))

(define (pairing-heap-empty? heap::%pairing-heap)
   (eq? (-> heap node) (class-nil %pairing-node)))

(define (pairing-heap-copy heap::%pairing-heap)
   (duplicate::%pairing-heap heap (node (pairing-node-copy (-> heap node)))))

(define (singleton-pairing-heap itm comparator)
   (instantiate::%pairing-heap (node (instantiate::%pairing-node (elem itm)))
                               (comparator comparator)))

(define (pairing-node-copy node::%pairing-node)
   (duplicate::%pairing-node node (subheaps (map pairing-node-copy (-> node subheaps)))))

(define (pairing-node-merge! node1::%pairing-node node2::%pairing-node comparator)
   (cond ((eq? node1 (class-nil %pairing-node))
          node2)
         ((eq? node2 (class-nil %pairing-node))
          node1)
         ((comparator<? comparator (-> node1 elem) (-> node2 elem))
          (set! (-> node1 subheaps) (cons node2 (-> node1 subheaps)))
          node1)
         (else
          (set! (-> node2 subheaps) (cons node1 (-> node2 subheaps)))
          node2)))

(define (pairing-node-enqueue! node::%pairing-node itm comparator)
   (let ((new-node::%pairing-node (instantiate::%pairing-node (elem itm))))
      (pairing-node-merge! node new-node comparator)))

(define (pairing-heap-enqueue! heap::%pairing-heap itm)
   (set! (-> heap node) (pairing-node-enqueue! (-> heap node) itm (-> heap comparator)))
   (set! (-> heap length) (+ (-> heap length) 1))
   heap)

(define (pairing-heap-first heap::%pairing-heap)
   (-> heap node elem))

(define (pairing-heap-dequeue! heap::%pairing-heap)
   (if (pairing-heap-empty? heap)
       (raise-invalid-state-exception :proc "pairing-heap-dequeue!"
          :msg "cannot dequeue an item from an empty heap"
          :obj heap)
       (receive (res new-node)
          (pairing-node-dequeue! (-> heap node) (-> heap comparator))
          (set! (-> heap node) new-node)
          (set! (-> heap length) (- (-> heap length) 1))
          res)))

(define (pairing-node-merge-pairs! subheaps comparator)
   (let ((len (length subheaps)))
      (cond ((= 0 len)
             (class-nil %pairing-node))
            ((= 1 len)
             (car subheaps))
            (else
             (pairing-node-merge! (pairing-node-merge! (car subheaps)
                                     (cadr subheaps) comparator)
                (pairing-node-merge-pairs! (cddr subheaps) comparator) comparator)))))

(define (pairing-node-dequeue! node::%pairing-node comparator)
   (values (-> node elem) (pairing-node-merge-pairs! (-> node subheaps) comparator)))
   
(define (pairing-heap-merge! heap1::%pairing-heap heap2::%pairing-heap)
   (set! (-> heap1 node) (pairing-node-merge! (-> heap1 node) (-> heap2 node) (-> heap1 comparator)))
   heap1)


;;;; priority-queue protocol implementation
(define-method (priority-queue? obj::%pairing-heap)
   #t)

(define-method (priority-queue-enqueue! pq::%pairing-heap itm)
   (pairing-heap-enqueue! pq itm))

(define-method (priority-queue-dequeue! pq::%pairing-heap)
   (pairing-heap-dequeue! pq))

(define-method (priority-queue-first pq::%pairing-heap)
   (pairing-heap-first pq))

(define-method (priority-queue-length pq::%pairing-heap)
   (pairing-heap-length pq))

(define-method (priority-queue-fixed-capacity? pq::%pairing-heap)
   #f)

(define-method (priority-queue-capacity pq::%pairing-heap)
   #unspecified)

(define-method (priority-queue-empty? pq::%pairing-heap)
   (pairing-heap-empty? pq))

(define-method (priority-queue-copy pq::%pairing-heap)
   (pairing-heap-copy pq))

;;;; %pairing-heap-enumerator implementation
(define-method (enumerator? enumerator::%pairing-heap-enumerator)
   #t)

(define-method (enumerator-move-next! enumerator::%pairing-heap-enumerator)
   (cond ((eq? #f (-> enumerator started))
         (set! (-> enumerator started) #t)
         (pair? (-> enumerator nodes)))
         ((pair? (-> enumerator nodes))
         (let ((curr::%pairing-node (car (-> enumerator nodes))))
            (set! (-> enumerator nodes) (if (pair? (-> curr subheaps))
                                            (append (-> curr subheaps)
                                               (cdr (-> enumerator nodes)))
                                            (cdr (-> enumerator nodes))))
            (not (null? (-> enumerator nodes)))))
        (else 
         #f)))

(define-method (enumerator-copy enumerator::%pairing-heap-enumerator)
   (duplicate::%pairing-heap-enumerator enumerator (nodes (list-copy (-> enumerator nodes)))))

(define-method (enumerator-current enumerator::%pairing-heap-enumerator)
   (if (not (-> enumerator started))
       (raise-invalid-state-exception :proc "enumerator-current"
          :msg "invalid state; enumerator-move-next must be called before enumerator-current"
          :obj enumerator)
       (let ((curr::%pairing-node (car (-> enumerator nodes))))
          (-> curr elem))))

;;;; collection protocol implementation
(define-method (collection? obj::%pairing-heap)
   #t)

(define-method (collection-length obj::%pairing-heap)
   (pairing-heap-length obj))

(define-method (collection-enumerator obj::%pairing-heap)
   (instantiate::%pairing-heap-enumerator (nodes (if (not (pairing-heap-empty? obj))
                                                     (list (-> obj node))
                                                     '()))))

(define-method (collection-contains? obj::%pairing-heap itm)
   (let ((enumer (collection-enumerator obj)))
      (bind-exit (return)
         (let loop ((cont (enumerator-move-next! enumer)))
            (if cont
                (if (equal? (enumerator-current enumer) itm)
                    (return #t)
                    (loop (enumerator-move-next! enumer)))
                (return #f))))))
         
(define-method (collection-empty? obj::%pairing-heap)
   (pairing-heap-empty? obj))

(define-method (collection-copy obj::%pairing-heap)
   (pairing-heap-copy obj))

(define-method (collection-mutable? obj::%pairing-heap)
   #t)


;;; extendable protocol implementation
(define-method (collection-extendable? obj::%pairing-heap)
   #t)

(define-method (collection-extend! obj::%pairing-heap itm)
   (pairing-heap-enqueue! obj itm))

;;;; enumerable protocol implementation

(define-method (enumerable? obj::%pairing-heap)
   #t)

(define-method (enumerable-enumerator obj::%pairing-heap)
   (instantiate::%pairing-heap-enumerator (nodes (if (not (pairing-heap-empty? obj))
                                                     (list (-> obj node))
                                                     '()))))