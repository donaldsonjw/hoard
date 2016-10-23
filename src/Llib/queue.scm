(module hoard/queue
   (export (generic queue? obj)
           (generic queue-first queue)
           (generic queue-enqueue! queue item)
           (generic queue-dequeue! queue)
           (generic queue-length queue)
           (generic queue-empty? obj)))




;;;; generic queue protocol

(define-generic (queue? obj)
   #f)

(define-generic (queue-empty? obj))

(define-generic (queue-first queue))

(define-generic (queue-enqueue! queue item))

(define-generic (queue-dequeue! queue))

(define-generic (queue-length queue))

