(module hoard/priority-queue
   (export (generic priority-queue? obj)
           (generic priority-queue-enqueue! pq itm)
           (generic priority-queue-dequeue! pq)
           (generic priority-queue-first pq)
           (generic priority-queue-length pq)
           (generic priority-queue-fixed-capacity? pq)
           (generic priority-queue-capacity pq)
           (generic priority-queue-empty? pq)
           (generic priority-queue-copy pg)))

(define-generic (priority-queue? obj)
   #f)

(define-generic (priority-queue-enqueue! pq itm))

(define-generic (priority-queue-dequeue! pq))

(define-generic (priority-queue-first pq))

(define-generic (priority-queue-length pq))

(define-generic (priority-queue-fixed-capacity? pq))

(define-generic (priority-queue-capacity pq))

(define-generic (priority-queue-empty? pq))

(define-generic (priority-queue-copy pg))
