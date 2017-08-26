(module hoard/deque
   (export (generic deque? obj)
           (generic deque-empty? deque)
           (generic deque-first deque)
           (generic deque-last deque)
           (generic deque-enqueue! deque item)
           (generic deque-enqueue-front! deque item)
           (generic deque-dequeue! deque)
           (generic deque-dequeue-back! deque)
           (generic deque-fixed-capacity? deque)
           (generic deque-capacity deque)
           (generic deque-copy deque)
           (generic deque-length deque)))

(define-generic (deque? obj)
   #f)

(define-generic (deque-empty? deque))
   
(define-generic (deque-first deque))

(define-generic (deque-last deque))

(define-generic (deque-enqueue! deque item))

(define-generic (deque-enqueue-front! deque item))

(define-generic (deque-dequeue! deque))

(define-generic (deque-dequeue-back! deque))

(define-generic (deque-fixed-capacity? deque))

(define-generic (deque-capacity deque))

(define-generic (deque-copy deque))

(define-generic (deque-length deque))
