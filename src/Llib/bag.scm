(module hoard/bag
   (export (generic bag? obj)
           (generic bag-insert! bag item)
           (generic bag-delete! bag item)
           (generic bag-contains? bag item)
           (generic bag-count bag item)
           (generic bag-count-set! bag item count)
           (generic bag-length bag)
           (generic bag-empty? bag)
           (generic bag-copy ob)))

;;;; bag protocol
(define-generic (bag? obj)
   #f)

(define-generic (bag-copy ob))

(define-generic (bag-empty? bag))

(define-generic (bag-insert! bag item))

(define-generic (bag-delete! bag item))

(define-generic (bag-contains? bag item))

(define-generic (bag-count bag item))

(define-generic (bag-count-set! bag item count))

(define-generic (bag-length bag))

   