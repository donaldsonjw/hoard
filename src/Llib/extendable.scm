(module hoard/extendable
   (export (generic collection-extendable? obj)
           (generic collection-extend! obj item)))


(define-generic (collection-extendable? obj)
   (list? obj))


(define-inline (list-extend! lst item)
   (let ((cur-car (car lst))
         (cur-cdr (cdr lst)))
      (set-car! lst item)
      (set-cdr! lst (cons cur-car cur-cdr))))

(define-generic (collection-extend! obj item)
   (cond ((list? obj)
          (list-extend! obj item))
         (else
          (error "collection-extend!" "non-extendable obj" obj))))
         
