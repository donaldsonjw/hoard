(module hoard/extendable
   (import hoard/exceptions
           hoard/association)
   (export (generic collection-extendable? obj)
           (generic collection-extend! obj item)))


(define-generic (collection-extendable? obj)
   (or (list? obj)
       (hashtable? obj)))


(define (list-extend! lst item)
   (let ((cur-car (car lst))
         (cur-cdr (cdr lst)))
      (set-car! lst item)
      (set-cdr! lst (cons cur-car cur-cdr))
      lst))

(define (hashtable-extend! table item)
   (if (association? item)
       (begin
          (hashtable-put! table (=>key item)
             (=>value item))
          table)
       (raise-invalid-argument-exception :proc "collection-extend!"
          :args (list table item))))

(define-generic (collection-extend! obj item)
   (cond ((list? obj)
          (list-extend! obj item))
         ((hashtable? obj)
          (hashtable-extend! obj item))
         (else
          (raise-unsupported-operation-exception proc: "collection-extend!" obj: obj))))
         
