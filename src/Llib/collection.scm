(module hoard/collection
   (import hoard/exceptions
           hoard/enumerator
           hoard/dictionary-enumerator)
   (export
      (generic collection? obj)
      (generic collection-length obj)
      (generic collection-enumerator obj)
      (generic collection-contains? obj itm)
      (generic collection-empty? obj)
      (generic collection-first  obj)
      (generic collection-rest obj)))


;;;; collections protocol

;; is the obj a collection
(define-generic (collection? obj)
   (or (list? obj)
       (vector? obj)
       (hashtable? obj)
       (string? obj)))

(define-generic (collection-empty? obj)
   (cond ((list? obj)
          (eq? obj '()))
         ((vector? obj)
          (= (vector-length obj) 0))
         ((string? obj)
          (string=? obj ""))
         ((hashtable? obj)
          (= (hashtable-size obj) 0))
         (else
          (raise-unsupported-operation-exception proc: "collection-empty"
                                                 obj: obj))))
                      
                 

(define-generic (collection-enumerator obj)
   (cond ((list? obj)
          (instantiate::%list-enumerator (curr obj)))
         ((vector? obj)
          (instantiate::%vector-enumerator (vec obj)))
         ((string? obj)
          (instantiate::%string-enumerator (str obj)))
         ((hashtable? obj)
          (instantiate::%hashtable-enumerator (hash obj)))
         (else
          (raise-invalid-argument-exception proc: "collection-enumerator"
             args:(list obj)))))

;; return the length of the collection
(define-generic (collection-length obj)
   (cond ((list? obj)
          (length obj))
         ((vector? obj)
          (vector-length obj))
         ((hashtable? obj)
          (hashtable-size obj))
         ((string? obj)
          (string-length obj))
         (else
          (raise-invalid-argument-exception proc: "collection-size"
                                            args: (list obj)))))

(define-inline (vector-contains? vec itm)
    (do ((i 0 (+ i 1)))
        ((or (= i (vector-length vec)) (equal? (vector-ref vec i) itm)) (not (= i (vector-length vec))))))

(define-inline (coll-string-contains? str itm)
   (bind-exit (return)
      (do ((i 0 (+ i 1)))
          ((= i (string-length str) #f)
           (when (equal? (string-ref str i) itm)
              (return #t))))))

(define-generic (collection-contains? obj itm)
   (cond ((list? obj)
          (not (eq? #f (member itm obj))))
         ((hashtable? obj)
          (collection-contains? (hashtable->vector obj) itm))
         ((vector? obj)
          (vector-contains? obj itm))
         ((string? obj)
          (coll-string-contains? obj itm))
         (else (raise-invalid-argument-exception proc: "collection-contains?"
                  args: (list obj)))))


(define-generic (collection-first  obj)
   (if (enumerator? obj)
       (enumerator-current obj)
       (let ((enumer (collection-enumerator obj)))
          (enumerator-move-next! enumer)
          (enumerator-current enumer))))


(define-generic (collection-rest obj)
   (if (enumerator? obj)
       (let ((cln (enumerator-clone obj)))
          (enumerator-move-next! cln)
          cln)
       (let ((enumer (collection-enumerator obj)))
          ;; throw away first item
          (enumerator-move-next! enumer)
          enumer)))
 


(define-generic (collection->list obj)
   (cond ((list? obj)
          obj)
         ((vector? obj)
          (vector->list obj))
         ((hashtable? obj)
          (hashtable->list obj))
         ((string? obj)
          (string->list obj))
         (else (raise-invalid-argument-exception proc: "collection->list"
                  args: (list obj)))))

(define-inline (string->vector str)
   (let ((res (make-vector (string-length str))))
      (do ((i 0 (+ i 1)))
          ((= i (string-length str)) res)
          (vector-set! res i (string-ref str i)))))

(define-generic (collection->vector obj)
   (cond ((list? obj)
          (list->vector obj))
         ((vector? obj)
          obj)
         ((hashtable? obj)
          (hashtable->vector obj))
         ((string? obj)
          (string->vector obj))
         (else (raise-invalid-argument-exception proc: "collection->vector"
                                                 args: (list obj)))))

                             
;;; create a collection of the same type as obj with optional size, fill, and initial values
;;; vals. What vals is dependent on the type of obj.
;;; list and vector expect a simple list of values
;;; string expects a list of chars
;;; hashtable expects an even length list of alternating key and values 
(define-generic (collection-of-same-type obj #!key size (fill #unspecified) #!rest vals)
   (cond ((list? obj)
          (create-list :size size :fill fill vals: vals))
         ((vector? obj)
          (create-vector :size size :fill fill vals: vals))
         ((string? obj)
          (create-string :size size :fill fill vals: vals))
         ((hashtable? obj)
          (create-hash :size size :fill fill vals: vals))
         (else (raise-invalid-argument-exception proc: "collection-of-same-type"
                                                 args: (list obj)))))


(define-inline (create-list #!key size fill vals)
   (if size
       (let ((res (make-list size fill)))
          (when (pair? vals)
             (let loop ((l vals)
                        (r res))
                (when (and (pair? l)
                         (pair? r))
                   (set-car! r (car l))
                   (loop (cdr l)
                      (cdr r)))))
          res)
       (list-copy vals)))

(define-inline (create-vector #!key size fill vals)
   (if size
       (let ((res (make-vector size fill)))
          (when (pair? vals)
             (let loop ((l vals)
                        (i 0))
                (when (and (pair? l)
                           (< i (vector-length res)))
                   (vector-set! res i (car l))
                   (loop (cdr l)
                      (+ i 1)))))
          res)
       (list->vector vals)))

(define-inline (create-string #!key size fill vals)
   (if size
       (let ((res (make-string size (if (eq? fill #unspecified) #\space fill))))
          (when (pair? vals)
             (let loop ((l vals)
                        (i 0))
                (when (and (pair? l)
                           (< i (string-length res)))
                   (string-set! res i (car l))
                   (loop (cdr l)
                      (+ i 1)))))
          res)
       (list->string vals)))

(define (create-hash #!key size fill vals)
   (let ((len (length vals)))
      (if (not (= (modulo len 2) 0))
          (error "list-of-alternating-key-values->hashtable" "invalid list of keys and values" vals)
          (let ((res (create-hashtable)))
             (let loop ((l vals))
                (if (pair? l)
                    (let ((key (car l))
                          (val (cadr l)))
                       (hashtable-put! res key val)
                       (loop (cddr l)))
                    res))))))



; (define-generic (collection-zip obj1 obj2)
;    (if (and (collection? obj1)
;             (collection? obj2))
;        (let ((len (min (collection-length obj1)
;                      (collection-length obj2))))
          
          


; (define-generic (collection-filter obj pred)
;    (cond ((or (list? obj) (vector? obj) (string? obj))
;           (let ((lst (filter pred (collection->list obj))))
;              (apply collection-of-same-type (cons obj lst))))
;          ((hashtable? obj)
;           (let ((keys (hashtable-key
             
   









