(module hoard/dictionary-enumerator
   (import hoard/enumerator
           hoard/exceptions
           hoard/association
           hoard/hashtable-ext)
   (export (class %dictionary-map-enumerator
              enumer
              (curr-kv (default #unspecified))
              proc)
           (class %dictionary-filter-enumerator
              enumer
              pred)
           (class %dictionary-append-enumerator
              enumers)
           (class %alist-enumerator::%list-enumerator)
           (generic dictionary-enumerator-move-next! obj)
           (generic dictionary-enumerator-current obj)
           (generic dictionary-enumerator-key obj)
           (generic dictionary-enumerator-value obj)
           (generic dictionary-enumerator-copy obj)
           (generic dictionary-enumerator? obj)
           (dictionary-enumerator->list enumer)
           (dictionary-enumerator->vector enumer)
           (dictionary-enumerator->hashtable enumer)
           (alist? obj)))

;;; are we looking at a list being used as an alist
(define (alist? obj)
   (and (pair-or-null? obj)
        (every pair? obj)))


;;;; dictionary enumerator protocol

(define-generic (dictionary-enumerator? obj)
   #f)
(define-generic (dictionary-enumerator-copy obj))
(define-generic (dictionary-enumerator-move-next! obj))
(define-generic (dictionary-enumerator-current obj))
(define-generic (dictionary-enumerator-key obj))
(define-generic (dictionary-enumerator-value obj))



;;;; hashtable implementation
(define-method (dictionary-enumerator? enumerator::%hashtable-enumerator)
   #t)

(define-method (dictionary-enumerator-copy enumerator::%hashtable-enumerator)
   (duplicate::%hashtable-enumerator enumerator))

(define-method (dictionary-enumerator-move-next! enumerator::%hashtable-enumerator)
   (enumerator-move-next! enumerator))

(define-method (dictionary-enumerator-current enumerator::%hashtable-enumerator)
   (if (not (-> enumerator started))
       (raise-invalid-state-exception proc: "dictionary-enumerator-current"
          msg: "enumerator-move-next! must be called before dictionary-enumerator-current"
          obj: enumerator)
       (=> (car (-> enumerator curr-key))
          (enumerator-current enumerator))))

(define-method (dictionary-enumerator-key enumerator::%hashtable-enumerator)
   (if (not (-> enumerator started))
       (raise-invalid-state-exception proc: "dictionary-enumerator-key"
          msg: "enumerator-move-next! must be called before dictionary-enumerator-key"
          obj: enumerator)
       (car (-> enumerator curr-key))))

(define-method (dictionary-enumerator-value enumerator::%hashtable-enumerator)
   (enumerator-current enumerator))

;;;; alist implementation

(define-method (dictionary-enumerator? enumerator::%alist-enumerator)
   #t)

(define-method (dictionary-enumerator-copy enumerator::%alist-enumerator)
   (duplicate::%alist-enumerator enumerator))

(define-method (dictionary-enumerator-move-next! enumerator::%alist-enumerator)
    (enumerator-move-next! enumerator))

(define-method (dictionary-enumerator-current enumerator::%alist-enumerator)
   (let ((curr (enumerator-current enumerator)))
      (=> (car curr) (cdr curr))))

(define-method (dictionary-enumerator-key enumerator::%alist-enumerator)
   (let ((curr (enumerator-current enumerator)))
      (car curr)))

(define-method (dictionary-enumerator-value enumerator::%alist-enumerator)
   (cdr (enumerator-current enumerator)))

;;;; vector implementation
(define-method (dictionary-enumerator? enumerator::%vector-enumerator)
   #t)

(define-method (dictionary-enumerator-copy enumerator::%vector-enumerator)
   (duplicate::%vector-enumerator enumerator))

(define-method (dictionary-enumerator-move-next! enumerator::%vector-enumerator)
   (enumerator-move-next! enumerator))

(define-method (dictionary-enumerator-current enumerator::%vector-enumerator)
   (if (not (-> enumerator started))
       (raise-invalid-state-exception proc: "dictionary-enumerator-current"
          msg: "enumerator-move-next! must be called before dictionary-enumerator-current"
          obj: enumerator)
       (=> (-> enumerator curr-index) (enumerator-current enumerator))))

(define-method (dictionary-enumerator-key enumerator::%vector-enumerator)
   (if (not (-> enumerator started))
       (raise-invalid-state-exception proc: "dictionary-enumerator-key"
          msg: "enumerator-move-next! must be called before dictionary-enumerator-key"
          obj: enumerator)
       (-> enumerator curr-index)))

(define-method (dictionary-enumerator-value enumerator::%vector-enumerator)
   (enumerator-current enumerator))


;;;; string implementation
(define-method (dictionary-enumerator? enumerator::%string-enumerator)
   #t)

(define-method (dictionary-enumerator-copy enumerator::%string-enumerator)
   (duplicate::%string-enumerator enumerator))

(define-method (dictionary-enumerator-move-next! enumerator::%string-enumerator)
   (enumerator-move-next! enumerator))

(define-method (dictionary-enumerator-current enumerator::%string-enumerator)
   (if (not (-> enumerator started))
       (raise-invalid-state-exception proc: "dictionary-enumerator-current"
          msg: "enumerator-move-next! must be called before dictionary-enumerator-current"
          obj: enumerator)
       (=> (-> enumerator curr-index) (enumerator-current enumerator))))

(define-method (dictionary-enumerator-key enumerator::%string-enumerator)
   (if (not (-> enumerator started))
       (raise-invalid-state-exception proc: "dictionary-enumerator-key"
          msg: "enumerator-move-next! must be called before dictionary-enumerator-key"
          obj: enumerator)
       (-> enumerator curr-index)))

(define-method (dictionary-enumerator-value enumerator::%string-enumerator)
   (enumerator-current enumerator))

;;;; dictionary-map-enumerator implementation
(define-method (dictionary-enumerator? enumerator::%dictionary-map-enumerator)
   #t)

(define-method (dictionary-enumerator-copy enumerator::%dictionary-map-enumerator)
   (duplicate::%dictionary-map-enumerator enumerator
      (enumer (dictionary-enumerator-copy (-> enumerator enumer)))))

(define-method (dictionary-enumerator-move-next! enumerator::%dictionary-map-enumerator)
   (let ((cont (dictionary-enumerator-move-next! (-> enumerator enumer))))
      (if cont
          (begin
             (set! (-> enumerator curr-kv)
                ((-> enumerator proc) (dictionary-enumerator-key (-> enumerator enumer))
                                      (dictionary-enumerator-value (-> enumerator enumer))))
             #t)
          #f)))

(define-method (dictionary-enumerator-current enumerator::%dictionary-map-enumerator)
   (-> enumerator curr-kv))

(define-method (dictionary-enumerator-key enumerator::%dictionary-map-enumerator)
   (=>key (-> enumerator curr-kv)))

(define-method (dictionary-enumerator-value enumerator::%dictionary-map-enumerator)
   (=>value (-> enumerator curr-kv)))


;;;; dictionary-filter-enumerator implementation
(define-method (dictionary-enumerator? enumerator::%dictionary-filter-enumerator)
   #t)

(define-method (dictionary-enumerator-copy enumerator::%dictionary-filter-enumerator)
   (duplicate::%dictionary-filter-enumerator enumerator
      (enumer (dictionary-enumerator-copy (-> enumerator enumer)))))

(define-method (dictionary-enumerator-move-next! enumerator::%dictionary-filter-enumerator)
   (let loop ((cont (dictionary-enumerator-move-next! (-> enumerator enumer))))
      (cond ((and cont
                  ((-> enumerator pred) (dictionary-enumerator-key (-> enumerator enumer))
                                        (dictionary-enumerator-value (-> enumerator enumer))))
             #t)
            (cont
             (loop (dictionary-enumerator-move-next! (-> enumerator enumer))))
            (else
             #f))))

(define-method (dictionary-enumerator-current enumerator::%dictionary-filter-enumerator)
  (dictionary-enumerator-current (-> enumerator enumer)))

(define-method (dictionary-enumerator-key enumerator::%dictionary-filter-enumerator)
   (dictionary-enumerator-key (-> enumerator enumer)))

(define-method (dictionary-enumerator-value enumerator::%dictionary-filter-enumerator)
   (dictionary-enumerator-value (-> enumerator enumer)))



(define (dictionary-enumerator->list enumer)
   (if (not (dictionary-enumerator? enumer))
       (raise-invalid-argument-exception :proc "dictionary-enumerator->list"
          :args enumer)
       (let loop ((cont (dictionary-enumerator-move-next! enumer))
                  (res '()))
          (if cont
              (let ((val (=> (dictionary-enumerator-key enumer)
                            (dictionary-enumerator-value enumer))))
                 (loop (dictionary-enumerator-move-next! enumer)
                    (cons val res)))
              (reverse! res)))))


(define (dictionary-enumerator->vector enumer)
   (list->vector (dictionary-enumerator->list enumer)))

(define (dictionary-enumerator->hashtable enumer)
   (if (not (dictionary-enumerator? enumer))
       (raise-invalid-argument-exception :proc "dictionary-enumerator->hashtable"
          :args enumer)
       (let ((res (hashtable)))
          (let loop ((cont (dictionary-enumerator-move-next! enumer)))
             (if cont
                 (begin 
                    (hashtable-put! res (dictionary-enumerator-key enumer)
                       (dictionary-enumerator-value enumer))
                    (loop (dictionary-enumerator-move-next! enumer)))
                 res)))))
;;;; dictionary append enumerator
(define-method (dictionary-enumerator? enumerator::%dictionary-append-enumerator)
   #t)

(define-method (dictionary-enumerator-copy enumerator::%dictionary-append-enumerator)
   (duplicate::%dictionary-append-enumerator enumerator
      (enumers (map dictionary-enumerator-copy (-> enumerator enumers)))))

(define-method (dictionary-enumerator-move-next! enumerator::%dictionary-append-enumerator)
   (let loop ()
      (if (pair? (-> enumerator enumers))
          (let ((curr (car (-> enumerator enumers))))
             (if (enumerator-move-next! curr)
                 #t
                 (if (pair? (cdr (-> enumerator enumers)))
                     (begin
                        (set! (-> enumerator enumers) (cdr (-> enumerator enumers)))
                        (loop))
                     #f)))
          #f)))

(define-method (dictionary-enumerator-current enumerator::%dictionary-append-enumerator)
   (let ((curr (car (-> enumerator enumers))))
      (dictionary-enumerator-current curr)))


(define-method (dictionary-enumerator-key enumerator::%dictionary-append-enumerator)
   (let ((curr (car (-> enumerator enumers))))
      (dictionary-enumerator-key curr)))


(define-method (dictionary-enumerator-value enumerator::%dictionary-append-enumerator)
   (let ((curr (car (-> enumerator enumers))))
      (dictionary-enumerator-value curr)))


