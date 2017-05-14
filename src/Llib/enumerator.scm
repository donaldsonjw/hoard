(module hoard/enumerator
   (import hoard/exceptions
           hoard/hashtable-ext)
   (export
      (class %list-enumerator
         (started (default #f))
         curr)
      
      (class %vector-enumerator
         (started (default #f))
         vec
         (curr-index (default 0)))
      
      (class %string-enumerator
         (started (default #f))
         str
         (curr-index (default 0)))

      (class %hashtable-enumerator
         (started (default #f))
         hash
         (curr-key (default '())))
      
      (class %map-enumerator
         enumers
         proc)
      (class %filter-enumerator
         enumer
         pred)

      (class %append-enumerator
         enumers)
      
      (class %aggregate-enumerator
         enums::pair-nil)

      (class %take-enumerator
         enumer
         pred::procedure)
      
      (generic enumerator-move-next! enumerator)
      (generic enumerator-current enumerator)
      (generic enumerator? obj)
      (enumerator->list enumer)
      (generic enumerator-copy enumerator)
      (enumerator->vector enumer)
      (enumerator->string enumer)))


;;;; protocol for enumerating over the elements of a collection
;;;; basically the same protocol as found in .NET
(define-generic (enumerator? obj)
   #f)
(define-generic (enumerator-move-next! enumerator))
(define-generic (enumerator-current enumerator))
(define-generic (enumerator-copy enumerator))


;;;; list-enumerator implementation
(define-method (enumerator? enumerator::%list-enumerator)
   #t)

(define-method (enumerator-copy enumerator::%list-enumerator)
   (duplicate::%list-enumerator enumerator))

(define-method (enumerator-move-next! enumerator::%list-enumerator)
   (cond((eq? #f (-> enumerator started))
         (set! (-> enumerator started) #t)
         (not (null? (-> enumerator curr))))
        ((pair? (-> enumerator curr))
         (set! (-> enumerator curr) (cdr (-> enumerator curr)))
         (not (null? (-> enumerator curr))))
        (else 
         #f)))

(define-method (enumerator-current enumerator::%list-enumerator)
   (if (not (-> enumerator started))
       (raise-invalid-state-exception :proc "enumerator-current"
          :msg "invalid state; enumerator-move-next! must be called before enumerator-current"
          :obj enumerator)
       (car (-> enumerator curr))))


;; vector-enumerator implementation
(define-method (enumerator? enumerator::%vector-enumerator)
   #t)

(define-method (enumerator-copy enumerator::%vector-enumerator)
   (duplicate::%vector-enumerator enumerator))


(define-method (enumerator-move-next! enumerator::%vector-enumerator)
   (cond ((eq? #f (-> enumerator started))
          (set! (-> enumerator started) #t)
          (not (= (vector-length (-> enumerator vec)) 0)))
         ((< (+ (-> enumerator curr-index) 1) (vector-length (-> enumerator vec)))
          (set! (-> enumerator curr-index) (+ (-> enumerator curr-index) 1))
          #t)
         (else
          #f)))

(define-method (enumerator-current enumerator::%vector-enumerator)
   (if (not (-> enumerator started))
       (raise-invalid-state-exception :proc "enumerator-current"
          :msg "invalid state; enumerator-move-next must be called before enumerator-current"
          :obj enumerator)
       (vector-ref (-> enumerator vec) (-> enumerator curr-index))))
   
  

;;; string-enumerator implementation
(define-method (enumerator? enumerator::%string-enumerator)
   #t)

(define-method (enumerator-copy enumerator::%string-enumerator)
   (duplicate::%string-enumerator enumerator))

(define-method (enumerator-move-next! enumerator::%string-enumerator)
   (cond ((eq? #f (-> enumerator started))
          (set! (-> enumerator started) #t)
          (not (= (string-length (-> enumerator str)) 0)))
         ((< (+ (-> enumerator curr-index) 1) (string-length (-> enumerator str)))
          (set! (-> enumerator curr-index) (+ (-> enumerator curr-index) 1))
          #t)
         (else
          #f)))

(define-method (enumerator-current enumerator::%string-enumerator)
   (if (not (-> enumerator started))
       (raise-invalid-state-exception :proc "enumerator-current"
          :msg "invalid state; enumerator-move-next must be called before enumerator-current"
          :obj enumerator)
       (string-ref (-> enumerator str) (-> enumerator curr-index))))


;;;; hashtable-enumerator implementation

(define (hashtable-lazy-key-list hash)
   (let* ((buckets (hashtable-buckets hash))
          (buckets-length (vector-length buckets)))
      (let loop ((i 0))
         (if (= i buckets-length)
             '()
             (let inner-loop ((bucket (vector-ref buckets i)))
                (if (null? bucket)
                    (loop (+fx i 1))
                    (cons (caar bucket) (lambda ()
                                           (inner-loop (cdr bucket))))))))))
      
(define-method (enumerator? enumerator::%hashtable-enumerator)
   #t)

(define-method (enumerator-copy enumerator::%hashtable-enumerator)
   (duplicate::%hashtable-enumerator enumerator))

; (define-method (enumerator-move-next! enumerator::%hashtable-enumerator)
;    (cond ((eq? #f (-> enumerator started))
;           (set! (-> enumerator started) #t)
;           (set! (-> enumerator curr-key) (hashtable-key-list (-> enumerator hash))) 
;           (not (null? (-> enumerator curr-key))))
;          ((pair? (-> enumerator curr-key))
;           (set! (-> enumerator curr-key) (cdr (-> enumerator curr-key)))
;           (not (null? (-> enumerator curr-key))))
;          (else 
;           #f)))

(define-method (enumerator-move-next! enumerator::%hashtable-enumerator)
   (cond ((eq? #f (-> enumerator started))
          (set! (-> enumerator started) #t)
          (set! (-> enumerator curr-key) (hashtable-lazy-key-list (-> enumerator hash))) 
          (not (null? (-> enumerator curr-key))))
         ((pair? (-> enumerator curr-key))
          (set! (-> enumerator curr-key) ((cdr (-> enumerator curr-key))))
          (not (null? (-> enumerator curr-key))))
         (else 
          #f)))

(define-method (enumerator-current enumerator::%hashtable-enumerator)
   (if (not (-> enumerator started))
       (raise-invalid-state-exception :proc "enumerator-current"
          :msg "invalid state; enumerator-move-next must be called before enumerator-current"
          :obj enumerator)
       (hashtable-get (-> enumerator hash) (car (-> enumerator curr-key)))))



;;;; aggregate-enumerator implementation
(define-method (enumerator? enumerator::%aggregate-enumerator)
   #t)

(define-method (enumerator-copy enumerator::%aggregate-enumerator)
   (duplicate::%aggregate-enumerator enumerator
      (enums (map enumerator-copy (-> enumerator enums)))))

(define-method (enumerator-move-next! enumerator::%aggregate-enumerator)
   (every enumerator-move-next! (-> enumerator enums)))

(define-method (enumerator-current enumerator::%aggregate-enumerator)
   (let loop ((lst (-> enumerator enums))
              (res '()))
      (if (pair? lst)
          (loop (cdr lst)
             (cons (enumerator-current (car lst)) res))
          (reverse! res))))


 ;;;; map-enumerator implementation
(define-method (enumerator? enumerator::%map-enumerator)
   #t)

(define-method (enumerator-copy enumerator::%map-enumerator)
   (duplicate::%map-enumerator enumerator
      (enumers (map enumerator-copy (-> enumerator enumers)))))

(define-method (enumerator-move-next! enumerator::%map-enumerator)
   (every enumerator-move-next! (-> enumerator enumers)))


(define-method (enumerator-current enumerator::%map-enumerator)
   (let ((vals (map enumerator-current (-> enumerator enumers))))
      (apply (-> enumerator proc) vals)))


;;;; filter-enumerator implementation
(define-method (enumerator? enumerator::%filter-enumerator)
   #t)

(define-method (enumerator-copy enumerator::%filter-enumerator)
   (duplicate::%filter-enumerator enumerator
      (enumer (enumerator-copy (-> enumerator enumer)))))


(define-method (enumerator-move-next! enumerator::%filter-enumerator)
   (let loop ((cont (enumerator-move-next! (-> enumerator enumer))))
      (cond ((and cont
                  ((-> enumerator pred)
                   (enumerator-current (-> enumerator enumer))))
             #t)
            (cont
             (loop (enumerator-move-next! (-> enumerator enumer))))
            (else
             #f))))
          
(define-method (enumerator-current enumerator::%filter-enumerator)
   (enumerator-current (-> enumerator enumer)))

;;;; take-enumerator

(define-method (enumerator? enumerator::%take-enumerator)
   #t)

(define-method (enumerator-copy enumerator::%take-enumerator)
   (duplicate::%take-enumerator enumerator
      (enumer (enumerator-copy (-> enumerator enumer)))))


(define-method (enumerator-move-next! enumerator::%take-enumerator)
   (and (enumerator-move-next! (-> enumerator enumer))
        ((-> enumerator pred) (enumerator-current (-> enumerator enumer)))))
                  

(define-method (enumerator-current enumerator::%take-enumerator)
   (enumerator-current (-> enumerator enumer)))

;;;; append enumerator
(define-method (enumerator? obj::%append-enumerator)
   #t)

(define-method (enumerator-copy enumerator::%append-enumerator)
   (duplicate::%append-enumerator enumerator
      (enumers (map enumerator-copy (-> enumerator enumers)))))


(define-method (enumerator-move-next! obj::%append-enumerator)
   (let loop ()
      (if (pair? (-> obj enumers))
          (let ((curr (car (-> obj enumers))))
             (if (enumerator-move-next! curr)
                 #t
                 (if (pair? (cdr (-> obj enumers)))
                     (begin
                        (set! (-> obj enumers) (cdr (-> obj enumers)))
                        (loop))
                     #f)))
          #f)))

(define-method (enumerator-current obj::%append-enumerator)
   (let ((curr (car (-> obj enumers))))
      (enumerator-current curr)))

;;;; usefull utility procedures

(define (enumerator->list enumer)
   (if (not (enumerator? enumer))
       (raise-invalid-argument-exception :proc "enumerator->list"
          :args enumer)
       (let loop ((cont (enumerator-move-next! enumer))
                  (res '()))
          (if cont
              (let ((val (enumerator-current enumer)))
                 (loop (enumerator-move-next! enumer)
                    (cons val res)))
              (reverse! res)))))

(define (enumerator->vector enumer)
   (list->vector (enumerator->list enumer)))

(define (enumerator->string enumer)
   (list->string (enumerator->list enumer)))