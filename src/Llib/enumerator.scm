(module hoard/enumerator
   (export
      (class list-enumerator
         (started (default #f))
         curr)
      
      (class vector-enumerator
         (started (default #f))
         vec
         (curr-index (default 0)))
      
      (class string-enumerator
         (started (default #f))
         str
         (curr-index (default 0)))

      (class hashtable-enumerator
         (started (default #f))
         hash
         (curr-key (default '())))
      
      (class map-enumerator
         enumers
         proc)
      (class filter-enumerator
         enumer
         pred)
      
      (class aggregate-enumerator
         enums::pair-nil)

      (class take-enumerator
         enumer
         pred::procedure)
      
      

      (generic enumerator-move-next! enumerator)
      (generic enumerator-current enumerator)
      (generic enumerator? obj)
      (enumerator->list enumer)))


;;;; protocol for enumerating over the elements of a collection
;;;; basically the same protocol as found in .NET
(define-generic (enumerator? obj)
   #f)
(define-generic (enumerator-move-next! enumerator))
(define-generic (enumerator-current enumerator))


;;;; list-enumerator implementation
(define-method (enumerator? enumerator::list-enumerator)
   #t)

(define-method (enumerator-move-next! enumerator::list-enumerator)
   (cond ((eq? #f (-> enumerator started))
          (set! (-> enumerator started) #t)
          (not (null? (-> enumerator curr))))
         ((pair? (-> enumerator curr))
          (set! (-> enumerator curr) (cdr (-> enumerator curr)))
          (not (null? (-> enumerator curr))))
         (else 
          #f)))

(define-method (enumerator-current enumerator::list-enumerator)
   (if (not (-> enumerator started))
       (error "enumerator-current" "invalid state; enumerator-move-next must be called before enumerator-current"
          enumerator)
       (car (-> enumerator curr))))


;;; vector-enumerator implementation

(define-method (enumerator? enumerator::vector-enumerator)
   #t)

(define-method (enumerator-move-next! enumerator::vector-enumerator)
   (cond ((eq? #f (-> enumerator started))
          (set! (-> enumerator started) #t)
          (not (= (vector-length (-> enumerator vec)) 0)))
         ((< (+ (-> enumerator curr-index) 1) (vector-length (-> enumerator vec)))
          (set! (-> enumerator curr-index) (+ (-> enumerator curr-index) 1))
          #t)
         (else
          #f)))

(define-method (enumerator-current enumerator::vector-enumerator)
   (if (not (-> enumerator started))
       (error "enumerator-current" "invalid state; enumerator-move-next must be called before enumerator-current"
          enumerator)
       (vector-ref (-> enumerator vec) (-> enumerator curr-index))))
   
  

;;; string-enumerator implementation
(define-method (enumerator? enumerator::string-enumerator)
   #t)

(define-method (enumerator-move-next! enumerator::string-enumerator)
   (cond ((eq? #f (-> enumerator started))
          (set! (-> enumerator started) #t)
          (not (= (string-length (-> enumerator str)) 0)))
         ((< (+ (-> enumerator curr-index) 1) (string-length (-> enumerator str)))
          (set! (-> enumerator curr-index) (+ (-> enumerator curr-index) 1))
          #t)
         (else
          #f)))

(define-method (enumerator-current enumerator::string-enumerator)
   (if (not (-> enumerator started))
       (error "enumerator-current" "invalid state; enumerator-move-next must be called before enumerator-current"
          enumerator)
       (string-ref (-> enumerator str) (-> enumerator curr-index))))


;;;; hashtable-enumerator implementation
(define-method (enumerator? enumerator::hashtable-enumerator)
   #t)

(define-method (enumerator-move-next! enumerator::hashtable-enumerator)
   (cond ((eq? #f (-> enumerator started))
          (set! (-> enumerator started) #t)
          (set! (-> enumerator curr-key) (hashtable-key-list (-> enumerator hash))) 
          (not (null? (-> enumerator curr-key))))
         ((pair? (-> enumerator curr-key))
          (set! (-> enumerator curr-key) (cdr (-> enumerator curr-key)))
          (not (null? (-> enumerator curr-key))))
         (else 
          #f)))

(define-method (enumerator-current enumerator::hashtable-enumerator)
   (if (not (-> enumerator started))
       (error "enumerator-current" "invalid state; enumerator-move-next must be called before enumerator-current"
          enumerator)
       (hashtable-get (-> enumerator hash) (car (-> enumerator curr-key)))))




;;;; aggregate-enumerator implementation
(define-method (enumerator? enumerator::aggregate-enumerator)
   #t)

(define-method (enumerator-move-next! enumerator::aggregate-enumerator)
   (every enumerator-move-next! (-> enumerator enums)))

(define-method (enumerator-current enumerator::aggregate-enumerator)
   (let loop ((lst (-> enumerator enums))
              (res '()))
      (if (pair? lst)
          (loop (cdr lst)
             (cons (enumerator-current (car lst)) res))
          (reverse! res))))


 ;;;; map-enumerator implementation
(define-method (enumerator? enumerator::map-enumerator)
   #t)

(define-method (enumerator-move-next! enumerator::map-enumerator)
   (every enumerator-move-next! (-> enumerator enumers)))


(define-method (enumerator-current enumerator::map-enumerator)
   (let ((vals (map enumerator-current (-> enumerator enumers))))
      (apply (-> enumerator proc) vals)))


;;;; filter-enumerator implementation
(define-method (enumerator? enumerator::filter-enumerator)
   #t)

(define-method (enumerator-move-next! enumerator::filter-enumerator)
   (let loop ((cont (enumerator-move-next! (-> enumerator enumer))))
      (cond ((and cont
                  ((-> enumerator pred)
                   (enumerator-current (-> enumerator enumer))))
             #t)
            (cont
             (loop (enumerator-move-next! (-> enumerator enumer))))
            (else
             #f))))
          
(define-method (enumerator-current enumerator::filter-enumerator)
   (enumerator-current (-> enumerator enumer)))

;;;; take-enumerator

(define-method (enumerator? enumerator::take-enumerator)
   #t)

(define-method (enumerator-move-next! enumerator::take-enumerator)
   (and (enumerator-move-next! (-> enumerator enumer))
        ((-> enumerator pred) (enumerator-current (-> enumerator enumer)))))
                  

(define-method (enumerator-current enumerator::take-enumerator)
   (enumerator-current (-> enumerator enumer)))


;;;; usefull utility procedures

(define (enumerator->list enumer)
   (if (not (enumerator? enumer))
       (error "enumerator->list" "argument not an enumerator" enumer)
       (let loop ((cont (enumerator-move-next! enumer))
                  (res '()))
          (if cont
              (let ((val (enumerator-current enumer)))
                 (loop (enumerator-move-next! enumer)
                    (cons val res)))
              (reverse! res)))))