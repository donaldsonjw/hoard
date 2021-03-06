(module hoard/stretchy-vector
   (import  hoard/exceptions
            hoard/enumerator
            hoard/dictionary-enumerator
            hoard/enumerable
            hoard/dictionary-enumerable
            hoard/indexable
            hoard/collection
            hoard/extendable
            hoard/mutable-collection)
   
   (export
      (class %stretchy-vector
         vec
         length)
      (class %stretchy-vector-enumerator
         (started (default #f))
         svec
         (curr-index (default 0)))  
      +minimum-stretchy-vector-capacity+
      (stretchy-vector-length svec::%stretchy-vector)
      (stretchy-vector-expand! svec::%stretchy-vector at-least-len)
      (stretchy-vector-resize! svec::%stretchy-vector new-len)
      (stretchy-vector-capacity svec::%stretchy-vector)
      (stretchy-vector-set! svec::%stretchy-vector index val)
      (stretchy-vector-ref svec::%stretchy-vector index)
      (make-stretchy-vector len #!optional (fill #unspecified) #!key (capacity len))
      (stretchy-vector? obj)
      (stretchy-vector #!rest elems)
      (list->stretchy-vector lst)
      (vector->stretchy-vector vec)
      (stretchy-vector->vector svec::%stretchy-vector)
      (stretchy-vector->list svec::%stretchy-vector)
      (stretchy-vector-map! proc::procedure svec::%stretchy-vector)
      (stretchy-vector-map proc::procedure svec::%stretchy-vector)
      (stretchy-vector-copy svec::%stretchy-vector #!key (start 0)  (length #unspecified))
      (stretchy-vector-extend! svec::%stretchy-vector item)
      (stretchy-vector-remove! svec::%stretchy-vector)
      (stretchy-vector-append! sv1::%stretchy-vector sv2::%stretchy-vector)
      (stretchy-vector-append sv1::%stretchy-vector sv2::%stretchy-vector)))

(define +minimum-stretchy-vector-capacity+ 16)

;; create a stretchy-vector with an initial size. If fill is
;; provided all of the elements are defaulted to this value
(define (make-stretchy-vector len #!optional (fill #unspecified) #!key (capacity len))
   (let ((cap (if (< capacity +minimum-stretchy-vector-capacity+)
                  +minimum-stretchy-vector-capacity+
                  capacity)))
      (instantiate::%stretchy-vector (vec (if (eq? fill #unspecified)
                                              (make-vector cap) 
                                              (make-vector cap fill)))
                                     (length len))))

(define (stretchy-vector #!rest elems)
   (instantiate::%stretchy-vector (vec (if (> (length elems) 0)
                                           (list->vector elems)
                                           (make-vector  +minimum-stretchy-vector-capacity+ #unspecified)))
                                  (length (length elems))))


(define-method (object-equal? a::%stretchy-vector b)
   (and (isa? b %stretchy-vector)
        (= (stretchy-vector-length a)
           (stretchy-vector-length b))
        (let ((bv::%stretchy-vector b))
           (do ((i 0 (+ i 1))
                (res (equal? (vector-ref (-> a vec) 0)
                        (vector-ref (-> bv vec) 0))
                   (equal? (vector-ref (-> a vec) i)
                      (vector-ref (-> bv vec) i))))
               ((or (not res)
                    (= i (stretchy-vector-length a))) res)))))


;; stretchy-vector predicate
(define (stretchy-vector? obj)
   (isa? obj %stretchy-vector))


;; reference the element located at index
;; if index is beyond the current size an invalid index exception is thrown.
(define (stretchy-vector-ref svec::%stretchy-vector index)
   (if (or (>= index (-> svec length))
           (< index 0))
       (raise-invalid-index-exception proc: "stretchy-vector-ref"
          index: index)
       (vector-ref (-> svec vec) index)))

     
;; set the element located at index to the specified value val
;; if the index is larger than the current capacity resize the
;; stretch-vector
(define (stretchy-vector-set! svec::%stretchy-vector index val)
   (let ((len (stretchy-vector-length svec)))
      (when (>= index (stretchy-vector-capacity svec))
         (stretchy-vector-expand! svec index))
      (when (>= index len)
         (set! (-> svec length) (+ index 1)))
   (vector-set! (-> svec vec) index val)))

;; return the current capicity of a stretchy-vector
(define (stretchy-vector-capacity svec::%stretchy-vector)
   (vector-length (-> svec vec)))
          
   
;; resize the stretchy-vector to a new-len
;; If the new-len if larger than the current length, all of the
;; elements greater than the current size are set to unspecified.
;; If new-len is smaller than the current size, all of the elements greater than
;; the current size are dropped.
(define (stretchy-vector-resize! svec::%stretchy-vector new-len)
   (let ((new-vec (copy-vector (-> svec vec) new-len)))
      (set! (-> svec vec) new-vec)
      (set! (-> svec length) new-len))
   svec)


;; double the current capacity of the vector until it is larger
;; than at-least-len
(define (stretchy-vector-expand! svec::%stretchy-vector at-least-len)
   (let loop ((len (stretchy-vector-capacity svec)))
      (if (<= len at-least-len)
          (loop (* 2 len))
          (stretchy-vector-resize! svec len))))


;; return the current length of the stretchy-vector
(define (stretchy-vector-length svec::%stretchy-vector)
   (-> svec length))

;; convert a list to a stretchy-vector
(define (list->stretchy-vector lst)
   (instantiate::%stretchy-vector (vec (list->vector lst))
                                  (length (length lst))))


;; convert a vector to a stretchy-vector
(define (vector->stretchy-vector vec)
   (let ((vec-len (vector-length vec)))
      (instantiate::%stretchy-vector (vec (vector-copy vec 0 vec-len))
                                     (length vec-len))))

;; convert a stretchy-vector to list
(define (stretchy-vector->list svec::%stretchy-vector)
   (do ((i 0 (+ i 1))
        (res '() (cons (vector-ref (-> svec vec) i)
                    res)))
       ((= i (-> svec length)) (reverse! res))))

;; convert a stretchy-vector to a vector
(define (stretchy-vector->vector svec::%stretchy-vector)
   (vector-copy (-> svec vec) 0 (-> svec length)))


(define (stretchy-vector-copy svec::%stretchy-vector #!key (start 0)  (length #unspecified))
   (let ((len (if (not (eq? length #unspecified)) length (- (stretchy-vector-length svec) start))))
      (if (or (< start 0)
              (> (+ start len) (stretchy-vector-length svec)))
          (raise-invalid-argument-exception :proc "stretchy-vector-copy" args: (list svec start length))
          (instantiate::%stretchy-vector (vec (vector-copy (-> svec vec) start (+ start len)))
                                         (length len)))))

;; map a function over a stretchy-vector and modify in place
(define (stretchy-vector-map! proc::procedure svec::%stretchy-vector)
   (do ((i 0 (+ i 1)))
       ((= i (stretchy-vector-length svec)) svec)
       (stretchy-vector-set! svec i (proc (stretchy-vector-ref svec i)))))


(define (stretchy-vector-map proc::procedure svec::%stretchy-vector)
   (let ((new-svec (stretchy-vector-copy svec)))
      (stretchy-vector-map! proc new-svec)))

(define (stretchy-vector-append! sv1::%stretchy-vector sv2::%stretchy-vector)
   (do ((i 0 (+ i 1)))
       ((= i (stretchy-vector-length sv2)) sv1)
       (stretchy-vector-extend! sv1 (stretchy-vector-ref sv2 i))))

(define (stretchy-vector-append sv1::%stretchy-vector sv2::%stretchy-vector)
   (let ((new-svec (stretchy-vector-copy sv1)))
      (stretchy-vector-append! new-svec sv2)))


(define (stretchy-vector-extend! svec::%stretchy-vector item)
   (stretchy-vector-set! svec (stretchy-vector-length svec) item))


(define (stretchy-vector-remove! svec::%stretchy-vector)
   (let ((res (stretchy-vector-ref svec (- (stretchy-vector-length svec) 1))))
      (set! (-> svec length) (- (-> svec length) 1))
      ;; reduce the size of our backing vector by half when the current length is a quarter
      ;; of the capacity.
      (when (<= (-> svec length) (/ (stretchy-vector-capacity svec) 4))
         (stretchy-vector-resize! svec (/ (stretchy-vector-capacity svec) 2))) 
      res))


;;; implementation of collection api
(define-method (collection? obj::%stretchy-vector)
   #t)

(define-method (collection-length obj::%stretchy-vector)
   (stretchy-vector-length obj))

(define-method (collection-contains? obj::%stretchy-vector itm)
   (bind-exit (return)
      (do ((i 0 (+ i 1)))
          ((= i (stretchy-vector-length obj)) #f)
          (if (equal? (stretchy-vector-ref obj i) itm)
              (return #t)))))

(define-method (collection-empty? obj::%stretchy-vector)
   (= (stretchy-vector-length obj) 0))


(define-method (collection-enumerator obj::%stretchy-vector)
   (enumerable-enumerator obj))

;;; mutable protocol
(define-method (collection-mutable? obj::%stretchy-vector)
   #t)

;;; indexable protocol
(define-method (collection-indexable? obj::%stretchy-vector)
   #t)

(define-method (collection-ref obj::%stretchy-vector index #!optional (default +collection-unspecified+))
   (if (and (>= index 0)
            (< index (stretchy-vector-length obj)))
       (stretchy-vector-ref obj index)
       default))

(define-method (collection-set! obj::%stretchy-vector index val)
   (stretchy-vector-set! obj index val))


;;; extendable protocol
(define-method (collection-extendable? obj::%stretchy-vector)
   #t)

(define-method (collection-extend! obj::%stretchy-vector itm)
   (stretchy-vector-extend! obj itm))

;;; stretchy-vector enumerator
(define-method (enumerator? enumerator::%stretchy-vector-enumerator)
   #t)

(define-method (enumerator-copy enumerator::%stretchy-vector-enumerator)
   (duplicate::%stretchy-vector-enumerator enumerator))

(define-method (enumerator-move-next! enumerator::%stretchy-vector-enumerator)
   (cond ((eq? #f (-> enumerator started))
          (set! (-> enumerator started) #t)
          (not (= (stretchy-vector-length (-> enumerator svec)) 0)))
         ((< (+ (-> enumerator curr-index) 1) (stretchy-vector-length (-> enumerator svec)))
          (set! (-> enumerator curr-index) (+ (-> enumerator curr-index) 1))
          #t)
         (else
          #f)))

(define-method (enumerator-current enumerator::%stretchy-vector-enumerator)
   (if (not (-> enumerator started))
       (raise-invalid-state-exception :proc "enumerator-current"
          :msg "invalid state; enumerator-move-next must be called before enumerator-current"
          :obj enumerator)
       (stretchy-vector-ref (-> enumerator svec) (-> enumerator curr-index))))


;;;; stretchy-vector enumerable
(define-method (enumerable? obj::%stretchy-vector)
   #t)

(define-method (enumerable-enumerator obj::%stretchy-vector)
   (instantiate::%stretchy-vector-enumerator (svec obj)))

;;;; stretchy-vector dictionary enumerator  protocol
(define-method (dictionary-enumerator? enumerator::%stretchy-vector-enumerator)
   #t)

(define-method (dictionary-enumerator-copy enumerator::%stretchy-vector-enumerator)
   (duplicate::%stretchy-vector-enumerator enumerator))

(define-method (dictionary-enumerator-move-next! enumerator::%stretchy-vector-enumerator)
   (cond ((eq? #f (-> enumerator started))
          (set! (-> enumerator started) #t)
          (not (= (stretchy-vector-length (-> enumerator svec)) 0)))
         ((< (+ (-> enumerator curr-index) 1) (stretchy-vector-length (-> enumerator svec)))
          (set! (-> enumerator curr-index) (+ (-> enumerator curr-index) 1))
          #t)
         (else
          #f)))

(define-method (dictionary-enumerator-current enumerator::%stretchy-vector-enumerator)
   (if (not (-> enumerator started))
       (raise-invalid-state-exception :proc "dictionary-enumerator-current"
          :msg "invalid state; dictionary-enumerator-move-next must be called before enumerator-current"
          :obj enumerator)
       (stretchy-vector-ref (-> enumerator svec) (-> enumerator curr-index))))

(define-method (dictionary-enumerator-value enumerator::%stretchy-vector-enumerator)
   (if (not (-> enumerator started))
       (raise-invalid-state-exception :proc "dictionary-enumerator-value"
          :msg "invalid state; dictionary-enumerator-move-next must be called before enumerator-current"
          :obj enumerator)
       (stretchy-vector-ref (-> enumerator svec) (-> enumerator curr-index))))

(define-method (dictionary-enumerator-key enumerator::%stretchy-vector-enumerator)
   (if (not (-> enumerator started))
       (raise-invalid-state-exception :proc "dictionary-enumerator-key"
          :msg "invalid state; dictionary-enumerator-move-next must be called before enumerator-current"
          :obj enumerator)
       (-> enumerator curr-index)))


;;;; stretchy-vector dictionary-enumerable
(define-method (dictionary-enumerable? obj::%stretchy-vector)
   #t)

(define-method (dictionary-enumerable-enumerator obj::%stretchy-vector)
   (instantiate::%stretchy-vector-enumerator (svec obj)))