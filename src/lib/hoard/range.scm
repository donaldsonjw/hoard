(module hoard/range
   (import hoard/enumerable
           hoard/enumerator
           hoard/exceptions)
   (export
      (class %range
         start
         end
         step)
      (class %range-enumerator
         (curr (default #unspecified))
         range::%range)
      (range-for-each proc::procedure range::%range)
      (range #!key (start 0) end (step 1))
      (range-map proc::procedure range::%range)
      (valid-range-args? start end step)
      (range? obj)))



(define (valid-range-args? start end step)
   (and (integer? start)
        (integer? end)
        (integer? step)
        (not (= step 0))
        (or (and (positive? step)
                 (<= start end))
            (and (negative? step)
                 (>= start end)))))



(define (range #!key (start 0) end (step 1))
   (if (valid-range-args? start end step)
       (instantiate::%range (start start)
                            (end end)
                            (step step))
       (raise-invalid-argument-exception proc: "range"
          args: (list start end step))))

(define (range? obj)
   (isa? obj %range))

;;;; enumerator protocol

(define-method (enumerator? obj::%range-enumerator)
   #t)

(define-method (enumerator-copy obj::%range-enumerator)
   (duplicate::%range-enumerator obj))

(define-method (enumerator-move-next! obj::%range-enumerator)
   (cond ((eq? (-> obj curr) #unspecified)
          (if (not (= (-> obj range start) (->  obj range end)))
              (begin
                 (set! (-> obj curr) (-> obj range start))
                 #t)
              #f))
         ((or (and (positive? (-> obj range step))
                        (< (+ (-> obj curr) (-> obj range step)) (-> obj range end)))
              (and (negative? (-> obj range step))
                   (> (+ (-> obj curr) (-> obj range step)) (-> obj range end))))
          (set! (-> obj curr) (+ (-> obj curr) (-> obj range step)))
          #t)
         (else
          #f)))

(define-method (enumerator-current obj::%range-enumerator)
   (if (eq? (-> obj curr) #unspecified)
       (raise-invalid-state-exception proc: "enumerator-current"
          msg: "invalid state; enumerator-move-next must be called before enumerator-current"
          obj: obj)
       (-> obj curr)))
          
;;;; enumerable protocol for range
(define-method (enumerable? obj::%range)
   #t)

(define-method (enumerable-enumerator obj::%range)
   (instantiate::%range-enumerator (range obj)))



(define (range-for-each proc::procedure range::%range)
   (do ((i (-> range start) (+ i (-> range step))))
       ((if (positive? (-> range step))
            (>= i (-> range end))
            (<= i (-> range end))))
       (proc i)))


(define (range-map proc::procedure range::%range)
   (do ((i (-> range start) (+ i (-> range step)))
        (res '() (cons (proc i) res)))
       ((if (positive? (-> range step))
            (>= i (-> range end))
            (<= i (-> range end)))
        (reverse! res))))