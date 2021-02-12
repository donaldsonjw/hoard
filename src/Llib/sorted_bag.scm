(module hoard/sorted-bag
   (include "enumerable.sch")
   (include "dictionary_enumerable.sch")
   (import hoard/collection
           hoard/mutable-collection
           hoard/exceptions
           hoard/enumerator
           hoard/dictionary-enumerator
           hoard/enumerable
           hoard/dictionary-enumerable
           hoard/bag
           hoard/comparator
           hoard/association
           hoard/red-black-tree
           hoard/extendable
           hoard/sorted-dictionary)
   (export
      (class %sorted-bag::%sorted-dictionary)
      (class %sorted-bag-enumerator
         (curr (default #unspecified))
         enumer)
      ( sorted-bag? obj)
      ( make-sorted-bag #!key comparator)
      ( sorted-bag #!key comparator #!rest items)
      ( sorted-bag-delete! bag::%sorted-bag item)
      ( sorted-bag-insert! bag::%sorted-bag item)
      (sorted-bag-length bag::%sorted-bag)
      ( sorted-bag-contains? bag::%sorted-bag item)
      ( sorted-bag-empty? bag::%sorted-bag)
      ( sorted-bag-count bag::%sorted-bag item)
      ( sorted-bag-count-set! bag::%sorted-bag item count)
      (make-sorted-bag-enumerator bag::%sorted-bag)
      ( sorted-bag-copy bag::%sorted-bag)))


(define (sorted-bag? obj)
   (isa? obj %sorted-bag))

(define (make-sorted-bag #!key comparator)
   (when (not (comparator? comparator))
      (raise-invalid-argument-exception :proc "make-sorted-bag"
         :args comparator
         :msg "make-sorted-bag must be passed a valid comparator"))
   (instantiate::%sorted-bag (comparator (make-association-comparator comparator))))


(define (sorted-bag #!key comparator #!rest items)
   (when (not (comparator? comparator))
      (raise-invalid-argument-exception :proc "sorted-bag"
         :args comparator
         :msg "make-sorted-bag must be passed a valid comparator"))
   (let ((bag (make-sorted-bag :comparator comparator)))
      (for-each (lambda (x) (sorted-bag-insert! bag x)) items)
      bag))

(define (sorted-bag-copy bag::%sorted-bag)
   (duplicate::%sorted-bag bag (root (red-black-node-copy (-> bag root)))))

(define (sorted-bag-insert! bag::%sorted-bag item)
   (sorted-dictionary-update! bag item 1 (lambda (x) (+ x 1))))


(define (sorted-bag-delete! bag::%sorted-bag item)
   (let ((res (sorted-dictionary-get bag item)))
      (if (and res (> res 1))
          (sorted-dictionary-update! bag item 0 (lambda (x) (- x 1)))
          (sorted-dictionary-remove! bag item))))

(define (sorted-bag-length bag::%sorted-bag)
   (enumerable-fold (lambda (s x) (+ s 1)) 0 bag))
   
(define (sorted-bag-contains? bag::%sorted-bag item)
   (sorted-dictionary-contains? bag item))

(define (sorted-bag-empty? bag::%sorted-bag)
   (sorted-dictionary-empty? bag))



(define (sorted-bag-count bag::%sorted-bag item)
   (let ((res (sorted-dictionary-get bag item)))
      (if res res 0)))

(define (sorted-bag-count-set! bag::%sorted-bag item count)
   (if (<= count 0)
       (sorted-dictionary-remove! bag item)
       (sorted-dictionary-put! bag item count)))


;;;; implementation of bag protocol
(define-method (bag? obj::%sorted-bag)
   #t)

(define-method (bag-insert! bag::%sorted-bag item)
   (sorted-bag-insert! bag item))

(define-method (bag-delete! bag::%sorted-bag item)
   (sorted-bag-delete! bag item))

(define-method (bag-contains? bag::%sorted-bag item)
   (sorted-bag-contains? bag item))

(define-method (bag-count bag::%sorted-bag item)
   (sorted-bag-count bag item))

(define-method (bag-count-set! bag::%sorted-bag item count)
   (sorted-bag-count-set! bag item count))

(define-method (bag-length bag::%sorted-bag)
   (sorted-bag-length bag))

(define-method (bag-empty? bag::%sorted-bag)
   (sorted-bag-empty? bag))

(define-method (bag-copy bag::%sorted-bag)
   (sorted-bag-copy bag))

;;; collection implementation
(define-method (collection? bag::%sorted-bag)
   #t)

(define-method (collection-length bag::%sorted-bag)
   (sorted-bag-length bag))

(define-method (collection-enumerator bag::%sorted-bag)
   (make-sorted-bag-enumerator bag))

(define-method (collection-contains? bag::%sorted-bag itm)
   (sorted-bag-contains? bag itm))

(define-method (collection-empty? bag::%sorted-bag)
   (sorted-bag-empty? bag))

(define-method (collection-copy bag::%sorted-bag)
   (sorted-bag-copy bag))

;;;; sorted-bag-enumerator
(define (make-sorted-bag-enumerator bag::%sorted-bag)
   (instantiate::%sorted-bag-enumerator (enumer (make-sorted-dictionary-enumerator bag))))

(define-method (enumerator? obj::%sorted-bag-enumerator)
   #t)

(define-method (enumerator-current enumer::%sorted-bag-enumerator)
   (if (eq? (-> enumer curr) #unspecified)
       (raise-invalid-state-exception :proc "enumerator-current"
          :msg "invalid state: enumerator-move-next! must be called before enumerator-current"
          :obj enumer)
       (=>key (-> enumer curr))))

(define-method (enumerator-copy enumer::%sorted-bag-enumerator)
   (instantiate::%sorted-bag-enumerator (curr (if (eq? (-> enumer curr)
                                                   #unspecified)
                                                (-> enumer curr)
                                                (=> (=>key (-> enumer curr))
                                                   (=>value (-> enumer curr)))))
                                      (enumer (dictionary-enumerator-copy (-> enumer enumer)))))

(define-method (enumerator-move-next! enumer::%sorted-bag-enumerator)
   (if (or (eq? (-> enumer curr) #unspecified)
           (and (association? (-> enumer curr))
                (= (=>value (-> enumer curr)) 1)))
       (let ((res (dictionary-enumerator-move-next! (-> enumer enumer))))
          (if res
              (begin 
                 (set! (-> enumer curr) (=> (dictionary-enumerator-key (-> enumer enumer))
                                           (dictionary-enumerator-value (-> enumer enumer))))
                 res)
              res))
       (begin
          (=>value! (-> enumer curr) (- (=>value (-> enumer curr)) 1))
          #t)))


;;; enumerable implementation
(define-method (enumerable? bag::%sorted-bag)
   #t)

(define-method (enumerable-enumerator bag::%sorted-bag)
  (make-sorted-bag-enumerator bag))



;;; sorted-dictionary is mutable
(define-method (collection-mutable? bag::%sorted-bag)
   #t)


;;; sorted dictionary is extendable
(define-method (collection-extendable? bag::%sorted-bag)
   #t)

(define-method (collection-extend! bag::%sorted-bag item)
  (sorted-bag-insert! bag item))

