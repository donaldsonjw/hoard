(module hoard/hash-bag
   (include "enumerable.sch")
   (include "dictionary-enumerable.sch")
   (import hoard/collection
           hoard/exceptions
           hoard/bag
           hoard/hashtable-ext
           hoard/association
           hoard/enumerator
           hoard/dictionary-enumerator
           hoard/enumerable
           hoard/extendable
           hoard/dictionary-enumerable
           hoard/mutable-collection)
   (export
      (class %hash-bag
         hash)
      (class %hash-bag-enumerator
         (curr (default #unspecified))
         enumer)
      (inline make-hash-bag #!key (comparator #f))
      (inline hash-bag #!key (comparator #f) #!rest items)
      (inline hash-bag-insert! hash-bag::%hash-bag item)
      (inline hash-bag-delete! hash-bag::%hash-bag item)
      (inline hash-bag-contains? hash-bag::%hash-bag item)
      (inline hash-bag-count hash-bag::%hash-bag item)
      (inline hash-bag-count-set! hash-bag::%hash-bag item count)
      (inline hash-bag? obj)
      (hash-bag-length hash-bag::%hash-bag)
      (inline hash-bag-empty? hash-bag::%hash-bag)
      (make-hash-bag-enumerator bag::%hash-bag)
      (inline hash-bag-copy hash-bag::%hash-bag)))

(define-inline (hash-bag? obj)
   (isa? obj %hash-bag))

(define-inline (make-hash-bag #!key (comparator #f))
   (instantiate::%hash-bag
      (hash (hashtable :comparator comparator))))


(define-inline (hash-bag #!key (comparator #f) #!rest items)
   (let ((bag (make-hash-bag :comparator comparator)))
      (for-each (lambda (item)
                   (hash-bag-insert! bag item))
         items)
      bag))

(define-inline (hash-bag-insert! hash-bag::%hash-bag item)
   (hashtable-update! (-> hash-bag hash) item (lambda (x) (+ x 1)) 1))


(define-inline (hash-bag-delete! hash-bag::%hash-bag item)
   (let ((res (hashtable-get (-> hash-bag hash) item)))
      (if (and res (> res 1))
          (hashtable-update! (-> hash-bag hash) item (lambda (x) (- x 1)) 0)
          (hashtable-remove! (-> hash-bag hash) item))))

(define (hash-bag-length hash-bag::%hash-bag)
   (enumerable-fold + 0 (-> hash-bag hash)))

(define-inline (hash-bag-contains? hash-bag::%hash-bag item)
   (hashtable-contains? (-> hash-bag hash) item))


(define-inline (hash-bag-count hash-bag::%hash-bag item)
   (let ((res (hashtable-get (-> hash-bag hash) item)))
      (if res res 0)))


(define-inline (hash-bag-count-set! hash-bag::%hash-bag item count)
   (if (= count 0)
       (hashtable-remove! (-> hash-bag hash) item)
       (hashtable-put! (-> hash-bag hash) item count)))

(define-inline (hash-bag-empty? hash-bag::%hash-bag)
   (= (hashtable-size (-> hash-bag hash)) 0))

(define-inline (hash-bag-copy hash-bag::%hash-bag)
   (duplicate::%hash-bag hash-bag (hash (hashtable-copy (-> hash-bag hash)))))

;;;; implementation of bag protocol
(define-method (bag? obj::%hash-bag)
   #t)

(define-method (bag-insert! bag::%hash-bag item)
   (hash-bag-insert! bag item))

(define-method (bag-delete! bag::%hash-bag item)
   (hash-bag-delete! bag item))

(define-method (bag-contains? bag::%hash-bag item)
   (hash-bag-contains? bag item))

(define-method (bag-count bag::%hash-bag item)
   (hash-bag-count bag item))

(define-method (bag-count-set! bag::%hash-bag item count)
   (hash-bag-count-set! bag item count))

(define-method (bag-length bag::%hash-bag)
   (hash-bag-length bag))

(define-method (bag-empty? bag::%hash-bag)
   (hash-bag-empty? bag))

(define-method (bag-copy bag::%hash-bag)
   (hash-bag-copy bag))

;;;; hash-bag-enumerator
(define (make-hash-bag-enumerator bag::%hash-bag)
   (instantiate::%hash-bag-enumerator (enumer (get-enumer (-> bag hash)))))

(define-method (enumerator? obj::%hash-bag-enumerator)
   #t)

(define-method (enumerator-current enumer::%hash-bag-enumerator)
   (if (eq? (-> enumer curr) #unspecified)
       (raise-invalid-state-exception :proc "enumerator-current"
          :msg "invalid state: enumerator-move-next! must be called before enumerator-current"
          :obj enumer)
       (=>key (-> enumer curr))))

(define-method (enumerator-copy enumer::%hash-bag-enumerator)
   (instantiate::%hash-bag-enumerator (curr (if (eq? (-> enumer curr)
                                                   #unspecified)
                                                (-> enumer curr)
                                                (=> (=>key (-> enumer curr))
                                                   (=>value (-> enumer curr)))))
                                      (enumer (dictionary-enumerator-copy (-> enumer enumer)))))

(define-method (enumerator-move-next! enumer::%hash-bag-enumerator)
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

;;;; enumerable protocol implementation
(define-method (enumerable? obj::%hash-bag)
   #t)

(define-method (enumerable-enumerator obj::%hash-bag)
   (make-hash-bag-enumerator obj))

;;;; collection protocol implementation
(define-method (collection? obj::%hash-bag)
   #t)

(define-method (collection-length obj::%hash-bag)
   (hash-bag-length obj))

(define-method (collection-enumerator obj::%hash-bag)
   (make-hash-bag-enumerator obj))

(define-method (collection-contains? obj::%hash-bag item)
   (hash-bag-contains? obj item))

(define-method (collection-empty? obj::%hash-bag)
   (hash-bag-empty? obj))

(define-method (collection-copy obj::%hash-bag)
   (hash-bag-copy obj))

;;;; mutable-collection
(define-method (collection-mutable? obj::%hash-bag)
   #t)

;;;; extendable protocol
(define-method (collection-extendable? obj::%hash-bag)
   #t)

(define-method (collection-extend! obj::%hash-bag item)
   (hash-bag-insert! obj item))