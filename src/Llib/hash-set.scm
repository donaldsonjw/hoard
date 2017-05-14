(module hoard/hash-set
   (import hoard/hashtable-ext
           hoard/exceptions
           hoard/collection
           hoard/enumerable
           hoard/enumerator
           hoard/dictionary-enumerator
           hoard/dictionary-enumerable
           hoard/mutable-collection
           hoard/extendable
           hoard/association
           hoard/set)
   (export
      (class %hash-set
         hash)
      (class %hash-set-enumerator
         (curr (default #unspecified))
         enumer)
      (inline make-hash-set #!key (comparator #f))
      (inline hash-set #!key (comparator #f) #!rest items)
      (inline hash-set-insert! hash-set::%hash-set item)
      (inline hash-set-delete! hash-set::%hash-set item)
      (inline hash-set-contains? hash-set::%hash-set item)
      (inline hash-set-length hash-set::%hash-set)
      (inline hash-set-empty? hash-set::%hash-set)
      (inline hash-set? obj)
      (inline hash-set-copy hash-set::%hash-set)
      (make-hash-set-enumerator set::%hash-set)))


(define-inline (make-hash-set #!key (comparator #f))
   (instantiate::%hash-set (hash (hashtable :comparator comparator))))

(define-inline (hash-set #!key (comparator #f) #!rest items)
   (let ((set (make-hash-set :comparator comparator)))
      (for-each (lambda (item)
                   (hash-set-insert! set item)) items)
      set))

(define-inline (hash-set? obj)
   (isa? obj %hash-set))

(define-inline (hash-set-insert! hash-set::%hash-set item)
   (hashtable-put! (-> hash-set hash) item 0))

(define-inline (hash-set-delete! hash-set::%hash-set item)
   (hashtable-remove! (-> hash-set hash) item))

(define-inline (hash-set-contains? hash-set::%hash-set item)
   (hashtable-contains? (-> hash-set hash) item))

(define-inline (hash-set-length hash-set::%hash-set)
   (hashtable-size (-> hash-set hash)))

(define-inline (hash-set-empty? hash-set::%hash-set)
   (= 0 (hashtable-size (-> hash-set hash))))

(define-inline (hash-set-copy hash-set::%hash-set)
   (duplicate::%hash-set hash-set (hash (hashtable-copy (-> hash-set hash)))))


;;;; set protocol

(define-method (set? obj::%hash-set)
   #t)

(define-method (set-empty? set::%hash-set)
   (hash-set-empty? set))

(define-method (set-insert! set::%hash-set item)
   (hash-set-insert! set item))

(define-method (set-delete! set::%hash-set item)
   (hash-set-delete! set item))

(define-method (set-contains? set::%hash-set item)
   (hash-set-contains? set item))

(define-method (set-length set::%hash-set)
   (hash-set-length set))

(define-method (set-copy set::%hash-set)
   (hash-set-copy set))


;;;; hash-set-enumerator
(define (make-hash-set-enumerator set::%hash-set)
   (instantiate::%hash-set-enumerator (enumer (get-enumer (-> set hash)))))
    
(define-method (enumerator? obj::%hash-set-enumerator)
   #t)

(define-method (enumerator-current enumer::%hash-set-enumerator)
   (if (eq? (-> enumer curr) #unspecified)
       (raise-invalid-state-exception :proc "enumerator-current"
          :msg "invalid state: enumerator-move-next! must be called before enumerator-current"
          :obj enumer)
       (=>key (-> enumer curr))))

(define-method (enumerator-copy enumer::%hash-set-enumerator)
   (instantiate::%hash-set-enumerator (curr (if (eq? (-> enumer curr)
                                                   #unspecified)
                                                (-> enumer curr)
                                                (=> (=>key (-> enumer curr))
                                                   (=>value (-> enumer curr)))))
                                      (enumer (dictionary-enumerator-copy (-> enumer enumer)))))

(define-method (enumerator-move-next! enumer::%hash-set-enumerator)
   (let ((res (dictionary-enumerator-move-next! (-> enumer enumer))))
      (if res
          (begin 
             (set! (-> enumer curr) (=> (dictionary-enumerator-key (-> enumer enumer))
                                       (dictionary-enumerator-value (-> enumer enumer))))
             res)
          res)))

;;;; enumerable protocol implementation
(define-method (enumerable? obj::%hash-set)
   #t)

(define-method (enumerable-enumerator obj::%hash-set)
   (make-hash-set-enumerator obj))


;;;; collection protocol implementation
(define-method (collection? obj::%hash-set)
   #t)

(define-method (collection-length obj::%hash-set)
   (hash-set-length obj))

(define-method (collection-enumerator obj::%hash-set)
   (make-hash-set-enumerator obj))

(define-method (collection-contains? obj::%hash-set item)
   (hash-set-contains? obj item))

(define-method (collection-empty? obj::%hash-set)
   (hash-set-empty? obj))

;;;; mutable-collection
(define-method (collection-mutable? obj::%hash-set)
   #t)

;;;; extendable protocol
(define-method (collection-extendable? obj::%hash-set)
   #t)

(define-method (collection-extend! obj::%hash-set item)
   (hash-set-insert! obj item))