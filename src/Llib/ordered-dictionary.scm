(module hoard/ordered-dictionary
   (include "enumerable.sch")
   (import
      hoard/mutable-collection
      hoard/extendable
      hoard/association
      hoard/exceptions
      hoard/enumerator
      hoard/dictionary-enumerator
      hoard/enumerable
      hoard/dictionary-enumerable
      hoard/dictionary
      hoard/red-black-tree
      hoard/collection
      hoard/stack
      hoard/linked-stack
      hoard/comparator)
   (export
      (class %ordered-dictionary-enumerator::%red-black-tree-enumerator)
      (class %ordered-dictionary::%red-black-tree)
      (make-ordered-dictionary #!key comparator)
      (ordered-dictionary #!key comparator #!rest list-of-associations)
      (ordered-dictionary? obj)
      (ordered-dictionary-get dict::%ordered-dictionary key)
      (ordered-dictionary-put! dict::%ordered-dictionary key value)
      (ordered-dictionary-contains? dict::%ordered-dictionary key)
      (ordered-dictionary-remove! dict::%ordered-dictionary key)
      (ordered-dictionary-empty? dict::%ordered-dictionary)))

;;; utility procedures for ordered-dictionary implementation
(define-inline (list-of-associations? loa)
   (and (list? loa)
        (every association? loa)))

(define (ordered-dictionary? obj)
   (isa? obj %ordered-dictionary))

(define (make-ordered-dictionary #!key comparator)
   (instantiate::%ordered-dictionary (comparator
                                        (make-association-comparator comparator))))


(define (ordered-dictionary #!key comparator #!rest list-of-associations)
   (if (and (comparator? comparator)
            (list-of-associations? list-of-associations))
       (let ((dict (instantiate::%ordered-dictionary
                      (comparator (make-association-comparator comparator)))))
          (for-each (lambda (kv) (red-black-tree-insert! dict
                                    kv))
              list-of-associations)
           dict)
       (raise-invalid-argument-exception :proc "ordered-dictionary"
          :args (list :comparator comparator list-of-associations)
          :msg "must pass a valid comparator and values to include in ordered-dictionary must be lists of associations")))


(define (ordered-dictionary-length dict::%ordered-dictionary)
   (red-black-tree-size dict))

(define (ordered-dictionary-empty? dict::%ordered-dictionary)
   (red-black-tree-empty? dict))

(define (ordered-dictionary-contains? dict::%ordered-dictionary key)
   (let ((search-item (=> key #unspecified)))
      (red-black-tree-contains? dict search-item)))

(define (ordered-dictionary-get dict::%ordered-dictionary key)
   (let ((search-item (=> key #unspecified)))
      (let loop ((curr::%red-black-node (-> dict root)))
         (cond ((eq? curr +red-black-node-nil+)
                #f)
               ((comparator<? (-> dict comparator) search-item (-> curr item))
                (loop (-> curr left)))
               ((comparator<? (-> dict comparator) (-> curr item) search-item)
                (loop (-> curr right)))
               (else
                (let* ((node::%red-black-node curr)
                       (item::%association (-> node item)))
                   (-> item value)))))))

(define (ordered-dictionary-put! dict::%ordered-dictionary key value)
   (let ((item (=> key value)))
      (red-black-tree-insert! dict item)))

(define (ordered-dictionary-remove! dict::%ordered-dictionary key)
   (let ((item (=> key #unspecified)))
      (red-black-tree-delete! dict item)))

;;; dictionary implementation

(define-method (dictionary? dict::%ordered-dictionary)
   #t)

(define-method (dictionary-get dict::%ordered-dictionary key)
   (ordered-dictionary-get dict key))

(define-method (dictionary-put! dict::%ordered-dictionary key val)
   (ordered-dictionary-put! dict key val))

(define-method (dictionary-length dict::%ordered-dictionary)
   (ordered-dictionary-length dict))

(define-method (dictionary-remove! dict::%ordered-dictionary key)
   (ordered-dictionary-remove! dict key))

(define-method (dictionary-empty? dict::%ordered-dictionary)
   (ordered-dictionary-empty? dict))

(define-method (dictionary-contains? dict::%ordered-dictionary key)
   (ordered-dictionary-contains? dict key))

(define-method (dictionary-enumerator dict::%ordered-dictionary)
   (make-red-black-tree-in-order-enumerator dict))

;;; collection implementation
(define-method (collection? dict::%ordered-dictionary)
   #t)

(define-method (collection-length dict::%ordered-dictionary)
   (ordered-dictionary-length dict))

(define-method (collection-enumerator dict::%ordered-dictionary)
   (make-red-black-tree-in-order-enumerator dict))

(define-method (collection-contains? dict::%ordered-dictionary itm)
   (enumerable-any? (lambda (a) (equal? a itm)) dict))

(define-method (collection-empty? dict::%ordered-dictionary)
   (ordered-dictionary-empty? dict))


;;; ordered-dictionary-enumerator implementation
(define (make-ordered-dictionary-enumerator dict::%ordered-dictionary)
   (let ((stack (linked-stack)))
      (when (not (ordered-dictionary-empty? dict))
         (stack-push! stack (-> dict root)))
      (instantiate::%ordered-dictionary-enumerator (curr +red-black-node-nil+) (nodes stack))))

(define-method (enumerator-current enumer::%ordered-dictionary-enumerator)
   (let ((association (call-next-method)))
      (=>value association)))

;;; dictionary-enumerator implementation
(define-method (dictionary-enumerator? enumer::%ordered-dictionary-enumerator)
   #t)

(define-method (dictionary-enumerator-move-next! enumer::%ordered-dictionary-enumerator)
   (enumerator-move-next! enumer))

(define-method (dictionary-enumerator-key enumer::%ordered-dictionary-enumerator)
   (define (get-item)
      (if (not (-> enumer started))
       (raise-invalid-state-exception :proc "dictionary-enumerator-key"
          :msg "invalid state; enumerator-move-next! must be called before dictionary-enumerator-key"
          :obj enumer)
       (-> enumer curr item)))
   (let ((association (get-item)))
      (=>key association)))

(define-method (dictionary-enumerator-value enumer::%ordered-dictionary-enumerator)
   (enumerator-current enumer))

(define-method (dictionary-enumerator-clone enumer::%ordered-dictionary-enumerator)
   (enumerator-clone enumer))


;;; enumerable implementation
(define-method (enumerable? dict::%ordered-dictionary)
   #t)

(define-method (enumerable-enumerator dict::%ordered-dictionary)
  (make-ordered-dictionary-enumerator dict))

;;; dictionary-enumerable implementation
(define-method (dictionary-enumerable? dict::%ordered-dictionary)
   #t)
   
(define-method (dictionary-enumerable-enumerator dict::%ordered-dictionary)
   (make-ordered-dictionary-enumerator dict))


;;; ordered-dictionary is mutable
(define-method (collection-mutable? dict::%ordered-dictionary)
   #t)


;;; ordered dictionary is extendable
(define-method (collection-extendable? dict::%ordered-dictionary)
   #t)

(define-method (collection-extend! dict::%ordered-dictionary item)
   (if (association? item)
       (ordered-dictionary-put! dict (=>key item) (=>value item))
       (raise-invalid-argument-exception :proc "collection-extend!"
          :args (list dict item)))) 


