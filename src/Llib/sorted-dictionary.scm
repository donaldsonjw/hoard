(module hoard/sorted-dictionary
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
      hoard/comparator
      hoard/indexable)
   (export
      (class %sorted-dictionary-enumerator::%red-black-tree-enumerator)
      (class %sorted-dictionary::%red-black-tree)
      (make-sorted-dictionary #!key comparator)
      (sorted-dictionary #!key comparator #!rest list-of-associations)
      (sorted-dictionary? obj)
      (sorted-dictionary-get dict::%sorted-dictionary key)
      (sorted-dictionary-put! dict::%sorted-dictionary key value)
      (sorted-dictionary-update! dict::%sorted-dictionary key value exists-fun)
      (sorted-dictionary-contains? dict::%sorted-dictionary key)
      (sorted-dictionary-remove! dict::%sorted-dictionary key)
      (sorted-dictionary-empty? dict::%sorted-dictionary)
      (make-sorted-dictionary-enumerator dict::%sorted-dictionary)
      (sorted-dictionary-copy dict::%sorted-dictionary)
      (sorted-dictionary-length dict::%sorted-dictionary)))

;;; utility procedures for sorted-dictionary implementation
(define-inline (list-of-associations? loa)
   (and (list? loa)
        (every association? loa)))

(define (sorted-dictionary? obj)
   (isa? obj %sorted-dictionary))

(define (make-sorted-dictionary #!key comparator)
   (when (not (comparator? comparator))
      (raise-invalid-argument-exception :proc "make-sorted-dictionary"
         :args comparator
         :msg "make-sorted-dictionary must be passed a valid comparator"))
   (instantiate::%sorted-dictionary (comparator
                                        (make-association-comparator comparator))))


(define (sorted-dictionary #!key comparator #!rest list-of-associations)
   (if (and (comparator? comparator)
            (list-of-associations? list-of-associations))
       (let ((dict (instantiate::%sorted-dictionary
                      (comparator (make-association-comparator comparator)))))
          (for-each (lambda (kv) (red-black-tree-insert! dict
                                    kv))
              list-of-associations)
           dict)
       (raise-invalid-argument-exception :proc "sorted-dictionary"
          :args (list :comparator comparator list-of-associations)
          :msg "must pass a valid comparator and values to include in sorted-dictionary must be lists of associations")))


(define (sorted-dictionary-length dict::%sorted-dictionary)
   (red-black-tree-size dict))

(define (sorted-dictionary-empty? dict::%sorted-dictionary)
   (red-black-tree-empty? dict))

(define (sorted-dictionary-contains? dict::%sorted-dictionary key)
   (let ((search-item (=> key #unspecified)))
      (red-black-tree-contains? dict search-item)))

(define (sorted-dictionary-get dict::%sorted-dictionary key)
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

(define (sorted-dictionary-put! dict::%sorted-dictionary key value)
   (let ((item (=> key value)))
      (red-black-tree-insert! dict item)))

(define (sorted-dictionary-update! dict::%sorted-dictionary key value exists-fun)
   (let ((item (=> key value)))
      (red-black-tree-insert! dict item
         :exists-fun (lambda (assoc) (=>value! assoc (exists-fun (=>value assoc)))
                             assoc))))

(define (sorted-dictionary-remove! dict::%sorted-dictionary key)
   (let ((item (=> key #unspecified)))
      (red-black-tree-delete! dict item)))

(define (sorted-dictionary-copy dict::%sorted-dictionary)
   (duplicate::%sorted-dictionary dict (root (red-black-node-copy (-> dict root)))))

;;; dictionary implementation

(define-method (dictionary? dict::%sorted-dictionary)
   #t)

(define-method (dictionary-get dict::%sorted-dictionary key)
   (sorted-dictionary-get dict key))

(define-method (dictionary-put! dict::%sorted-dictionary key val)
   (sorted-dictionary-put! dict key val))


(define-method (dictionary-update! dict::%sorted-dictionary key val exists-fun)
   (sorted-dictionary-update! dict key val exists-fun))

(define-method (dictionary-length dict::%sorted-dictionary)
   (sorted-dictionary-length dict))

(define-method (dictionary-remove! dict::%sorted-dictionary key)
   (sorted-dictionary-remove! dict key))

(define-method (dictionary-empty? dict::%sorted-dictionary)
   (sorted-dictionary-empty? dict))

(define-method (dictionary-contains? dict::%sorted-dictionary key)
   (sorted-dictionary-contains? dict key))

(define-method (dictionary-copy dict::%sorted-dictionary)
   (sorted-dictionary-copy dict))

(define-method (dictionary-enumerator dict::%sorted-dictionary)
   (make-sorted-dictionary-enumerator dict))

;;; collection implementation
(define-method (collection? dict::%sorted-dictionary)
   #t)

(define-method (collection-length dict::%sorted-dictionary)
   (sorted-dictionary-length dict))

(define-method (collection-enumerator dict::%sorted-dictionary)
   (make-sorted-dictionary-enumerator dict))

(define-method (collection-contains? dict::%sorted-dictionary itm)
   (enumerable-any? (lambda (a) (equal? a itm)) dict))

(define-method (collection-empty? dict::%sorted-dictionary)
   (sorted-dictionary-empty? dict))

(define-method (collection-copy dict::%sorted-dictionary)
   (sorted-dictionary-copy dict))

;;; sorted-dictionary-enumerator implementation
(define (make-sorted-dictionary-enumerator dict::%sorted-dictionary)
   (let ((stack (linked-stack)))
      (when (not (sorted-dictionary-empty? dict))
         (stack-push! stack (-> dict root)))
      (instantiate::%sorted-dictionary-enumerator (curr +red-black-node-nil+) (nodes stack))))

(define-method (enumerator-current enumer::%sorted-dictionary-enumerator)
   (let ((association (call-next-method)))
      (=>value association)))

(define-method (enumerator-copy enumer::%sorted-dictionary-enumerator)
   (duplicate::%sorted-dictionary-enumerator enumer
      (nodes (stack-copy (-> enumer nodes)))))

;;; dictionary-enumerator implementation
(define-method (dictionary-enumerator? enumer::%sorted-dictionary-enumerator)
   #t)

(define-method (dictionary-enumerator-move-next! enumer::%sorted-dictionary-enumerator)
   (enumerator-move-next! enumer))

(define-method (dictionary-enumerator-current enumer::%sorted-dictionary-enumerator)
   (if (not (-> enumer started))
       (raise-invalid-state-exception :proc "dictionary-enumerator-current"
          :msg "invalid state; dicitionary-enumerator-move-next! must be called before dictionary-enumerator-current"
          :obj enumer)
       (-> enumer curr item)))

(define-method (dictionary-enumerator-key enumer::%sorted-dictionary-enumerator)
   (define (get-item)
      (if (not (-> enumer started))
       (raise-invalid-state-exception :proc "dictionary-enumerator-key"
          :msg "invalid state; enumerator-move-next! must be called before dictionary-enumerator-key"
          :obj enumer)
       (-> enumer curr item)))
   (let ((association (get-item)))
      (=>key association)))

(define-method (dictionary-enumerator-value enumer::%sorted-dictionary-enumerator)
   (enumerator-current enumer))

(define-method (dictionary-enumerator-copy enumer::%sorted-dictionary-enumerator)
   (enumerator-copy enumer))


;;; enumerable implementation
(define-method (enumerable? dict::%sorted-dictionary)
   #t)

(define-method (enumerable-enumerator dict::%sorted-dictionary)
  (make-sorted-dictionary-enumerator dict))

;;; dictionary-enumerable implementation
(define-method (dictionary-enumerable? dict::%sorted-dictionary)
   #t)
   
(define-method (dictionary-enumerable-enumerator dict::%sorted-dictionary)
   (make-sorted-dictionary-enumerator dict))


;;; sorted-dictionary is mutable
(define-method (collection-mutable? dict::%sorted-dictionary)
   #t)


;;; sorted dictionary is extendable
(define-method (collection-extendable? dict::%sorted-dictionary)
   #t)

(define-method (collection-extend! dict::%sorted-dictionary item)
   (if (association? item)
       (sorted-dictionary-put! dict (=>key item) (=>value item))
       (raise-invalid-argument-exception :proc "collection-extend!"
          :args (list dict item)))) 


;;; sorted dictionary is indexable

(define-method (collection-indexable? obj::%sorted-dictionary)
   #t)

(define-method (collection-ref dict::%sorted-dictionary index
                  #!optional (default +collection-unspecified+))
   (let ((res (sorted-dictionary-get dict index)))
      (if res
          res
          (if (specified? default) default
              (raise-invalid-index-exception :proc "collection-ref"
                 :index index)))))

(define-method (collection-set! dict::%sorted-dictionary index val)
   (sorted-dictionary-put! dict index val))