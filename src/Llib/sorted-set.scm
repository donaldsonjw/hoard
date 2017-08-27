(module hoard/sorted-set
   (import hoard/set
           hoard/stack
           hoard/linked-stack
           hoard/exceptions
           hoard/comparator
           hoard/enumerator
           hoard/enumerable
           hoard/mutable-collection
           hoard/extendable
           hoard/red-black-tree
           hoard/collection)
   (export
      (class %sorted-set::%red-black-tree)
      (class %sorted-set-enumerator::%red-black-tree-enumerator)
      ( sorted-set? obj)
      ( make-sorted-set #!key comparator)
      ( sorted-set #!key comparator #!rest items)
      ( sorted-set-insert! set::%sorted-set item)
      ( sorted-set-delete! set::%sorted-set item)
      ( sorted-set-contains? set::%sorted-set item)
      ( sorted-set-length set::%sorted-set)
      ( sorted-set-empty? set::%sorted-set)
      ( sorted-set-copy set::%sorted-set)
      (make-sorted-set-enumerator set::%sorted-set)))


(define (sorted-set? obj)
   (isa? obj %sorted-set))

(define (make-sorted-set #!key comparator)
   (when (not (comparator? comparator))
      (raise-invalid-argument-exception :proc "make-sorted-set"
         :args comparator
         :msg "make-sorted-set must be passed a valid comparator"))
   (instantiate::%sorted-set (comparator comparator)))

(define (sorted-set #!key comparator #!rest items)
   (when (not (comparator? comparator))
      (raise-invalid-argument-exception :proc "sorted-set"
         :args comparator
         :msg "make-sorted-set must be passed a valid comparator"))
   (let ((set (make-sorted-set :comparator comparator)))
      (for-each (lambda (item) (sorted-set-insert! set item)) items) 
      set))

(define (sorted-set-insert! set::%sorted-set item)
   (red-black-tree-insert! set item))

(define (sorted-set-delete! set::%sorted-set item)
   (red-black-tree-delete! set item))

(define (sorted-set-contains? set::%sorted-set item)
   (red-black-tree-contains? set item))

(define (sorted-set-length set::%sorted-set)
   (red-black-tree-size set))

(define (sorted-set-empty? set::%sorted-set)
   (red-black-tree-empty? set))

(define (sorted-set-copy set::%sorted-set)
   (duplicate::%sorted-set set
      (root (red-black-node-copy (-> set root)))))


;;;; set protocol
(define-method (set? obj::%sorted-set)
   #t)

(define-method (set-empty? set::%sorted-set)
   (sorted-set-empty? set))

(define-method (set-insert! set::%sorted-set item)
   (sorted-set-insert! set item))

(define-method (set-delete! set::%sorted-set item)
   (sorted-set-delete! set item))

(define-method (set-contains? set::%sorted-set item)
   (sorted-set-contains? set item))

(define-method (set-length set::%sorted-set)
   (sorted-set-length set))

(define-method (set-copy set::%sorted-set)
   (sorted-set-copy set))

;;;; enumerator protocol implementation
(define (make-sorted-set-enumerator set::%sorted-set)
   (let ((stack (linked-stack)))
      (when (not (red-black-tree-empty? set))
         (stack-push! stack (-> set root)))
      (instantiate::%sorted-set-enumerator (curr +red-black-node-nil+) (nodes stack))))
   
(define-method (enumerator-copy enumer::%sorted-set-enumerator)
   (duplicate::%sorted-set-enumerator enumer
      (nodes (stack-copy (-> enumer nodes)))))


;;;; enumerable protocol implementation
(define-method (enumerable? obj::%sorted-set)
   #t)

(define-method (enumerable-enumerator obj::%sorted-set)
   (make-sorted-set-enumerator obj))

;;;; collection protocol implementation
(define-method (collection? obj::%sorted-set)
   #t)

(define-method (collection-length obj::%sorted-set)
   (sorted-set-length obj))

(define-method (collection-enumerator obj::%sorted-set)
   (make-sorted-set-enumerator obj))

(define-method (collection-contains? obj::%sorted-set item)
   (sorted-set-contains? obj item))

(define-method (collection-empty? obj::%sorted-set)
   (sorted-set-empty? obj))

;;;; mutable-collection
(define-method (collection-mutable? obj::%sorted-set)
   #t)

;;;; extendable protocol
(define-method (collection-extendable? obj::%sorted-set)
   #t)

(define-method (collection-extend! obj::%sorted-set item)
   (sorted-set-insert! obj item))
