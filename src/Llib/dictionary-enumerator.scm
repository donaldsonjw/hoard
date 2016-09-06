(module hoard/dictionary-enumerator
   (import hoard/enumerator)
   (export (class hashtable-dictionary-enumerator::hashtable-enumerator)
           (generic dictionary-enumerator-move-next! obj)
           (generic dictionary-enumerator-current obj)
           (generic dictionary-enumerator-key obj)
           (generic dictionary-enumerator-value obj)))



;;;; dictionary enumerator protocol

(define-generic (dictionary-enumerator? obj)
   #f)
(define-generic (dictionary-enumerator-move-next! obj))
(define-generic (dictionary-enumerator-current obj))
(define-generic (dictionary-enumerator-key obj))
(define-generic (dictionary-enumerator-value obj))



;;;; hashtable implementation

(define-method (dictionary-enumerator? enumerator::hashtable-dictionary-enumerator)
   #t)

(define-method (dictionary-enumerator-move-next! enumerator::hashtable-dictionary-enumerator)
   (enumerator-move-next! enumerator))

(define-method (dictionary-enumerator-current enumerator::hashtable-dictionary-enumerator)
   (enumerator-current enumerator))

(define-method (dictionary-enumerator-key enumerator::hashtable-dictionary-enumerator)
   (if (not (-> enumerator started))
       (error "enumerator-current" "invalid state; enumerator-move-next must be called before dictionary-enumerator-key"
          enumerator)
       (car (-> enumerator curr-key))))

(define-method (dictionary-enumerator-value enumerator::hashtable-dictionary-enumerator)
   (enumerator-current enumerator))


