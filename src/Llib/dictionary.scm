(module hoard/dictionary
   (import hoard/exceptions
           hoard/enumerator
           hoard/hashtable-ext
           hoard/dictionary-enumerator)
   (export (generic dictionary? obj)
           (generic dictionary-get obj key)
           (generic dictionary-put! obj key val)
           (generic dictionary-length obj)
           (generic dictionary-remove! obj key)
           (generic dictionary-empty? obj)
           (generic dictionary-enumerator obj)
           (generic dictionary-contains? obj key)
           (generic dictionary-update! obj key val exist-fun)
           (generic dictionary-copy obj)))

(define-generic (dictionary? obj)
   (hashtable? obj))

(define-generic (dictionary-empty? obj)
   (cond ((hashtable? obj)
          (= (hashtable-size obj) 0))
         (else
          (raise-unsupported-operation-exception :proc "dictionary-empty?"
             :obj obj))))

(define-generic (dictionary-contains? obj key)
   (cond ((hashtable? obj)
          (hashtable-contains? obj key))
         (else
          (raise-unsupported-operation-exception :proc "dictionary-contains"
             :obj obj))))

(define-generic (dictionary-get obj key)
   (cond ((hashtable? obj)
          (hashtable-get obj key))
         (else
          (raise-unsupported-operation-exception :proc "dictionary-get"
             :obj obj))))

(define-generic (dictionary-put! obj key val)
   (cond ((hashtable? obj)
          (hashtable-put! obj key val))
         (else
          (raise-unsupported-operation-exception :proc "dictionary-put!"
             :obj obj))))

(define-generic (dictionary-update! obj key val exist-fun)
   (cond ((hashtable? obj)
          (hashtable-update! obj key exist-fun val))
         (else
          (raise-unsupported-operation-exception :proc "dictionary-update!"
             :obj obj))))

(define-generic (dictionary-remove! obj key)
   (cond ((hashtable? obj)
          (hashtable-remove! obj key))
         (else
          (raise-unsupported-operation-exception :proc "dictionary-remove!"
             :obj obj))))

(define-generic (dictionary-length obj)
   (cond ((hashtable? obj)
          (hashtable-size obj))
         (else
          (raise-unsupported-operation-exception :proc "dictionary-length"
             :obj obj))))

(define-generic (dictionary-copy obj)
   (cond ((hashtable? obj)
          (hashtable-copy obj))
         ((vector? obj)
          (vector-copy obj 0 (vector-length obj)))
         ((string? obj)
          (string-copy obj))
         (else
          (raise-unsupported-operation-exception :proc "dictionary-copy"
             :obj obj))))
        


(define-generic (dictionary-enumerator obj)
   (cond ((hashtable? obj)
          (instantiate::%hashtable-enumerator (hash obj)))
         ((vector? obj)
          (instantiate::%vector-enumerator (vec obj)))
         ((string? obj)
          (instantiate::%string-enumerator (str obj)))
         (else
          (raise-unsupported-operation-exception :proc "dictionary-enumerator"
             :obj obj))))



