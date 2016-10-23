(module hoard/dictionary
   (import hoard/exceptions
           hoard/enumerator
           hoard/dictionary-enumerator)
   (export (generic dictionary? obj)
           (generic dictionary-get obj key)
           (generic dictionary-put! obj key val)
           (generic dictionary-length obj)
           (generic dictionary-remove! obj key)
           (generic dictionary-empty? obj)
           (generic dictionary-enumerator obj)))

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


(define-generic (dictionary-enumerator obj)
   (cond ((hashtable? obj)
          (instantiate::%hashtable-enumerator (hash obj)))
         (else
          (raise-unsupported-operation-exception :proc "dictionary-enumerator"
             :obj obj))))

