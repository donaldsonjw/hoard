;;;; varius utility macros built on top of dictionary-enumerable and the dictionary-enumerator protocol

(define-syntax dictionary-enumerable-for-each
   (syntax-rules ()
      ((_ proc dict)
       (if (dictionary-enum-or-enumer? dict)
           (let ((enumer (get-dictionary-enumer dict)))
              (let loop ((cont (dictionary-enumerator-move-next! enumer)))
                 (when cont
                    (proc (dictionary-enumerator-key enumer)
                       (dictionary-enumerator-value enumer))
                    (loop (dictionary-enumerator-move-next! enumer)))))
           (raise-invalid-argument-exception :proc "dictionary-enumerable-for-each"
              :args dict
              :msg "agument is not a dictiontary-enumberator or dictionary-enumerable")))))

(define-syntax dictionary-enumerable-map
   (syntax-rules ()
      ((_ proc1 dict)
       (if (dictionary-enum-or-enumer? dict)
           (instantiate::%dictionary-map-enumerator (enumer (get-dictionary-enumer dict))
                                                    (proc proc1))
           (raise-invalid-argument-exception :proc "dictionary-enumerable-map" :args dict
              :msg "argument is not a dictionary-enumerator or dictionary-enumerable")))))

(define-syntax dictionary-enumerable-filter
   (syntax-rules ()
      ((_ predicate dict)
       (if (dictionary-enum-or-enumer? dict)
           (instantiate::%dictionary-filter-enumerator (pred predicate)
                                                       (enumer (get-dictionary-enumer dict)))
           (raise-invalid-argument-exception proc: "dictionary-enumerable-filter" args: dict
              msg: "argument is not a dictionary-enumerator or dictionary-enumerable")))))

(define-syntax dictionary-enumerable-fold
   (syntax-rules ()
      ((_ proc seed dict)
       (if (dictionary-enum-or-enumer? dict)
           (let ((enumer (get-dictionary-enumer dict)))
              (let loop ((cont (dictionary-enumerator-move-next! enumer))
                         (s seed))
                 (if cont
                     (let ((ns (proc s (dictionary-enumerator-key enumer)
                                  (dictionary-enumerator-value enumer))))
                        (loop (dictionary-enumerator-move-next! enumer)
                           ns))
                     s)))
           (raise-invalid-argument-exception :proc "dictionary-enumerable-fold" :args dict
              :msg "argument is not a dictionary-enumerator or dictionary-enumerable")))))

(define-syntax dictionary-enumerable-any?
   (syntax-rules ()
      ((_ pred dict)
       (bind-exit (return)
          (if (dictionary-enum-or-enumer? dict)
              (let ((enumer (get-dictionary-enumer dict)))
                 (let loop ((cont (dictionary-enumerator-move-next! enumer)))
                    (if cont
                        (let ((res (pred (dictionary-enumerator-key enumer)
                                      (dictionary-enumerator-value enumer))))
                           (if (not res)
                               (loop (dictionary-enumerator-move-next! enumer))
                               (return res)))
                        #f)))
              (raise-invalid-argument-exception :proc "dictionary-enumerable-any?" :args dict
                 :msg "argument is not a dictionary-enumerator or dictionary-enumerable"))))))

(define-syntax dictionary-enumerable-every?
   (syntax-rules ()
      ((_ pred dict)
       (bind-exit (return)
          (if (dictionary-enum-or-enumer? dict)
              (let ((enumer (get-dictionary-enumer dict)))
                 (let loop ((cont (dictionary-enumerator-move-next! enumer)))
                    (if cont
                        (let ((res (pred (dictionary-enumerator-key enumer)
                                      (dictionary-enumerator-value enumer))))
                           (if (not res)
                               (return res)
                               (loop (dictionary-enumerator-move-next! enumer))))
                        #t)))
              (raise-invalid-argument-exception :proc "dictionary-enumerable-every?" :args dict
                 :msg "argument is not a dictionary-enumerator or dictionary-enumerable"))))))

(define-syntax dictionary-enumerable-append
   (syntax-rules ()
      ((_ obj ...)
       (let ((enums (list obj ...)))
          (if (every dictionary-enum-or-enumer? enums)
              (instantiate::%dictionary-append-enumerator
                 (enumers (map get-dictionary-enumer
                             enums)))
              (raise-invalid-argument-exception proc: "dictionary-enumerable-append" args: enums
                 msg: "not all arguments are dictionary enumerators or dictionary enumerables"))))))


(define-syntax dictionary-enumerable-collect
   (syntax-rules ()
      ((_ obj coll)
       (if (and (collector? coll)
                (dictionary-enum-or-enumer? obj))
           (let ((enumer (get-dictionary-enumer obj)))
              (let loop ((cont (dictionary-enumerator-move-next! enumer))
                         (supp (collector-supplier coll)))
                 (if cont
                     (let ((newsupp (collector-accumulate coll supp (dictionary-enumerator-current enumer))))
                        (loop (dictionary-enumerator-move-next! enumer)
                           newsupp))
                     (collector-finish coll supp))))
           (raise-invalid-argument-exception proc: "dictionary-enumerable-collect" args: (list coll obj)
              msg: "either we have an invalid enumerable/enumerator or invalid collector")))))





