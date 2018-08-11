;;;; varius utility macros built on top of dictionary-enumerable and the dictionary-enumerator protocol

(define-syntax dictionary-enumerable-for-each
   (syntax-rules ()
      ((_ proc dict)
       (let ((p proc)
             (d dict))
          (if (dictionary-enum-or-enumer? d)
           (let ((enumer (get-dictionary-enumer d)))
              (let loop ((cont (dictionary-enumerator-move-next! enumer)))
                 (when cont
                    (p (dictionary-enumerator-key enumer)
                       (dictionary-enumerator-value enumer))
                    (loop (dictionary-enumerator-move-next! enumer)))))
           (raise-invalid-argument-exception :proc "dictionary-enumerable-for-each"
              :args d
              :msg "agument is not a dictiontary-enumberator or dictionary-enumerable"))))))

(define-syntax dictionary-enumerable-map
   (syntax-rules ()
      ((_ proc1 dict)
       (let ((p proc1)
             (d dict))
          (if (dictionary-enum-or-enumer? d)
           (instantiate::%dictionary-map-enumerator (enumer (get-dictionary-enumer d))
                                                    (proc p))
           (raise-invalid-argument-exception :proc "dictionary-enumerable-map" :args d
              :msg "argument is not a dictionary-enumerator or dictionary-enumerable"))))))

(define-syntax dictionary-enumerable-filter
   (syntax-rules ()
      ((_ predicate dict)
       (let ((p predicate)
             (d dict))
          (if (dictionary-enum-or-enumer? d)
              (instantiate::%dictionary-filter-enumerator (pred p)
                                                          (enumer (get-dictionary-enumer d)))
              (raise-invalid-argument-exception proc: "dictionary-enumerable-filter" args: d
                 msg: "argument is not a dictionary-enumerator or dictionary-enumerable"))))))

(define-syntax dictionary-enumerable-fold
   (syntax-rules ()
      ((_ proc seed dict)
       (let ((p proc)
             (d dict))
          (if (dictionary-enum-or-enumer? d)
              (let ((enumer (get-dictionary-enumer d)))
                 (let loop ((cont (dictionary-enumerator-move-next! enumer))
                            (s seed))
                    (if cont
                        (let ((ns (proc s (dictionary-enumerator-key enumer)
                                     (dictionary-enumerator-value enumer))))
                           (loop (dictionary-enumerator-move-next! enumer)
                              ns))
                        s)))
              (raise-invalid-argument-exception :proc "dictionary-enumerable-fold" :args d
                 :msg "argument is not a dictionary-enumerator or dictionary-enumerable"))))))

(define-syntax dictionary-enumerable-any?
   (syntax-rules ()
      ((_ pred dict)
       (let ((p pred)
             (d dict))
          (bind-exit (return)
          (if (dictionary-enum-or-enumer? d)
              (let ((enumer (get-dictionary-enumer d)))
                 (let loop ((cont (dictionary-enumerator-move-next! enumer)))
                    (if cont
                        (let ((res (p (dictionary-enumerator-key enumer)
                                      (dictionary-enumerator-value enumer))))
                           (if (not res)
                               (loop (dictionary-enumerator-move-next! enumer))
                               (return res)))
                        #f)))
              (raise-invalid-argument-exception :proc "dictionary-enumerable-any?" :args d
                 :msg "argument is not a dictionary-enumerator or dictionary-enumerable")))))))

(define-syntax dictionary-enumerable-every?
   (syntax-rules ()
      ((_ pred dict)
       (let ((p pred)
             (d dict))
          (bind-exit (return)
          (if (dictionary-enum-or-enumer? d)
              (let ((enumer (get-dictionary-enumer d)))
                 (let loop ((cont (dictionary-enumerator-move-next! enumer)))
                    (if cont
                        (let ((res (p (dictionary-enumerator-key enumer)
                                      (dictionary-enumerator-value enumer))))
                           (if (not res)
                               (return res)
                               (loop (dictionary-enumerator-move-next! enumer))))
                        #t)))
              (raise-invalid-argument-exception :proc "dictionary-enumerable-every?" :args d
                 :msg "argument is not a dictionary-enumerator or dictionary-enumerable")))))))

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
       (let ((o obj)
             (c coll))
          (if (and (collector? c)
                (dictionary-enum-or-enumer? o))
           (let ((enumer (get-dictionary-enumer o)))
              (let loop ((cont (dictionary-enumerator-move-next! enumer))
                         (supp (collector-supplier c)))
                 (if cont
                     (let ((newsupp (collector-accumulate c supp (dictionary-enumerator-current enumer))))
                        (loop (dictionary-enumerator-move-next! enumer)
                           newsupp))
                     (collector-finish c supp))))
           (raise-invalid-argument-exception proc: "dictionary-enumerable-collect" args: (list c o)
              msg: "either we have an invalid enumerable/enumerator or invalid collector"))))))





