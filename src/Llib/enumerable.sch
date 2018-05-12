;;;; varius utility macros built on top of enumerables and the enumerator protocol

(define-syntax enumerable-for-each
   (syntax-rules ()
      ((_ proc obj)
       (let ((t obj)
             (p proc))
          (if (enum-or-enumer? t)
              (let ((| enumer | (get-enumer t)))
                 (let loop ((cont (enumerator-move-next! | enumer |)))
                    (when cont
                       (p (enumerator-current | enumer |))
                       (loop (enumerator-move-next! | enumer |)))))
              (raise-invalid-argument-exception proc: "enumerable-for-each" args: obj
                 msg: "not all arguments are enumerators or enumerables"))))
      ((_ proc obj rest ...)
       (let ((enums (list obj rest ...))
             (p proc))
          (if (every enum-or-enumer? enums)
              (let ((enumers (map get-enumer enums)))
                 (let loop ((cont (every enumerator-move-next! enumers)))
                    (if cont
                        (let ((vals (map enumerator-current enumers)))
                           (apply p vals)
                           (loop (every enumerator-move-next! enumers))))))
              (raise-invalid-argument-exception proc: "enumerable-for-each" args: enums
                 msg: "not all arguments are enumerators or enumerables"))))))
       

(define-syntax enumerable-map
   (syntax-rules ()
      ((_ proc1 obj ...)
       (let ((enums (list obj ...)))
          (if (every enum-or-enumer? enums)
              (instantiate::%map-enumerator (enumers (map get-enumer enums))
                                            (proc proc1))
              (raise-invalid-argument-exception proc: "enumerable-map" args: (list obj ...)
                 msg: "not all arguments are enumerators or enumerables"))))))

           
(define-syntax enumerable-filter
   (syntax-rules ()
      ((_ predicate obj)
       (if (enum-or-enumer? obj)
           (instantiate::%filter-enumerator (pred predicate)
                                            (enumer (get-enumer obj)))
           (raise-invalid-argument-exception proc: "enumerable-filter" args: obj
              msg: "not all arguments are enumerators or enumerables")))))

(define-syntax enumerable-take-while
   (syntax-rules ()
      ((_ predicate obj)
       (if (enum-or-enumer? obj)
           (instantiate::%take-enumerator (pred predicate)
                                          (enumer (get-enumer obj)))
           (raise-invalid-argument-exception proc: "enumerable-take-while" args: obj
              msg: "not all arguments are enumerators or enumerables")))))

(define-syntax enumerable-take
   (syntax-rules ()
      ((_ n obj)
       (if (enum-or-enumer? obj)
           (instantiate::%take-enumerator (pred (let ((count 0))
                                                   (lambda (v)
                                                      (set! count (+ count 1))
                                                      (<= count n))))
                                          (enumer (get-enumer obj)))
           (raise-invalid-argument-exception proc: "enumerable-take" args: obj
              msg: "not all arguments are enumerators or enumerables")))))


(define-syntax enumerable-fold
   (syntax-rules ()
      ((_ proc seed obj)
       (if (enum-or-enumer? obj)
           (let ((enumer (get-enumer obj)))
              (let loop ((cont (enumerator-move-next! enumer))
                         (s seed))
                 (if cont
                     (let ((ns (proc s (enumerator-current enumer))))
                        (loop (enumerator-move-next! enumer)
                           ns))
                     s)))
           (raise-invalid-argument-exception proc: "enumerable-fold" args: obj
              msg: "not all arguments are enumerators or enumerables")))
      ((_ proc seed obj rest ...)
       (let ((enums (list obj rest ...)))
          (if (every enum-or-enumer? enums)
              (let ((enumers (map get-enumer enums)))
                 (let loop ((cont (every enumerator-move-next! enumers))
                            (s seed))
                    (if cont
                        (let ((vals (map enumerator-current enumers)))
                           (let ((ns (apply proc (cons s vals))))
                              (loop (every enumerator-move-next! enumers)
                                 ns)))
                        s)))
              (raise-invalid-argument-exception proc: "enumerable-fold" args: enums
                 msg: "not all arguments are enumerators or enumerables"))))))
       


(define-syntax enumerable-any?
   (syntax-rules ()
      ((_ pred obj)
       (bind-exit (return)
          (if (enum-or-enumer? obj)
              (let ((enumer (get-enumer obj)))
                 (let loop ((cont (enumerator-move-next! enumer)))
                    (if cont
                        (let ((res (pred (enumerator-current enumer))))
                           (if (not res)
                               (loop (enumerator-move-next! enumer))
                               (return res)))
                        #f)))
              (raise-invalid-argument-exception proc: "enumerable-any" args: obj
                 msg: "not all arguments are enumerators or enumerables"))))
      ((_ pred obj rest ...)
       (bind-exit (return)
          (let ((enums (list obj rest ...)))
             (if (every enum-or-enumer? enums)
                 (let ((enumers (map get-enumer enums)))
                    (let loop ((cont (every enumerator-move-next! enumers)))
                       (if cont
                           (let ((vals (map enumerator-current enumers)))
                              (let ((res (apply pred  vals)))
                                 (if (not res)
                                     (loop (every enumerator-move-next! enumers))
                                     (return res))))
                           #f))))
             (raise-invalid-argument-exception proc: "enumerable-any" args: enums
                msg: "not all arguments are enumerators or enumerables"))))))


(define-syntax enumerable-every?
   (syntax-rules ()
      ((_ pred obj)
       (bind-exit (return)
          (if (enum-or-enumer? obj)
              (let ((enumer (get-enumer obj)))
                 (let loop ((cont (enumerator-move-next! enumer)))
                    (if cont
                        (let ((res (pred (enumerator-current enumer))))
                           (if res
                               (loop (enumerator-move-next! enumer))
                               (return res)))
                        #t)))
              (raise-invalid-argument-exception proc: "enumerable-every" args: obj
                 msg: "not all arguments are enumerators or enumerables"))))
       ((_ pred obj rest ...)
        (bind-exit (return)
           (let ((enums (list obj rest ...)))
              (if (every enum-or-enumer? enums)
                  (let ((enumers (map get-enumer enums)))
                     (let loop ((cont (every enumerator-move-next! enumers)))
                        (if cont
                            (let ((vals (map enumerator-current enumers)))
                               (let ((res (apply pred  vals)))
                                  (if res
                                      (loop (every enumerator-move-next! enumers))
                                      (return res)))
                               #t))))
                  (raise-invalid-argument-exception proc: "enumerable-every" args: enums
                     msg: "not all arguments are enumerators or enumerables")))))))



(define-syntax enumerable-skip 
   (syntax-rules ()
      ((_ n obj)
       (if (enum-or-enumer? obj)
           (let ((enumer (get-enumer obj)))
              (let loop ((cont (enumerator-move-next! enumer))
                         (count 0))
                 (if (and cont (< count n))
                     (loop (enumerator-move-next! enumer)
                           (+ count 1))
                     enumer)))
           (raise-invalid-argument-exception proc: "enumerable-skip" args: obj
              msg: "not all arguments are enumerators or enumerables")))
      ((_ n obj rest ...)
       (let ((enms (list obj rest ...)))
          (if (every enum-or-enumer? enms)
              (let ((enumers (map get-enumer enms)))
                 (let loop ((cont (every enumerator-move-next! enumers))
                            (count 0))
                    (if (and cont (< count n))
                        (loop (every enumerator-move-next! enumers)
                           (+ count 1))
                        (instantiate::%aggregate-enumerator (enums enumers)))))
              (raise-invalid-argument-exception proc: "enumerable-skip" args: enms
                 msg: "not all arguments are enumerators or enumerables"))))))



(define-syntax enumerable-skip-while 
   (syntax-rules ()
      ((_ pred obj)
       (if (enum-or-enumer? obj)
           (let ((enumer (get-enumer obj)))
              (let loop ((cont (enumerator-move-next! enumer)))
                 (if (and cont (pred (enumerator-current enumer)))
                     (loop (enumerator-move-next! enumer))
                     enumer)))
           (raise-invalid-argument-exception proc: "enumerable-skip-while" args: obj
              msg: "not all arguments are enumerators or enumerables")))
      ((_ n obj rest ...)
       (let ((enms (list obj rest ...)))
          (if (every enum-or-enumer? enms)
              (let ((enumers (map get-enumer enms)))
                 (let loop ((cont (every enumerator-move-next! enumers)))
                    (if (and cont (apply pred (map enumerator-current enumers)))
                        (loop (every enumerator-move-next! enumers))
                        (instantiate::%aggregate-enumerator (enums enumers)))))
              (raise-invalid-argument-exception proc: "enumerable-skip-while" args: enms
                 msg: "not all arguments are enumerators or enumerables"))))))


(define-syntax enumerable-append
   (syntax-rules ()
      ((_ obj ...)
       (let ((enums (list obj ...)))
          (if (every enum-or-enumer? enums)
              (instantiate::%append-enumerator (enumers
                                                  (map get-enumer
                                                     enums)))
              (raise-invalid-argument-exception proc: "enumerable-append" args: enums
                 msg: "not all arguments are enumerators or enumerables"))))))


(define-syntax enumerable-collect
   (syntax-rules ()
      ((_ obj coll)
       (if (and (collector? coll)
                (enum-or-enumer? obj))
           (let ((enumer (get-enumer obj)))
              (let loop ((cont (enumerator-move-next! enumer))
                         (supp (collector-supplier coll)))
                 (if cont
                     (let ((newsupp (collector-accumulate coll supp (enumerator-current enumer))))
                        (loop (enumerator-move-next! enumer)
                           newsupp))
                     (collector-finish coll supp))))
           (raise-invalid-argument-exception proc: "enumerable-collect" args: (list coll obj)
              msg: "either we have an invalid enumerable/enumerator or invalid collector")))))

