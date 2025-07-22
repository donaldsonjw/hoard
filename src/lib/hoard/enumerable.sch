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
       (let ((enums (list obj ...))
             (p proc1))
          (if (every enum-or-enumer? enums)
              (instantiate::%map-enumerator (enumers (map get-enumer enums))
                                            (proc p))
              (raise-invalid-argument-exception proc: "enumerable-map" args: enums
                 msg: "not all arguments are enumerators or enumerables"))))))

           
(define-syntax enumerable-filter
   (syntax-rules ()
      ((_ predicate obj)
       (let ((t obj)
             (p predicate))
          (if (enum-or-enumer? t)
           (instantiate::%filter-enumerator (pred p)
                                            (enumer (get-enumer t)))
           (raise-invalid-argument-exception proc: "enumerable-filter" args: t
              msg: "not all arguments are enumerators or enumerables"))))))

(define-syntax enumerable-take-while
   (syntax-rules ()
      ((_ predicate obj)
       (let ((t obj)
             (p predicate))
          (if (enum-or-enumer? t)
           (instantiate::%take-enumerator (pred p)
                                          (enumer (get-enumer t)))
           (raise-invalid-argument-exception proc: "enumerable-take-while" args: t
              msg: "not all arguments are enumerators or enumerables"))))))

(define-syntax enumerable-take
   (syntax-rules ()
      ((_ num obj)
       (let ((t obj)
             (n num))
          (if (enum-or-enumer? t)
              (instantiate::%take-enumerator (pred (let ((count 0))
                                                      (lambda (v)
                                                         (set! count (+ count 1))
                                                         (<= count n))))
                                             (enumer (get-enumer t)))
              (raise-invalid-argument-exception proc: "enumerable-take" args: t
                 msg: "not all arguments are enumerators or enumerables"))))))


(define-syntax enumerable-fold
   (syntax-rules ()
      ((_ proc seed obj)
       (let ((t obj)
             (p proc)
             (s seed))
          (if (enum-or-enumer? t)
              (let ((enumer (get-enumer t)))
                 (let loop ((cont (enumerator-move-next! enumer))
                            (s s))
                    (if cont
                        (let ((ns (p s (enumerator-current enumer))))
                           (loop (enumerator-move-next! enumer)
                              ns))
                        s)))
              (raise-invalid-argument-exception proc: "enumerable-fold" args: t
                 msg: "not all arguments are enumerators or enumerables"))))
      ((_ proc seed obj rest ...)
       (let ((enums (list obj rest ...))
             (p proc)
             (s seed))
          (if (every enum-or-enumer? enums)
              (let ((enumers (map get-enumer enums)))
                 (let loop ((cont (every enumerator-move-next! enumers))
                            (s s))
                    (if cont
                        (let ((vals (map enumerator-current enumers)))
                           (let ((ns (apply p (cons s vals))))
                              (loop (every enumerator-move-next! enumers)
                                 ns)))
                        s)))
              (raise-invalid-argument-exception proc: "enumerable-fold" args: enums
                 msg: "not all arguments are enumerators or enumerables"))))))
       


(define-syntax enumerable-any?
   (syntax-rules ()
      ((_ pred obj)
       (let ((p pred)
             (t obj))
          (bind-exit (return)
             (if (enum-or-enumer? t)
                 (let ((enumer (get-enumer t)))
                    (let loop ((cont (enumerator-move-next! enumer)))
                       (if cont
                           (let ((res (p (enumerator-current enumer))))
                              (if (not res)
                                  (loop (enumerator-move-next! enumer))
                                  (return res)))
                           #f)))
              (raise-invalid-argument-exception proc: "enumerable-any" args: t
                 msg: "not all arguments are enumerators or enumerables")))))
      ((_ pred obj rest ...)
       (bind-exit (return)
          (let ((enums (list obj rest ...))
                (p pred))
             (if (every enum-or-enumer? enums)
                 (let ((enumers (map get-enumer enums)))
                    (let loop ((cont (every enumerator-move-next! enumers)))
                       (if cont
                           (let ((vals (map enumerator-current enumers)))
                              (let ((res (apply p  vals)))
                                 (if (not res)
                                     (loop (every enumerator-move-next! enumers))
                                     (return res))))
                           #f))))
             (raise-invalid-argument-exception proc: "enumerable-any" args: enums
                msg: "not all arguments are enumerators or enumerables"))))))


(define-syntax enumerable-every?
   (syntax-rules ()
      ((_ pred obj)
       (let ((t obj)
             (p pred))
          (bind-exit (return)
          (if (enum-or-enumer? t)
              (let ((enumer (get-enumer t)))
                 (let loop ((cont (enumerator-move-next! enumer)))
                    (if cont
                        (let ((res (p (enumerator-current enumer))))
                           (if res
                               (loop (enumerator-move-next! enumer))
                               (return res)))
                        #t)))
              (raise-invalid-argument-exception proc: "enumerable-every" args: t
                 msg: "not all arguments are enumerators or enumerables")))))
       ((_ pred obj rest ...)
        (bind-exit (return)
           (let ((enums (list obj rest ...))
                 (p pred))
              (if (every enum-or-enumer? enums)
                  (let ((enumers (map get-enumer enums)))
                     (let loop ((cont (every enumerator-move-next! enumers)))
                        (if cont
                            (let ((vals (map enumerator-current enumers)))
                               (let ((res (apply p  vals)))
                                  (if res
                                      (loop (every enumerator-move-next! enumers))
                                      (return res)))
                               #t))))
                  (raise-invalid-argument-exception proc: "enumerable-every" args: enums
                     msg: "not all arguments are enumerators or enumerables")))))))



(define-syntax enumerable-skip 
   (syntax-rules ()
      ((_ num obj)
       (let ((n num)
             (t obj))
          (if (enum-or-enumer? t)
              (let ((enumer (get-enumer t)))
              (let loop ((cont (enumerator-move-next! enumer))
                         (count 0))
                 (if (and cont (< count n))
                     (loop (enumerator-move-next! enumer)
                           (+ count 1))
                     enumer)))
           (raise-invalid-argument-exception proc: "enumerable-skip" args: t
              msg: "not all arguments are enumerators or enumerables"))))
      ((_ num obj rest ...)
       (let ((enms (list obj rest ...))
             (n num))
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
       (let ((t obj)
             (p pred))
          (if (enum-or-enumer? t)
              (let ((enumer (get-enumer t)))
                 (let loop ((cont (enumerator-move-next! enumer)))
                    (if (and cont (p (enumerator-current enumer)))
                        (loop (enumerator-move-next! enumer))
                        enumer)))
              (raise-invalid-argument-exception proc: "enumerable-skip-while" args: t
                 msg: "not all arguments are enumerators or enumerables"))))
      ((_ num obj rest ...)
       (let ((enms (list obj rest ...))
             (n num))
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
       (let ((t obj)
             (c coll))
          (if (and (collector? c)
                   (enum-or-enumer? t))
              (let ((| enumer | (get-enumer t)))
                 (let loop ((cont (enumerator-move-next! | enumer |))
                            (supp (collector-supplier c)))
                    (if cont
                        (let ((newsupp (collector-accumulate c supp (enumerator-current | enumer |))))
                           (loop (enumerator-move-next! | enumer |)
                              newsupp))
                        (collector-finish c supp))))
              (raise-invalid-argument-exception proc: "enumerable-collect" args: (list c t)
                 msg: "either we have an invalid enumerable/enumerator or invalid collector"))))))

