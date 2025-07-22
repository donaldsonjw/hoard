(module hoard/set
   (include "enumerable.sch")
   (import hoard/exceptions
           hoard/enumerable
           hoard/enumerator)
   (export (generic set? obj)
           (generic set-empty? set)
           (generic set-insert! set item)
           (generic set-delete! set item)
           (generic set-contains? set item)
           (generic set-length set)
           (generic set-copy set)
           (generic set-union! set #!rest sets)
           (generic set-union set #!rest sets)
           (generic set-intersect! set #!rest sets)
           (generic set-intersect set #!rest sets)
           (generic set-difference set #!rest sets)
           (generic set-difference! set #!rest sets)))

;;;; set protocol
(define-generic (set? obj)
   #f)

(define-generic (set-empty? set))

(define-generic (set-insert! set item))

(define-generic (set-delete! set item))

(define-generic (set-contains? set item))

(define-generic (set-length set))

(define-generic (set-copy set))

(define-generic (set-union set #!rest sets)
   (let ((res (set-copy set)))
      (apply set-union! (cons res sets))
      res))

(define-generic (set-union! set #!rest sets)
   (if (and (set? set) (every set? sets))
       (for-each (lambda (s)
                    (enumerable-for-each (lambda (elem)
                                            (set-insert! set elem)) s)) sets)
       (raise-invalid-argument-exception :proc "set-union!"
          :args (cons set sets))))


(define-generic (set-intersect set #!rest sets)
   (let ((res (set-copy set)))
      (apply set-intersect! (cons res sets))
      res))

(define-generic (set-intersect! set #!rest sets)
   (if (and (set? set) (every set? sets))
       (enumerable-for-each (lambda (elem)
                               (when (not (every (lambda (s) (set-contains? s elem)) sets))
                                  (set-delete! set elem))) set)
       (raise-invalid-argument-exception :proc "set-intersect!"
          :args (cons set sets))))
              

(define-generic (set-difference set #!rest sets)
   (let ((res (set-copy set)))
      (apply set-difference! (cons res sets))
      res))

(define-generic (set-difference! set #!rest sets)
   (if (and (set? set) (every set? sets))
       (enumerable-for-each (lambda (elem)
                               (when (any (lambda (s) (set-contains? s elem)) sets)
                                  (set-delete! set elem))) set)
       (raise-invalid-argument-exception :proc "set-difference!"
          :args (cons set sets))))