(module qsort-test
   (library btest
            hoard)
   (export qsort-tests))



;;;; an incremental implementation of qsort
;;;; inspired by the example found in the joy of clojure

(define (qsort enumerable #!optional (less-than <))
   (define (qsort-int enumer)
      (if (enumerator-move-next! enumer)
          (let* ((pivot (enumerator-current enumer))
                 (cln (enumerator-copy enumer)))
             (enumerable-append (qsort-int (enumerable-filter (lambda (x) (less-than x pivot)) enumer))
                (list pivot)
                (qsort-int (enumerable-filter (lambda (x) (not (less-than x pivot))) cln))))
          '()))
   (qsort-int (get-enumer enumerable)))


(define-test-suite qsort-tests
  
   (test "qsort '(4 2 3 1) results in '(1 2 3 4)"
      (let ((enumer (qsort '(4 2 3 1))))
         ;(fprint (current-error-port) "enumer: " enumer)
         (assert-equal? (enumerator->list (qsort '(4 2 3 1))) '(1 2 3 4))))

   (test "qsort '(10 9 8 7 6 5 4 3 2 1) results in '(1 2 3 4 5 6 7 8 9 10)"
      (let ((enumer (qsort '(4 2 3 1))))
         ;(fprint (current-error-port) "enumer: " enumer)
      (assert-equal? (enumerator->list (qsort '(10 9 8 7 6 5 4 3 2 1))) '(1 2 3 4 5 6 7 8 9 10)))))
   

   
       
    