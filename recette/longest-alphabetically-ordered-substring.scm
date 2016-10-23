(module longest-alphabetically-ordered-substring
   (library hoard
            btest)
   (export longest-alphabetically-ordered-substring-tests))


(define (end-index-of-longest-alphabetically-ordered-substring str start-index)
   (do ((i (+  start-index 1) (+ i 1)))
       ((or (= i (collection-length str))
            (char>? (collection-ref str (- i 1)) (collection-ref str i))) i)))


(define (longest-alphabetically-ordered-substring str)
   (let loop ((focus 0)
              (lc-start 0)
              (lc-end 0))
      (if (= focus (collection-length str))
          (enumerator->string (collection-slice str (range :start lc-start end: lc-end)))
          (let ((next-focus (end-index-of-longest-alphabetically-ordered-substring str focus)))
             (if (> (- next-focus focus) (- lc-end lc-start))
                 (loop next-focus
                    focus
                    next-focus)
                 (loop next-focus
                    lc-start
                    lc-end))))))




(define-test-suite longest-alphabetically-ordered-substring-tests

   (test "(longest-alphabetically-ordered-substring \"\") returns \"\""
      (assert-equal? (longest-alphabetically-ordered-substring "") ""))

   (test "(longest-alphabetically-ordered-substring \"abcbabcdea\") returns \"abcde\""
      (assert-equal? (longest-alphabetically-ordered-substring "abcbabcdea") "abcde"))

   (test "(longest-alphabetically-ordered-substring \"abababab\") returns \"ab\""
      (assert-equal? (longest-alphabetically-ordered-substring "abababab") "ab"))

   (test "(longest-alphabetically-ordered-substring \"a\") returns \"a\""
      (assert-equal? (longest-alphabetically-ordered-substring "a") "a")))
   
