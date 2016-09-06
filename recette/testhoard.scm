(module testhoard
   (import collection-test
           mutable-collection-test
           enumerator-test
           enumerable-test
           extendable-test
           indexable-test
           dictionary-enumerator-test
           stretchy-vector-test)
   (main main)
   (library btest hoard))


(define-test-suite hoard-tests)

(define (main args)

   (suite-add-subsuite! hoard-tests collection-tests)
   (suite-add-subsuite! hoard-tests mutable-collection-tests)
   (suite-add-subsuite! hoard-tests enumerator-tests)
   (suite-add-subsuite! hoard-tests dictionary-enumerator-tests)
   (suite-add-subsuite! hoard-tests enumerable-tests)
   (suite-add-subsuite! hoard-tests extendable-tests)
   (suite-add-subsuite! hoard-tests indexable-tests)
   (suite-add-subsuite! hoard-tests stretchy-vector-tests)
   (let ((tr (instantiate::terminal-test-runner (suite hoard-tests))))
      (test-runner-execute tr #t)))


  