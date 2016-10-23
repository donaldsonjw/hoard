(module testhoard
   (import collection-test
           mutable-collection-test
           enumerator-test
           enumerable-test
           extendable-test
           indexable-test
           dictionary-test
           dictionary-enumerator-test
           stretchy-vector-test
           qsort-test
           range-test
           longest-alphabetically-ordered-substring
           red-black-tree-test
           linked-queue-test
           linked-stack-test
           hashtable-ext-test)
   (main main)
   (library btest hoard))


(define-test-suite hoard-tests)

(define (main args)

   (suite-add-subsuite! hoard-tests collection-tests)
   (suite-add-subsuite! hoard-tests mutable-collection-tests)
   (suite-add-subsuite! hoard-tests enumerator-tests)
   (suite-add-subsuite! hoard-tests dictionary-tests)
   (suite-add-subsuite! hoard-tests dictionary-enumerator-tests)
   (suite-add-subsuite! hoard-tests enumerable-tests)
   (suite-add-subsuite! hoard-tests extendable-tests)
   (suite-add-subsuite! hoard-tests indexable-tests)
   (suite-add-subsuite! hoard-tests stretchy-vector-tests)
   (suite-add-subsuite! hoard-tests qsort-tests)
   (suite-add-subsuite! hoard-tests range-tests)
   (suite-add-subsuite! hoard-tests longest-alphabetically-ordered-substring-tests)
   (suite-add-subsuite! hoard-tests linked-queue-tests)
   (suite-add-subsuite! hoard-tests linked-stack-tests)
   (suite-add-subsuite! hoard-tests red-black-tree-tests)
   (suite-add-subsuite! hoard-tests hashtable-ext-tests)
   (let ((tr (instantiate::terminal-test-runner (suite hoard-tests))))
      (test-runner-execute tr #t)))


  