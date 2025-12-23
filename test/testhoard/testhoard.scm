(module testhoard
   (import collection-test
           mutable-collection-test
           enumerator-test
           comparator-test
           enumerable-test
           extendable-test
           indexable-test
           dictionary-test
           sorted-dictionary-test
           dictionary-enumerator-test
           dictionary-enumerable-test
           stretchy-vector-test
           qsort-test
           range-test
           longest-alphabetically-ordered-substring
           red-black-tree-test
           linked-queue-test
           ring-buffer-test
           contiguous-queue-test
           linked-stack-test
           contiguous-stack-test
           hashtable-ext-test
           binary-heap-test
           pairing-heap-test
           association-test
           hash-bag-test
           sorted-bag-test
           hash-set-test
           sorted-set-test
           linked-deque-test)
   (main main)
   (library btest hoard))


(define-test-suite hoard-tests)

(define (main args)

   (let* ((h1 (binary-heap :capacity 6 :comparator +number-comparator+ 9 3 8 5))
            (h2 (binary-heap-copy h1)))
         (equal? (binary-heap-length h1)
            (binary-heap-length h2))
         (equal? (binary-heap-first h1)
            (binary-heap-first h2))
         (binary-heap-dequeue! h1)
         (equal? (binary-heap-first h1)
            (binary-heap-first h2))
         (equal? (binary-heap-length h1)
            (binary-heap-length h2)))
   
   (suite-add-subsuite! hoard-tests collection-tests)
   (suite-add-subsuite! hoard-tests mutable-collection-tests)
   (suite-add-subsuite! hoard-tests enumerator-tests)
   (suite-add-subsuite! hoard-tests comparator-tests)
   (suite-add-subsuite! hoard-tests dictionary-tests)
   (suite-add-subsuite! hoard-tests sorted-dictionary-tests) 
   (suite-add-subsuite! hoard-tests dictionary-enumerator-tests)
   (suite-add-subsuite! hoard-tests enumerable-tests)
   (suite-add-subsuite! hoard-tests extendable-tests)
   (suite-add-subsuite! hoard-tests indexable-tests)
   (suite-add-subsuite! hoard-tests stretchy-vector-tests)
   (suite-add-subsuite! hoard-tests qsort-tests)
   (suite-add-subsuite! hoard-tests range-tests)
   (suite-add-subsuite! hoard-tests longest-alphabetically-ordered-substring-tests)
   (suite-add-subsuite! hoard-tests linked-queue-tests)
   (suite-add-subsuite! hoard-tests contiguous-queue-tests)
   (suite-add-subsuite! hoard-tests linked-stack-tests)
   (suite-add-subsuite! hoard-tests contiguous-stack-tests)
   (suite-add-subsuite! hoard-tests red-black-tree-tests)
   (suite-add-subsuite! hoard-tests hashtable-ext-tests)
   (suite-add-subsuite! hoard-tests dictionary-enumerable-tests)
   (suite-add-subsuite! hoard-tests binary-heap-tests)
   (suite-add-subsuite! hoard-tests pairing-heap-tests)
   (suite-add-subsuite! hoard-tests association-tests)
   (suite-add-subsuite! hoard-tests hash-bag-tests)
   (suite-add-subsuite! hoard-tests sorted-bag-tests)
   (suite-add-subsuite! hoard-tests hash-set-tests)
   (suite-add-subsuite! hoard-tests sorted-set-tests)
   (suite-add-subsuite! hoard-tests ring-buffer-tests)
   (suite-add-subsuite! hoard-tests linked-deque-tests)
   (let ((tr (instantiate::terminal-test-runner (suite hoard-tests))))
      (if (test-runner-execute tr #t) 0 -1)))


  