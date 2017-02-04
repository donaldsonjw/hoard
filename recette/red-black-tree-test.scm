(module red-black-tree-test
   (library btest
            hoard)
   (export red-black-tree-tests))


(define-test-suite red-black-tree-tests

   (test "red-black-tree? returns false for non-tree items"
      (assert-false (red-black-tree? 1))
      (assert-false (red-black-tree? (list 1)))
      (assert-false (red-black-tree? (vector 3)))
      (assert-false (red-black-tree? (create-hashtable))))

   (test "red-black-tree? returns true for red-black-tree"
      (assert-true (red-black-tree? (make-red-black-tree :comparator +number-comparator+))))

   (test "make-red-black-tree raises an invalid argument exception if comparator is not a comparator object"
      (assert-exception-thrown (make-red-black-tree :comparator #f)
         &invalid-argument-exception))

   (test "a newly created red-black-tree has size 0"
      (assert-equal? (red-black-tree-size (make-red-black-tree :comparator +number-comparator+)) 0))

   (test "a red-black-tree with a single item inserted has size 1"
      (let ((tree (make-red-black-tree :comparator +number-comparator+)))
         (red-black-tree-insert! tree 2)
         (assert-equal? (red-black-tree-size tree) 1)))

   (test "(red-black-tree :comparator +number-comparator+ 1 2 3 4) contains 3"
      (let ((t (red-black-tree :comparator +number-comparator+ 1 2 3 4)))
         (assert-true (red-black-tree-contains? t 3))))

   (test "inserting 1 2 3 into a red black tree results in tree with 2 as the root
         and a black left child 1 and black right child 3"
      (let ((tree::%red-black-tree (make-red-black-tree :comparator +number-comparator+)))
         (red-black-tree-insert! tree 1)
         (red-black-tree-insert! tree 2)
         (red-black-tree-insert! tree 3)
         (assert-equal? (-> tree root item) 2)
         (assert-false (-> tree root red?))
         (assert-equal? (-> tree root left item) 1)
         (assert-false (-> tree root left red?))
         (assert-equal? (-> tree root right item) 3)
         (assert-false (-> tree root right red?))))


   (test "empty tree is balanced"
      (assert-true (red-black-tree-balanced? (make-red-black-tree :comparator +number-comparator+))))
   
   (test "1000 trees from 50 random numbers from 0 to 1000 is balanced"
      (do ((j 0 (+ j 1)))
          ((= j 1000))
          (let ((tree (make-red-black-tree :comparator +number-comparator+)))
             (do ((i 0 (+ i 1)))
                 ((= i 50))
                 (red-black-tree-insert! tree (random 1000)))
             (assert-true (red-black-tree-balanced? tree))
             )))

   (test "100 trees from 50 random numbers from 0 to 1000 remains blanced as we remove items from it"
      (do ((j 0 (+ j 1)))
          ((= j 100))
          (let ((tree (make-red-black-tree :comparator +number-comparator+))
                (items '()))
             (do ((i 0 (+ i 1)))
                 ((= i 50))
                 (let ((curr (random 1000)))
                    (set! items (cons curr items))
                    (red-black-tree-insert! tree curr)))
             (assert-true (red-black-tree-balanced? tree))
             (for-each (lambda (i) (red-black-tree-delete! tree i)
                               (when (not (red-black-tree-balanced? tree))
                                  (red-black-tree-visualize tree))
                               (assert-true (red-black-tree-balanced? tree)))
                items))))

   (test "red-black-tree-find-min and red-black-tree-find-max works"
      (let ((tree (make-red-black-tree :comparator +number-comparator+)))
         (for-each (lambda (i) (red-black-tree-insert! tree i)) (iota 100))
         (assert-equal? (red-black-tree-find-min tree) 0)
         (assert-equal? (red-black-tree-find-max tree) 99)))


   (test "red-black-tree-delete-min! works"
      (let ((tree (make-red-black-tree :comparator +number-comparator+)))
         (for-each (lambda (i) (red-black-tree-insert! tree i)) (iota 100))
         (assert-equal? (red-black-tree-find-min tree) 0)
         (red-black-tree-delete-min! tree)
         (assert-equal? (red-black-tree-find-min tree) 1)
         (assert-equal? (red-black-tree-find-max tree) 99)))


   (test "red-black-tree-delete-max! works"
      (let ((tree (make-red-black-tree :comparator +number-comparator+)))
         (for-each (lambda (i) (red-black-tree-insert! tree i)) (iota 100))
         (assert-equal? (red-black-tree-find-max tree) 99)
         ;(red-black-tree-visualize tree)
         (red-black-tree-delete-max! tree)
         ;(red-black-tree-visualize tree)
         (assert-equal? (red-black-tree-find-min tree) 0)
         (assert-equal? (red-black-tree-find-max tree) 98)))

   )





