;;;; a left-leaning red-black tree as described in Algorithms 4th Edition
;;;; by Robert Sedgewick and Kevin Wayne
;;;; The code is heavily based on the Java implementation found at
;;;; http://algs4.cs.princeton.edu/33balanced/RedBlackBST.java.html

(module hoard/red-black-tree
   (import hoard/exceptions
           hoard/queue
           hoard/linked-queue)
   
   (export (class %red-black-node
              item
              size
              (red? (default #f))
              (left::%red-black-node (default +red-black-node-nil+))
              (right::%red-black-node (default +red-black-node-nil+)))
           
           (class %red-black-tree
              less-than?::procedure
              (root::%red-black-node (default +red-black-node-nil+)))
           (inline red-black-tree? obj)
           +red-black-node-nil+
           (make-red-black-tree #!key less-than?)
           (red-black-tree-size tree::%red-black-tree)
           (red-black-tree-insert! tree::%red-black-tree item)
           (red-black-tree-contains? tree::%red-black-tree item)
           (red-black-tree #!key less-than? #!rest objs)
           (red-black-tree-empty? tree::%red-black-tree)
           (red-black-tree-visualize tree::%red-black-tree)
           (red-black-tree-delete! tree::%red-black-tree item)
           (red-black-tree-balanced? tree::%red-black-tree)
           (red-black-tree-delete-min! tree::%red-black-tree)
           (red-black-tree-find-min tree::%red-black-tree)
           (red-black-tree-find-max tree::%red-black-tree)
           (red-black-tree-delete-max! tree::%red-black-tree)))

(define +red-black-node-nil+ (class-nil %red-black-node))

(define (make-red-black-tree #!key less-than?)
   (if (not (procedure? less-than?))
       (raise-invalid-argument-exception :proc "make-red-black-tree"
          :args less-than?)
       (instantiate::%red-black-tree (less-than? less-than?))))

(define (red-black-tree #!key less-than? #!rest objs)
   (let ((tree (make-red-black-tree :less-than? less-than?)))
      (for-each (lambda (i) (red-black-tree-insert! tree i)) objs)
      tree))

(define-inline (red-black-tree? obj)
   (isa? obj %red-black-tree))

(define (red-black-tree-size tree::%red-black-tree)
   (node-size (-> tree root)))

(define (red-black-tree-empty? tree::%red-black-tree)
   (= (node-size (-> tree root)) 0))

(define (red-black-tree-insert! tree::%red-black-tree item)
   (set! (-> tree root) (node-insert! (-> tree root) item (-> tree less-than?)))
   (node-red?-set! (-> tree root) #f))

(define (red-black-tree-contains? tree::%red-black-tree item)
   (node-contains? (-> tree root) item (-> tree less-than?)))

(define (red-black-tree-find-min tree::%red-black-tree)
   (if (not (red-black-tree-empty? tree))
       (node-find-min (-> tree root))
       (raise-invalid-state-exception :proc "red-black-tree-find-min"
          msg: "cannot find min from empty red black tee"
          obj: tree)))

(define (red-black-tree-find-max tree::%red-black-tree)
   (if (not (red-black-tree-empty? tree))
       (node-find-max (-> tree root))
       (raise-invalid-state-exception :proc "red-black-tree-find-max"
          msg: "cannot find max from empty red black tree"
          obj: tree)))

          
(define (red-black-tree-delete! tree::%red-black-tree item)
   (when (red-black-tree-contains? tree item)
       (when (and (-> tree root left red?)
                  (-> tree root right red?))
          (set! (-> tree root red?) #t))
      (set! (-> tree root) (node-delete! (-> tree root) item (-> tree less-than?)))
      (when (not (red-black-tree-empty? tree))
         (set! (-> tree root red?) #f))))

(define (red-black-tree-delete-min! tree::%red-black-tree)
   (if (not (red-black-tree-empty? tree)) 
       (set! (-> tree root) (node-delete-min! (-> tree root)))
       (raise-invalid-state-exception :proc "red-black-tree-delete-min!"
          msg: "cannot delete min from empty red black tee"
          obj: tree)))

(define (red-black-tree-delete-max! tree::%red-black-tree)
   (if (not (red-black-tree-empty? tree))
       (begin
          (when (and (not (-> tree root left red?))
                     (not (-> tree root right red?)))
             (set! (-> tree root red?) #t))
          (set! (-> tree root) (node-delete-max! (-> tree root)))
          (when (not (red-black-tree-empty? tree))
             (set! (-> tree root red?) #f)))
       (raise-invalid-state-exception :proc "red-black-tree-delete-max!"
          msg: "cannot delete max from empty red black tee"
          obj: tree)))


(define (red-black-tree-bread-first-traversal tree::%red-black-tree proc)
   (let ((q (make-linked-queue)))
      (queue-enqueue! q (-> tree root))
      (let loop ()
         (when (not (queue-empty? q))
            (let ((curr::%red-black-node (queue-dequeue! q)))
               (when (not (eq? (-> curr left) +red-black-node-nil+))
                  (queue-enqueue! q (-> curr left)))
               (when (not (eq? (-> curr right) +red-black-node-nil+))
                  (queue-enqueue! q (-> curr right)))
               (proc curr)
               (loop))))))


(define (red-black-tree-visualize tree::%red-black-tree)
   (with-output-to-file "/tmp/tree.dot"
      (lambda ()
         (print "digraph tree {")
         (print " node[shape=\"circle\"];")
         (let ((res '()))
            (red-black-tree-bread-first-traversal tree
               (lambda (n) (set! res (cons n res))))
            (set! res (reverse! res))
            (for-each (lambda (n::%red-black-node)
                         (if (-> n red?)
                             (printf "\"~a\" [color=\"red\"];~%" (-> n item))
                             (printf  "\"~a\" [color=\"black\"];~%" (-> n item))))
               res)
            (for-each (lambda (n::%red-black-node)
                         (when (not (eq? (-> n left) +red-black-node-nil+))
                            (printf "\"~a\" -> \"~a\";~%" (-> n item) (-> n left item)))
                         (when (not (eq? (-> n right) +red-black-node-nil+))
                            (printf "\"~a\" -> \"~a\";~%" (-> n item) (-> n right item))))
               res))
         (print "}")))

   (system "dot -Tsvg /tmp/tree.dot > /tmp/tree.svg; eog /tmp/tree.svg")) 

;;;; node utility methods
(define-inline (node-red?-set! node::%red-black-node val)
   (set! (-> node red?) val))

(define-inline (node-size node::%red-black-node)
   (if (eq? node +red-black-node-nil+)
       0
       (-> node size)))

(define-inline (node-contains? node::%red-black-node item less-than?)
   (if (eq? node +red-black-node-nil+)
       #f
       (cond ((less-than?  item (-> node item))
              (node-contains? (-> node left) item less-than?))
             ((less-than? (-> node item) item)
              (node-contains? (-> node right) item less-than?))
             (else
              #t))))

(define-inline (node-find-min node::%red-black-node)
   (if (not (eq? (-> node left) +red-black-node-nil+))
       (node-find-min (-> node left))
       (-> node item)))

(define-inline (node-find-max node::%red-black-node)
   (if (not (eq? (-> node right) +red-black-node-nil+))
       (node-find-max (-> node right))
       (-> node item)))


(define-inline (node-delete! node::%red-black-node item less-than?)
   (define (node=? a b less-than?)
      (not (or (less-than? a b)
               (less-than? b a))))
   (bind-exit (return)
      (cond ((less-than? item (-> node item))
             (when (and (not (-> node left red?))
                        (not (-> node left left red?)))
                (set! node (node-move-red-left! node)))
             (set! (-> node left) (node-delete! (-> node left) item less-than?)))
            (else
             (when (-> node left red?)
                (set! node (node-rotate-right! node)))
             (when (and (node=? item (-> node item) less-than?)
                        (eq? (-> node right) +red-black-node-nil+))
                (return +red-black-node-nil+))
             (when (and (not (-> node right red?))
                        (not (-> node right left red?)))
                (set! node (node-move-red-right! node)))
             (if (node=? item (-> node item) less-than?)
                 (let ((m::%red-black-node (node-min (-> node right))))
                    (set! (-> node item) (-> m item))
                    (set! (-> node right) (node-delete-min! (-> node right))))
                 (set! (-> node right) (node-delete! (-> node right) item less-than?)))))
      (node-balance! node)))
             
(define-inline (node-delete-min! node::%red-black-node)
   (if (eq? (-> node left) +red-black-node-nil+)
       +red-black-node-nil+
       (begin
          (when (and (not (-> node left red?))
                     (not (-> node left left red?)))
             (set! node (node-move-red-left! node)))
          (set! (-> node left) (node-delete-min! (-> node left)))
          (node-balance! node))))

(define-inline (node-delete-max! node::%red-black-node)
   (when (-> node left red?)
      (set! node (node-rotate-right! node)))
   (if (eq? (-> node right) +red-black-node-nil+)
       +red-black-node-nil+
       (begin
          (when (and (not (-> node right red?))
                     (not (-> node right left red?)))
             (set! node (node-move-red-right! node)))
          (set! (-> node right) (node-delete-max! (-> node right)))
          (node-balance! node))))

(define-inline (node-min node::%red-black-node)
   (if (eq? (-> node left) +red-black-node-nil+)
       node
       (node-min (-> node left))))
   
                 
(define-inline (node-move-red-right! node::%red-black-node)
   (node-flip-colors! node)
   (when (-> node left left red?)
      (set! node (node-rotate-right! node))
      (node-flip-colors! node))
   node)


(define-inline (node-move-red-left! node::%red-black-node)
   (node-flip-colors! node)
   (when (-> node right left red?)
      (set! (-> node right) (node-rotate-right! (-> node right)))
      (set! node (node-rotate-left! node))
      (node-flip-colors! node))
   node)


(define-inline (node-balance! node::%red-black-node)
   (when (and (-> node right red?)
              (not (-> node left red?)))
      (set! node (node-rotate-left! node)))
   (when (and (-> node left red?)
              (-> node left left red?))
      (set! node (node-rotate-right! node)))
   (when (and (-> node left red?)
              (-> node right red?))
      (node-flip-colors! node))
   (set! (-> node size) (+ (node-size (-> node left))
                           (node-size (-> node right))
                           1))
   node)

 ;   1              3  
 ;  / \            / \ 
 ; 0   3    =>    1   4
 ;    / \        / \   
 ;   2   4      0   2  
(define-inline (node-rotate-left! node::%red-black-node)
   (let ((n::%red-black-node (-> node right)))
      (set! (-> node right) (-> n left))
      (set! (-> n left) node)
      (set! (-> n size) (-> node size))
      (set! (-> node size) (+ 1 (node-size (-> node left))
                                (node-size (-> node right))))
      (set! (-> n red?) (-> node red?))
      (set! (-> node red?) #t)
      n))

;     3          1    
;    / \        / \   
;   1   4  =>  0   3  
;  / \            / \ 
; 0   2          2   4                  
(define-inline (node-rotate-right! node::%red-black-node)
   (let ((n::%red-black-node (-> node left)))
      (set! (-> node left) (-> n right))
      (set! (-> n right) node)
      (set! (-> n size) (-> node size))
      (set! (-> node size) (+ 1 (node-size (-> node left))
                                (node-size (-> node right))))
      (set! (-> n red?) (-> node red?))
      (set! (-> node red?) #t)
      n))

(define-inline (node-flip-colors! node::%red-black-node)
   (set! (-> node red?)  (not (-> node red?)))
   (set! (-> node left red?) (not (-> node left red?)))
   (set! (-> node right red?) (not (-> node right red?))))
          
(define-inline (node-insert! node::%red-black-node item less-than?)
   (if (eq? node +red-black-node-nil+)
       (instantiate::%red-black-node 
          (item item)
          (size 1)
          (red? #t))
       (begin
          (cond ((less-than? item (-> node item))
                 (set! (-> node left) (node-insert! (-> node left) item less-than?)))
                ((less-than? (-> node item) item)
                 (set! (-> node right) (node-insert! (-> node right) item less-than?)))
                (else (set! (-> node item) item)))
          (node-balance! node))))


(define-inline (node-balanced? node::%red-black-node black-height)
   (if (eq? node +red-black-node-nil+)
       (= black-height 0)
       (let ((dec (if (-> node red?) 0 1)))
          (and (node-balanced?  (-> node left) (- black-height dec))
               (node-balanced? (-> node right) (- black-height dec))))))


(define (red-black-tree-balanced? tree::%red-black-tree)
   (define (node-black-height node::%red-black-node curr-height)
      (if (eq? node +red-black-node-nil+)
           curr-height
          (node-black-height (-> node left)
             (if (not (-> node red?)) (+ curr-height 1) curr-height))))

   (let ((black-height (node-black-height (-> tree root) 0)))
      (node-balanced? (-> tree root) black-height)))

   


       



       