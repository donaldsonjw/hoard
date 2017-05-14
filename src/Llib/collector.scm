(module hoard/collector
   (import hoard/exceptions
           hoard/stretchy-vector)
   (export
      (class %paramaterized-collector
         supplier
         accumulate
         finish
         combine)
      (generic collector? obj)
      (generic collector-supplier coll)
      (generic collector-accumulate coll supp val)
      (generic collector-finish coll supp)
      (generic collector-combine coll a b)
      +list-collector+
      +stretchy-vector-collector+
      +sum-collector+))

;;;; collector protocol, heavily influenced by the Collector interface in Java 8

(define-generic (collector? obj))

(define-generic (collector-supplier coll))

(define-generic (collector-accumulate coll supp val))

(define-generic (collector-finish coll supp))

(define-generic (collector-combine coll a b))


(define-method (collector? coll::%paramaterized-collector)
   #t)

(define-method (collector-supplier coll::%paramaterized-collector)
   ((-> coll supplier)))

(define-method (collector-accumulate coll::%paramaterized-collector supp val)
   ((-> coll accumulate) supp val))

(define-method (collector-finish coll::%paramaterized-collector supp)
   ((-> coll finish) supp))

(define-method (collector-combine coll::%paramaterized-collector a b)
   ((-> coll combine) a b))


(define (make-collector #!key supplier accumulate finish combine)
   (if (and supplier accumulate finish combine)
       (instantiate::%paramaterized-collector (supplier supplier)
                                              (accumulate accumulate)
                                              (finish finish)
                                              (combine combine))
       (raise-invalid-argument-exception :proc "make-collector"
          :msg "You must provide valid procedures for supplier accumulate finish and combine"
          :args (list supplier accumulate finish combine))))


(define +list-collector+
   (make-collector :supplier (lambda ()
                                '())
      :accumulate (lambda (supp val)
                     (cons val supp))
      :combine (lambda (a b)
                  (append a b))
      :finish (lambda (x) (reverse x))))

(define +stretchy-vector-collector+
   (make-collector :supplier (lambda ()
                                (stretchy-vector))
      :accumulate (lambda (supp val)
                     (stretchy-vector-extend! supp val)
                     supp)
      :combine (lambda (a b)
                  (stretchy-vector-append! a b)
                  a)
      :finish (lambda (x)
                 x)))


(define +sum-collector+
   (make-collector :supplier (lambda () 0)
      :accumulate (lambda (supp val)
                     (+ supp val))
      :combine (lambda (a b) (+ a b))
      :finish (lambda (x)
                 x)))


