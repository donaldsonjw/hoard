(module hoard/stack
   (import hoard/exceptions)
   (export (generic stack? stk)
           (generic stack-push! stk val)
           (generic stack-pop! stk)
           (generic stack-top stk)
           (generic stack-length stk)
           (generic stack-fixed-capacity? queue)
           (generic stack-capacity queue)
           (generic stack-empty? stk)
           (generic stack-copy stk)))

(define-generic (stack? stk)
   #f)

(define-generic (stack-push! stk val))
  
(define-generic (stack-pop! stk))

(define-generic (stack-top stk))

(define-generic (stack-empty? stk))

(define-generic (stack-length stk))

(define-generic (stack-fixed-capacity? stk))

(define-generic (stack-capacity stk))

(define-generic (stack-copy stk))