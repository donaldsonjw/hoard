(module hoard/exceptions
   (export 
      (class &invalid-argument-exception::&error)
      (class &invalid-index-exception::&error)
      (class &invalid-state-exception::&error)
      (class &unsupported-operation-exception::&error)
      (raise-invalid-argument-exception #!key proc args (msg "invalid arguments"))
      (raise-invalid-index-exception #!key proc index)
      (raise-unsupported-operation-exception #!key proc obj)
      (raise-invalid-state-exception #!key proc msg obj)))



(define (raise-invalid-state-exception #!key proc msg obj)
   (raise (instantiate::&invalid-state-exception (proc proc)
                                                 (msg msg)
                                                 (obj obj))))

(define (raise-invalid-argument-exception #!key proc args (msg "invalid arguments"))
   (raise (instantiate::&invalid-argument-exception (proc  proc)
                                                    (obj args)
                                                    (msg msg))))

(define (raise-invalid-index-exception #!key proc index)
   (raise (instantiate::&invalid-index-exception (proc proc)
                                                 (obj index)
                                                 (msg "invalid index"))))

(define (raise-unsupported-operation-exception #!key proc obj)
   (raise (instantiate::&unsupported-operation-exception (proc proc)
                                                         (obj obj)
                                                         (msg "unsupported operation"))))