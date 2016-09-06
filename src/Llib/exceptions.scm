(module hoard/exceptions
   (export 
      (class &invalid-argument-exception::&error)
      (class &invalid-index-exception::&error)
      (class &unsupported-operation-exception::&error)
      (raise-invalid-argument-exception #!key proc args)
      (raise-invalid-index-exception #!key proc index)
      (raise-unsupported-operation-exception #!key proc obj)))




(define (raise-invalid-argument-exception #!key proc args)
   (raise (instantiate::&invalid-argument-exception (proc  proc)
                                                    (obj args)
                                                    (msg "invalid argument" ))))

(define (raise-invalid-index-exception #!key proc index)
   (raise (instantiate::&invalid-index-exception (proc proc)
                                                 (obj index)
                                                 (msg "invalid index"))))


(define (raise-unsupported-operation-exception #!key proc obj)
   (raise (instantiate::&unsupported-operation-exception (proc proc)
                                                         (obj obj)
                                                         (msg "unsupported operation"))))