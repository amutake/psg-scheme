(define (reset fn)
  ((lambda (save)
    (call/cc
      (lambda (cont)
        (set! *partcont*
              (lambda (val)
                (set! *partcont* save)
                (cont val)))
        (*partcont* (fn)))))
   *partcont*))
