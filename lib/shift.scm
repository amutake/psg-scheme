(define (shift fn)
  (call/cc
    (lambda (cont)
      (*partcont*
        (fn (lambda (val)
              (reset (lambda () (cont val)))))))))
