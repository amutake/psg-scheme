(define reverse*
  (lambda (ls)
    (let loop ((ls ls) (a '()))
      (if (null? ls)
          a
          (loop (cdr ls) (cons (car ls) a))))))

(define cond-test
  (lambda (x)
    (cond ((eqv? x 'a) 1)
          ((eqv? x 'b) 2)
          ((eqv? x 'c) 3)
          (else 0))))
