(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))

(define (not x) (if x #f #t))

(define reverse
  (lambda (ls)
    (letrec ((iter (lambda (ls a)
                     (if (null? ls)
                         a
                         (iter (cdr ls) (cons (car ls) a))))))
      (iter ls '()))))

(define (list . xs) xs)
