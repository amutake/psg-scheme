(define (list . xs) xs)

(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define caar (lambda (x) (car (car x))))
(define cddr (lambda (x) (cdr (cdr x))))

(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))

(define (not x) (if x #f #t))

(define (null? x) (if (eqv? x '()) #t #f))

(define (map f xs)
  (if (null? xs)
      xs
      (cons (f (car xs)) (map f (cdr xs)))))
