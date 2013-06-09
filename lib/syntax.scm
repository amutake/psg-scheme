(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define caar (lambda (x) (car (car x))))
(define cddr (lambda (x) (cdr (cdr x))))

(define (null? x) (if (eqv? x '()) #t #f))

(define (map f xs)
  (if (null? xs)
      xs
      (cons (f (car xs)) (map f (cdr xs)))))

(define-macro let
  (lambda (args . body)
    (cons (append (cons 'lambda (cons (map car args) ())) body)
          (map cadr args))))

(define-macro and
  (lambda args
    (if (null? args)
        #t
        (if (null? (cdr args))
            (car args)
            (append (append (cons 'if (list (car args)))
                            (list (cons 'and (cdr args))))
                    '(#f))))))
