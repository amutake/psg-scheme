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
    (if (pair? args)
        `((lambda ,(map car args) ,@body) ,@(map cadr args))
        `(letrec ((,args (lambda ,(map car (car body)) ,@(cdr body))))
           (,args ,@(map cadr (car body)))))))

(define-macro and
  (lambda args
    (if (null? args)
        #t
        (if (null? (cdr args))
            (car args)
            `(if ,(car args) (and ,@(cdr args)) #f)))))

(define-macro or
  (lambda args
    (if (null? args)
        #f
        (if (null? (cdr args))
            (car args)
            `(let ((+value+ ,(car args)))
               (if +value+ +value+ (or ,@(cdr args))))))))

(define-macro let*
  (lambda (args . body)
    (if (null? (cdr args))
        `(let (,(car args)) ,@body)
        `(let (,(car args)) (let* ,(cdr args) ,@body)))))

(define map-2
  (lambda (fn xs ys)
    (if (null? xs)
        '()
        (cons (fn (car xs) (car ys)) (map-2 fn (cdr xs) (cdr ys))))))

(define-macro letrec
  (lambda (args . body)
    (let ((vars (map car args))
          (vals (map cadr args)))
      `(let ,(map (lambda (x) `(,x 'undefined)) vars)
         ,@(map-2 (lambda (x y) `(set! ,x ,y)) vars vals)
         ,@body))))

(define-macro cond
  (lambda args
    (if (null? args)
        'undefined
        (if (eq? (caar args) 'else)
            `(begin ,@(cdar args))
            (if (null? (cdar args))
                `(let ((+value+ ,(caar args)))
                   (if +value+ +value+ (cond ,@(cdr args))))
                `(if ,(caar args)
                     (begin ,@(cdar args))
                     (cond ,@(cdr args))))))))

(define-macro do
  (lambda (var-form test-form . args)
    (let ((vars (map car var-form))
          (vals (map cadr var-form))
          (step (map cddr var-form)))
      `(letrec ((loop (lambda ,vars
                        (if ,(car test-form)
                            (begin ,@(cdr test-form))
                            (begin
                              ,@args
                              (loop ,@(map-2 (lambda (x y)
                                               (if (null? x) y (car x)))
                                             step
                                             vars)))))))
         (loop ,@vals)))))
