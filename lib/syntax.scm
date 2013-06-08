(define-macro let
  (lambda (args . body)
    `((lambda ,(map car args) ,@body) ,@(map cadr args))))

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
