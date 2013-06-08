(define-macro let
  (lambda (args . body)
    `((lambda ,(map car args) ,@body) ,@(map cadr args))))
