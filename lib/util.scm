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

(define (list? xs)
  (if (null? xs)
      #t
      (if (pair? xs)
          (list? (cdr xs))
          #f)))

(define (helper-compare f xs)
  (if (or (null? xs) (null? (cdr xs)))
      'undefined
      (let ((n (car xs)) (m (cadr xs)))
        (if (null? (cddr xs))
          (or (= n m) (f n m))
          (and (or (= n m) (f n m))
               (helper-compare f (cdr xs)))))))

(define (<= . xs)
  (helper-compare '< xs))

(define (>= . xs)
  (helper-compare '> xs))

(define (length xs)
  (if (null? xs)
      0
      (+ 1 (length (cdr xs)))))

(define (last xs)
  (cond ((null? xs) 'undefined)
        ((null? (cdr xs)) (car xs))
        (else (last (cdr xs)))))

(define (even? x)
  (if (= x 0) #t (odd? (- x 1))))
(define (odd? x)
  (if (= x 0) #f (even? (- x 1))))

(define (neq? x y) (not (eq? x y)))
