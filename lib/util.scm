(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))

(define (snoc xs x)
  (append xs (list x)))

(define (reverse xs)
  (if (null? xs)
      '()
      (snoc (reverse (cdr xs)) (car xs))))


(define (not x) (if x #f #t))

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
  (if (null xs)
      'undefined
      (if (null? (cdr xs))
          (car xs)
          (last (cdr xs)))))

(define (even? x)
  (if (= x 0) #t (odd? (- x 1))))
(define (odd? x)
  (if (= x 0) #f (even? (- x 1))))

(define (neq? x y) (not (eq? x y)))
