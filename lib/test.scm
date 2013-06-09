(+ (reset (* (shift (lambda (k) (+ 1 (k 3)))) 4)) 5)

(define (even? x)
  (if (= x 0)
      #t
      (odd? (- x 1))))
(define (odd? x)
  (if (= x 0)
      #f
      (even? (- x 1))))
