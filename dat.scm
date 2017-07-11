(define (sum-squared . vals)
  (apply
    +
    (map (lambda (x) * x x)
         vals)))

(print (sum-squared 1 2 3 4 5 6))
