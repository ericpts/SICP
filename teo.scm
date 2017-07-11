(use srfi-1)

(define (!= x y) (not (= x y)))

(define (digits n)
  (if (< n 10)
    (list n)
    (cons (remainder n 10)
          (digits (quotient n 10)))))

(define (same-digits x y)
  (let ((xd (digits x))
        (yd (digits y)))
    (list= =
           (sort xd <)
           (sort yd <))))

(define (solve x y)
  (if (same-digits x y)
    (list x y)
    (solve (+ 1 x)
           (+ 1 y))))

(define (next-pair x y)
  (solve (+ 1 x)(+ 1 y)))

(define (show-first-n x y n)
  (if (!= n 0)
  (let ((next (next-pair x y)))
    (print next)
    (show-first-n (car next) (cadr next) (- n 1)))))


(define coef 18771399)
(print "coef is " coef)

(define d (* 9 coef))
(print "d is " d)

(define p (length (digits d)))

(define x (+ coef (expt 10 p)))
(print "start from " x)
(show-first-n x (+ x d) 10)

