(define tolerance 0.000001)
(define dx 0.000001)

(define (square x) (* x x))


(define (fixed-point f first-guess)
  (define (close-enough? x1 x2) (< (abs (- x1 x2)) tolerance))
  (define (iter guess)
    (print guess)
    (let ((next-guess (f guess)))
      (if (close-enough? guess next-guess)
        guess
        (iter next-guess))))
  (iter first-guess))


(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx))
          (f x))
       dx)))

(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newton-method f guess)
  (fixed-point (newton-transform f)
               guess))


(define (inc n) (+ 1 n))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (cond ((= n 0) (lambda (x) x))
        ((even? n) (let
                     ((ff (repeated f (/ n 2))))
                     (lambda (x)
                       (ff (ff x)))))
        (else (lambda (x)
                (f ((repeated f (- n 1)) x))))))

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (average-damp f)
  (lambda (x)
    (/ (+ x (f x))
       2)))

(define (fixed-point-of-transform f transform first-guess)
  (fixed-point (transform f) first-guess))

(define (log2 n)
  (define (try k)
    (if (> (expt 2 k)
           n)
      (- k 1)
      (try (+ k 1))))
  (try 0))

(define (nth-root n x)
  (fixed-point-of-transform (lambda (y)
                              (/ x
                                 (expt y (- n 1))))
                            (repeated average-damp (log2 n))
                            1))

; 8
; 16
(print (nth-root 64 19383341))

