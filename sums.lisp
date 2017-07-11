
(define (inc n) (+ n 1))


(define (sum term a next b)
  (define (iter at acum)
    (if (> at b)
      acum
      (iter (next at) (+ acum (term at)))))
  (iter a 0))

(define (product term a next b)
  (define (iter at acum)
    (if (> at b)
      acum
      (iter (next at) (* acum (term at)))))
  (iter a 1))

(define (id x) x)

(define (aprox-pi n)
  (define (up-term k) (* 2 (+ 1 (floor (/ k 2)))))
  (define up (product up-term 1 inc n))
  (define (down-term k) (- (up-term (inc k)) 1))
  (define down (product down-term 1 inc n))
  (* 4.0 (/ up down)))

(print (aprox-pi 4))



(define (factorial n)
  (product id 1 inc n))

(print (factorial 10))

(define (cube x) (* x x x))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (define (area x) (/ (+ (f x)
                         (f (+ x dx)))
                      2.0))
  (* (sum area a add-dx b)
     dx))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (term k)
    (define raw (f (+ a (* k h))))
    (* raw (cond ((or (= 0 k)
                      (= n k)) 1)
                 ((odd? k) 4)
                 (else 2))))
  (* (sum term 0 inc n)
     (/ h 3)))

(print (integral cube 0 1 0.001))

(print (simpson cube 0 1 100))

