(use srfi-1)


(define (variable? e) (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp)
       (= exp num)))

(define (make-sum . e)
  (let ((nums (filter number? e))
        (rest (filter (lambda (x) (not (number? x)))
                      e)))
    (cond
      ((= (length e) 1) (car e))
      ((= (length nums) 0) (apply list '+ rest))
      (else
        (let ((prod (foldl + 0 nums)))
          (cond
            ((= (length rest) 0) prod)
            ((= prod 0) (apply make-sum rest))
            (else (apply list '+ prod rest))))))))

(define (sum? e)
  (and (pair? e)
       (eq? (car e)
            '+)))

(define (addend s)
  (cadr s))

(define (augend s)
  (apply
    make-sum
    (cddr s)))

(define (make-product . e)
  (let ((nums (filter number? e))
        (rest (filter (lambda (x) (not (number? x)))
                      e)))
    (cond
      ((= (length e) 1) (car e))
      ((= (length nums) 0) (apply list '* rest))
      (else
        (let ((prod (foldl * 1 nums)))
          (cond
            ((= (length rest) 0) prod)
            ((= prod 0) 0)
            ((= prod 1) (apply make-product rest))
            (else (apply list '* prod rest))))))))

(define (product? e)
  (and (pair? e)
       (eq? (car e)
            '*)))

(define (multiplier p)
  (cadr p))

(define (multiplicand p)
  (apply
    make-product
    (cddr p)))

(define (make-exponentiation e p)
  (cond ((=number? p 0) 1)
        ((=number? p 1) e)
        (else (list '** e p))))

(define (exponentiation? e)
  (and (pair? e)
       (eq? '**
            (car e))))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
           1
           0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product
                     (multiplier exp)
                     (deriv (multiplicand exp) var))
                   (make-product
                     (multiplicand exp)
                     (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product
           (exponent exp)
           (make-exponentiation
             (base exp)
             (- (exponent exp)
                1))
           (deriv (base exp) var)))
        (else (error "unknown expression
        type: DERIV" exp))))


; (print (deriv '(+ 3 (* x x)) 'x))

(print (deriv '(* x y (+ x 3)) 'x))
