(define (attach-tag type-tag datum)
  (cons type-tag datum))

(define (type-tag datum)
  (car datum))

(define (contents datum)
  (cadr datum))

(define (variable? e) (symbol? e))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp)
       (= exp num)))

(define (install-sum-package)
  ;; internal procedures
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
  (define (addend s)
    (cadr s))
  (define (augend s)
    (apply
      make-sum
      (cddr s)))
  (define (deriv sum)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  ;; exported procedures
  (put 'make '+ make-sum)
  (put 'deriv '+ deriv))


(define (install-prod-package)
  ;; internal procedures
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

  (define (multiplier p)
    (cadr p))

  (define (multiplicand p)
    (apply
      make-product
      (cddr p)))

  (define (deriv prod)
    (make-sum
      (make-product
        (multiplier exp)
        (deriv (multiplicand exp) var))
      (make-product
        (multiplicand exp)
        (deriv (multiplier exp) var))))
  ;; exported procedures
  (put 'make '* make-product)
  (put 'deriv '* deriv))

(define (install-exp-package)
  ;; internal procedures
  (define (make-exponentiation e p)
    (cond ((=number? p 0) 1)
          ((=number? p 1) e)
          (else (list '** e p))))
  (define (base e)
    (cadr e))
  (define (exponent e)
    (caddr e))
  (define (deriv exp)
    ((get 'make '*)
     (exponent exp)
     (make-exponentiation
       (base exp)
       (- (exponent exp)
          1))
     (deriv (base exp) var)))
  ;; exported procedures
  (put 'make '** make-exponentiation)
  (put 'deriv '** deriv))



(define (deriv exp var)
  (print "deriving: " exp)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var)
           1
           0))
        (else ((get 'deriv (operator exp))
               (operands exp)
               var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(print (deriv '(* x y (+ x 3)) 'x))
