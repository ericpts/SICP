(define (attach-tag tag datum)
  (cons tag datum))
(define (type-tag datum)
  (car datum))
(define (contents datum)
  (cdr datum))

(define (deriv exp var)
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

(define (install-sum-package)
  ;; internal procedures
  (define (make-sum x y)
    (list x y))
  (define (addend data)
    (car data))
  (define (augend data)
    (cadr data))
  (define (deriv-sum sum var)
    ((get 'make '+)
     (deriv (addend sum) var)
     (deriv (augend sum) var)))

  ;; interface
  (define (tag data)
    (attach-tag '+ data))

  (put 'deriv '+ deriv-sum)
  (put 'make '+
       (lambda (x y)
         (tag (make-sum x y))))
  'done)

(define (install-product-package)
  ;; internal procedures
  (define (make-product x y)
    (list x y))
  (define (multiplicator prod)
    (car prod))
  (define (multiplicand prod)
    (cadr prod))
  (define (deriv-product exp var)
    ((get 'make '+)
     ((get 'make '*) (deriv (multiplicator exp) var) (multiplicand exp))
     ((get 'make '*) (deriv (multiplicand exp) var) (multiplicator exp))))

  ;; interface
  (define (tag data)
    (attach-tag '* data))

  (put 'deriv '* deriv-product)
  (put 'make '*
       (lambda (x y)
         (tag (make-prod x y))))

  'done)

;; This installs exponentiations to natural numbers.
(define (install-exponentiation-package)
  ;;internal procedures
  (define (make-exponentation exp n)
    (list exp n))

  (define (base exp)
    (car exp))
  (define (exponent exp)
    (cadr exp))

  (define (deriv-exponentiation exp var)
    ((get 'make '*)
      (exponent exp)
      ((get 'make '*)
        ((get 'make '^)
          (base exp)
          (- (exponent exp)
             1))
        (deriv (base exp) var))))

  ;; interface
  (define (tag data)
    (attach-tag '^ data))

  (put 'deriv '^ deriv-exponentiation)
  (put 'make '^
       (lambda (x y)
         (tag (make-exponentiation x y))))
  'done)

