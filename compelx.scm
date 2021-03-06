(define (attach-tag type-tag datum)
  (cons type-tag datum))

(define (type-tag datum)
  (car datum))

(define (contents datum)
  (cadr datum))


(define (install-rectangular-package)
  ;; internal
  (define (real-part z) (car z))
  (define (imag-part z) (cadr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (tag x)
    (attach-tag 'rectangular x))
  ;; exported
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic-op op . args)
  (let ((args-types (map type-tag args)))
    (let ((proc (get op args-types)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method found for these types:
          APPLY-GENERIC"
          (list op type-tags))))))


(define (real-part z)
  (apply-generic-op 'real-part z))

(define (imag-part z)
  (apply-generic-op 'imag-part z))

(define (magnitude z)
  (apply-generic-op 'magnitude z))

(define (angle z)
  (apply-generic-op 'angle z))


(define (make-from-real-imag x y)
  ((get 'make-from-real-imag
        'rectangular )
   x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang
        'polar)
   r a))


