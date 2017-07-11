(define (my-memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (my-memq item (cdr x)))))

(define (my-equal? xs ys)
  (cond
    ((and (null? xs) (null? ys)) #t)
    ((or (null? xs) (null? ys)) #f)
    (else
      (let ((x (car xs))
            (y (car ys)))
        (cond
          ((and (pair? x) (pair? y))
           (and (my-equal? x y)
                (my-equal? (cdr xs) (cdr ys))))
          ((and (integer? x) (integer? y))
           (and (= x y)
                (my-equal? (cdr xs) (cdr ys))))
          ((and (symbol? x) (symbol? y))
           (and (eq? x y)
                (my-equal? (cdr xs) (cdr ys))))
          (else #f))))))


(print (my-equal? '(this is 'a list)
                  '(this is a list)))
