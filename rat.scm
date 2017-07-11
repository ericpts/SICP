(define (sign x)
  (if (>= 0 x)
    1
    -1))

(define (make-rat n d)
  (let ((g (gcd n d))
        (s (* (sign n)
              (sign d))))
    (cons (* s
             (abs (/ n g)))
          (abs (/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
			   (* (numer y) (denom x)))
			(* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
			(* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
			(* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
	 (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-segment beg end)
  (cons beg end))

(define (start-segment x)
  (car x))

(define (end-segment x)
  (cdr x))

(define (make-point x y)
  (cons x y))

(define (x-point x)
  (car x))

(define (y-point x)
  (cdr x))

(define (average x y)
  (/ (+ x y)
     2))


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment seg)
  (let ((beg (start-segment seg))
        (end (end-segment seg)))
    (make-point (average (x-point beg)
                         (x-point end))
                (average (y-point beg)
                         (y-point end)))))


(define (make-rectangle x y)
  (cons x y))

(define (low-left rect)
  (car rect))

(define (high-right rect)
  (cdr rect))

(define (high-x rect)
  (x-point (high-right rect)))

(define (low-x rect)
  (x-point (low-left rect)))

(define (high-y rect)
  (y-point (high-right rect)))

(define (low-y rect)
  (y-point (low-left rect)))

(define (delta-x rect)
  (abs (- (high-x rect) (low-x rect))))

(define (delta-y rect)
  (abs (- (high-y rect) (low-y rect))))

(define (perimeter rect)
  (+ (delta-x rect) (delta-y rect)))

(define (area rect)
  (* (delta-x rect) (delta-y rect)))

(define (make-center-percent c p)
  (make-center-width c (* c p)))

(define (percent i)
  (/ (width i)
     (center i)))

