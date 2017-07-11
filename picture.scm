#lang racket/gui

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))


(define (split big-comb small-comb)
  (define (iter image n)
    (if (= n 0)
        image
        (let ((smaller (iter image (- n 1))))
          (big-comb image
                    (small-comb smaller smaller)))))
  (lambda (image n)
    (iter image n)))


(define right-split (split beside below))
(define up-split (split below beside))


(define (corner-split image n)
  (if (= n 0)
      image
      (let ((up-smaller (up-split image (- n 1)))
            (corner-smaller (corner-split image (- n 1)))
            (right-smaller (right-split image (- n 1))))
        (below
         (beside
          image
          (below right-smaller right-smaller))
         (beside
          (beside up-smaller up-smaller)
          corner-smaller)))))

(define (square-limit image n)
  (let ((quartet (corner-split image n)))
    (let ((half (beside (flip-horiz quartet)
                        quartet)))
      (below (flip-vert quartet) quartet))))

(define (square-of-four tl tr bl br)
  (lambda (image)
    (let ((up (beside (tl image)
                      (tr image)))
          (down (beside (bl image)
                        (br image))))
      (below down up))))

(define (flipped-pairs image)
  ((square-of-four identity
                  flip-vert
                  identity
                  flip-vert)
   image))


(define frame-painter 
(let ((segments (list (make-segment (make-vect 0 0)
                                      (make-vect 0 1))
                        (make-segment (make-vect 0 0)
                                      (make-vect 1 0))
                        (make-segment (make-vect 1 0)
                                      (make-vect 1 1))
                        (make-segment (make-vect 0 1)
                                      (make-vect 1 1)))))
  (segments->painter segments)))

(define x-painter
  (let ((segments (list (make-segment (make-vect 0 0)
                                      (make-vect 1 1))
                        (make-segment (make-vect 0 1)
                                      (make-vect 1 0)))))
    (segments->painter segments)))

(define diamond-painter
  (let ((segments (list (make-segment (make-vect 0.5 0)
                                      (make-vect 0 0.5))
                        (make-segment (make-vect 0.5 0)
                                      (make-vect 1 0.5))
                        (make-segment (make-vect 0 0.5)
                                      (make-vect 0.5 1))
                        (make-segment (make-vect 1 0.5)
                                      (make-vect 0.5 1)))))
    (segments->painter segments)))

(paint diamond-painter)

(paint frame-painter)
(paint x-painter)                 