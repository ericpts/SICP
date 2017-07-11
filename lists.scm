(use srfi-1)


(define (last-pair items)
  (let ((head (car items))
        (tail (cdr items)))
    (if (null? tail)
      head
      (last-pair tail))))

(define (same-parity head . tail)
  (define (good? n)
    (= (remainder n 2)
       (remainder head 2)))
  (cons head (filter good? tail)))

(define (heads lists)
  (if (null? lists)
    lists
    (cons (caar lists)
          (heads (cdr lists)))))

(define (tails lists)
  (if (null? lists)
    lists
    (cons (cdar lists)
          (tails (cdr lists)))))

(define (my-map fun . lists)
  (if (null? (car lists))
    '()
    (cons (apply fun (heads lists))
          (apply my-map fun (tails lists)))))

(define (square x) (* x x))

(define (single? x) (not (pair? x)))

(define nil '())

(define (deep-reverse items)
  (cond ((null? items) nil)
        ((single? items) items)
        (else (append (deep-reverse (cdr items))
                      (list (deep-reverse (car items)))))))

(define (fringe tree)
  (cond ((null? tree) nil)
        ((single? tree) (list tree))
        (else (append
                (fringe (car tree))
                (fringe (cdr tree))))))


(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (if (single? mobile)
    mobile
    (+ (total-weight
         (branch-structure (left-branch mobile)))
       (total-weight
         (branch-structure (right-branch mobile))))))

(define (balanced? mobile)
  (define (branch-torque branch)
    (* (branch-length branch)
       (total-weight (branch-structure branch))))
  (= (branch-torque (left-branch branch))
     (branch-torque (right-branch branch))))


(define (square-tree tree)
  (cond ((null? tree) tree)
        ((single? tree) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))



(define (square-tree-map tree)
  (map (lambda (subtree)
         (if (single? subtree)
           (square subtree)
           (square-tree-map subtree)))
       tree))

(define (tree-map fun tree)
  (cond ((null? tree) nil)
        ((single? tree) (fun tree))
        (else (cons (tree-map fun (car tree))
                    (tree-map fun (cdr tree))))))

(define (square-tree tree)
  (tree-map square tree))


(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest-subsets (subsets (cdr s))))
      (append rest-subsets
              (map (lambda (x)
                     (cons (car s) x))
                   rest-subsets)))))


(define (accumulate op init seq)
  (if (null? seq)
    init
    (op (car seq)
        (accumulate op init (cdr seq)))))


(define (my-map p seq)
  (accumulate (lambda (x y)
                (cons (p x) y))
              nil
              seq))


(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0
              sequence))

(define (horner-eval x coefficient-sequence)
  (accumulate
    (lambda (this-coeff higher-terms)
      (+ this-coeff
         (* x higher-terms)))
    0
    coefficient-sequence))


(define (count-leaves t)
  (accumulate + 0 (map count-leaves t)))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    init
    (op (accumulate op init (map car seqs))
        (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define m
  (list (list 1 2 3 4)
        (list 4 5 6 6)
        (list 6 7 8 9)))

(define v (list 1 1 1 1))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix mat1 mat2)
  (let ((cols (transpose mat2)))
    (map (lambda (r) (matrix-*-vector cols r))
         mat1)))

(define (fold-left fun init seq)
  (define (iter result rem)
    (if (null? rem)
      result
      (iter (fun result (car rem))
            (cdr rem))))
  (iter init seq))

(define (enumerate-interval lo hi)
  (if (< hi lo)
    nil
    (cons lo (enumerate-interval (+ lo 1) hi))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime? n)
  (define (iter d)
    (cond ((= 0 (remainder n d)) #f)
          ((> d (sqrt n)) #t)
          (else (iter (+ d 1)))))
  (iter 2))

(define (prime-sum-pairs n)
  (define (prime-sum? pair)
    (prime? (+ (car pair)
               (cadr pair))))
  (define (make-pair-sum pair)
    (append pair (list (apply + pair))))
  (define (all-pairs n)
            (flatmap
              (lambda (i)
                (map (lambda (j) (list i j))
                     (enumerate-interval 1 (- i 1))))
              (enumerate-interval 1 n)))

  (map
    make-pair-sum
    (filter prime-sum? (all-pairs n))))

(define (permutations s)
  (if (null? s) (list nil)
    (flatmap
      (lambda (x)
        (map (lambda (perm)
               (cons x perm))
             (permutations
               (remove (lambda (y)
                         (= y x))
                       s))))
      s)))

  (define (all-triplets n)
    (flatmap
      (lambda (i)
        (flatmap
          (lambda (j)
            (map (lambda (k)
                   (list i j k))
                 (enumerate-interval 1 n)))
          (enumerate-interval 1 n)))
      (enumerate-interval 1 n)))


(define (triplet-sum n s)
  (define (good-sum? triplet)
    (= s (apply + triplet)))
  (define (summed-triplet triplet)
    (append triplet (list (apply + triplet))))
  (map
    summed-triplet
    (filter good-sum? (all-triplets n))))

(define (any? pred seq)
  (cond ((null? seq) #t)
        ((pred (car seq)) #f)
        (else (any? pred (cdr seq)))))

(define (make-queen row col)
  (list row col))

(define (get-row queen)
  (car queen))

(define (get-col queen)
  (cadr queen))

(define (get-diag-1 queen)
  (- (get-row queen) (get-col queen)))

(define (get-diag-2 queen)
  (+ (get-row queen) (get-col queen)))

(define (check? q1 q2)
  (or
    (= (get-row q1)
       (get-row q2))
    (= (get-col q1)
       (get-col q2))
    (= (get-diag-1 q1)
       (get-diag-1 q2))
    (= (get-diag-2 q1)
       (get-diag-2 q2))))

(define (safe? positions)
  (any?
    (lambda (pair) (apply check? pair))
    (map (lambda (other-queen)
           (list (car positions) other-queen))
         (cdr positions))))

(define empty-board nil)

(define (adjoin-position row col rest-of-queens)
  (cons (make-queen row col) rest-of-queens))

(define (queens board-size)
  (define (queens-iter k)
    (if (= k 0)
      (list empty-board)
      (filter
        safe?
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                     new-row
                     k
                     rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queens-iter (- k 1))))))
  (queens-iter board-size))

(print (length (queens 8)))
