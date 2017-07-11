(define (flatmap proc items )
  (apply append (map proc items)))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))


(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-impl bits branch)
    (if (null? bits)
      '()
      (let ((next-branch (choose-branch (car bits) branch)))
        (if
          (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-impl (cdr bits) tree))
          (decode-impl (cdr bits) next-branch)))))
  (decode-impl bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))


(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define (choose-bit symbol branch)
  (let ((left (left-branch branch))
        (right (right-branch branch)))
    (cond
      ((memq symbol (symbols left))
       0)
      ((memq symbol (symbols right))
       1)
      (else (error "choose-bit: symbol not in any branch" symbol)))))

(define (encode-symbol symbol branch)
  (if (leaf? branch)
    '()
    (let ((bit (choose-bit symbol branch)))
      (cons bit
            (encode-symbol symbol (choose-branch bit branch))))))

(define (encode word tree)
  (flatmap (lambda (symbol) (encode-symbol symbol tree))
           word))

(define text (decode sample-message sample-tree))

(define (successive-merge leaves)
  (if (= (length leaves) 1)
      (car leaves)
      (successive-merge
        (adjoin-set (make-code-tree
                      (car leaves)
                      (cadr leaves))
                    (cddr leaves)))))

(define (generate-huffman-tree pairs)
  (successive-merge
    (make-leaf-set pairs)))

(define rock-tree
  (generate-huffman-tree
    '((a 2)
      (na 16)
      (boom 1)
      (sha 3)
      (get 2)
      (yip 9)
      (job 2)
      (wah 1))))

(define rock-lyrics
	'(get a job
		  sha na na na na na na na na

		  get a job
		  sha na na na na na na na na

		  wah yip yip yip yip
		  yip yip yip yip yip
		  sha boom))

(define rock-encode
  (encode rock-lyrics rock-tree))

(print (decode rock-encode rock-tree))
