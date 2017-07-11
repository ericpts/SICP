
(define (entry tree)
  (car tree))

(define (left-branch tree)
  (cadr tree))

(define (right-branch tree)
  (caddr tree))

(define (make-tree entry left right)
  (list entry left right))


(define (element-of-set? x set)
  (cond
    ((null? set) #f)
    ((= x (entry set)) #t)
    ((< x (entry set))
     (element-of-set?
       x
       (left-branch set)))
    ((> x (entry set))
     (element-of-set?
       x
       (right-branch set)))))


(define (adjoin-set x set)
  (cond
    ((null? set) (make-tree x '() '()))
    ((= x (entry set)) set)
    ((> x (entry set))
     (make-tree (entry set)
                (left-branch set)
                (adjoin-set x
                            (right-branch set))))
    ((< x (entry set))
     (make-tree (entry set)
                (adjoin-set x
                            (left-branch set))
                (left-branch set)))))




(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append
       (tree->list-1
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

(define null '())
(define (leaf x)
  (make-tree x null null))

(define tree-1
  (make-tree
    7
    (make-tree
      3
      (leaf 1)
      (leaf 5))
    (make-tree
      9
      null
      (leaf 11))))

(define tree-2
  (make-tree
    3
    (leaf 1)
    (make-tree
      7
      (leaf 5)
      (make-tree
        9
        null
        (leaf 11)))))

(define tree-3
  (make-tree
    5
    (make-tree
      3
      (leaf 1)
      null)
    (make-tree
      9
      (leaf 7)
      (leaf 11))))

(define (union-set set1 set2)
  (let ((list1 tree->list-1 set1)
        (list2 tree->list-2 set2))
    (list->tree (union-ordered-list list1 list2))))
