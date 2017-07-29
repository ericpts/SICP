(define nil '())

(define (make-node key value priority left-child right-child)
  (list (cons key value) priority left-child right-child))

(define MAX-PRIORITY 100)

(define (get-key node)
  (caar node))
(define (get-value node)
  (cdar node))
(define (get-priority node)
  (cadr node))
(define (get-left-child node)
  (caddr node))
(define (get-right-child node)
  (cadddr node))


(define (print-treap treap)
  (define (print-node node ident)
    (print ident
           "(key = " (get-key node)
           ", value = " (get-value node)
           ", priority = " (get-priority node)
           ")"))
  (define (print-treap-impl treap ident)
    (if (not (null? treap))
        (let ((next-ident (string-append ident "  ")))
          (print-node treap ident)
          (print-treap-impl (get-left-child treap) next-ident)
          (print-treap-impl (get-right-child treap) next-ident))
        (print ident "nil")))
  (print-treap-impl treap ""))

(define (treap-get treap key)
  (cond
   ((null? treap) nil)
   ((= key (get-key treap)) (get-value treap))
   ((< key (get-key treap)) (treap-get (get-left-child treap) key))
   ((> key (get-key treap)) (treap-get (get-right-child treap) key))
   (else (error "Invalid key comparison in GET"))))

(define (treap-insert treap key value)
  (define priority (random MAX-PRIORITY))
  ;;(print "Inserting " key " and " value " with priority " priority " into treap: ")
  ;;(print-treap treap)
  (define (insert-impl treap)
    (cond
     ((null? treap) (make-node key value priority nil nil))
     ((> priority (get-priority treap))
      (let ((node-splits (treap-split treap key)))
        (let ((new-left-treap (car node-splits))
              (new-right-treap (cdr node-splits)))
          (make-node key value priority new-left-treap new-right-treap))))
     ((< key (get-key treap))
      (make-node (get-key treap) (get-value treap) (get-priority treap) (insert-impl (get-left-child treap)) (get-right-child treap)))
     ((= key (get-key treap))
      (error "Inserting a duplicate key TREAP-INSERT"))
     ((> key (get-key treap))
      (make-node (get-key treap) (get-value treap) (get-priority treap) (get-left-child treap) (insert-impl (get-right-child treap))))
     (else (error "Invalid case TREAP-INSERT"))))
  (insert-impl treap))

;; returns (treap-with-smaller-or-equal-keys, treap-with-greater-keys)
(define (treap-split treap key)
  ;;(print "splitting on key " key " treap: ")
  ;;(print-treap treap)
  (cond
   ((null? treap) (cons nil nil))
   ((<= key (get-key treap))
    (let ((left-children (treap-split (get-left-child treap) key)))
      (let ((left-smaller-treap (car left-children))
            (left-greater-treap (cdr left-children)))
        (cons left-smaller-treap
              (make-node (get-key treap)
                         (get-value treap)
                         (get-priority treap)
                         left-greater-treap
                         (get-right-child treap))))))
   (else
    (let ((right-children (treap-split (get-right-child treap) key)))
      (let ((right-smaller-treap (car right-children))
            (right-greater-treap (cdr right-children)))
        (cons (make-node (get-key treap)
                         (get-value treap)
                         (get-priority treap)
                         (get-left-child treap)
                         right-smaller-treap)
              right-greater-treap))))))


(define (treap-join left-treap right-treap)
  ;;(print "joining treaps: ")
  ;;(print-treap left-treap)
  ;;(print "and ")
  ;;(print-treap right-treap)
  (cond
   ((null? left-treap) right-treap)
   ((null? right-treap) left-treap)

   ((> (get-priority left-treap) (get-priority right-treap))
    (make-node (get-key left-treap)
               (get-value left-treap)
               (get-priority left-treap)
               (get-left-child left-treap)
               (treap-join (get-right-child left-treap)
                           right-treap)))
   (else
    (make-node (get-key right-treap)
               (get-value right-treap)
               (get-priority right-treap)
               (treap-join left-treap
                           (get-left-child right-treap))
               (get-right-child right-treap)))))

(define treap nil)

(set! treap (treap-insert treap 10 'a))
(print (treap-get treap 10))

(set! treap (treap-insert treap 20 'b))
(print (treap-get treap 20))

(set! treap (treap-insert treap 30 'c))
(print (treap-get treap 30))

(set! treap (treap-insert treap 40 'd))
(print (treap-get treap 40))

(set! treap (treap-insert treap 50 'e))
(print (treap-get treap 50))
