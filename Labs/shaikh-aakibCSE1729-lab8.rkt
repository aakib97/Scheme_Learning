#lang racket
;;Problem 1
"Problem 1"
(define (make-tree value left right)(list value left right))
(define (value T) (car T))
(define (right T) (caddr T))
(define (left T) (cadr T))
(define (insert x T)
 (cond ((null? T) (make-tree x '() '()))
       ((equal? x (value T)) T)
       ((< x (value T)) (make-tree (value T)(insert x (left T))(right T)))
       ((> x (value T)) (make-tree (value T)(left T)(insert x (right T))))
 )
)
(define (tree-depth n)
 (cond ((null? n) 0)
       ((and (null? (left n)) (null? (right n))) 0)
       (else
         (let* ((left-child (tree-depth (left n)))
                (right-child (tree-depth (right n)))
                (children-depth (max left-child right-child)))
                (+ 1 children-depth)
         )
       )
 )
)

(tree-depth (make-tree 5 (make-tree 2 '() '()) (make-tree 7 '() '())))
;returns 1
(tree-depth (make-tree 5 (make-tree 2 (make-tree 1 '() '()) '()) (make-tree 7 '() '())))
;returns 2

;;Problem 2
"Problem 2"
(define (occurences-in-tree x T)
 (cond ((null? T) 0)
       ((= x (value T)) (+ 1 
                            (occurences-in-tree x (left T))
                            (occurences-in-tree x (right T))))
       ((< x (value T)) (+ 0 (occurences-in-tree x (left T))))
       ((> x (value T)) (+ 0 (occurences-in-tree x (right T))))
 )
)


(define tree (make-tree 5 (make-tree 2 (make-tree 1 '() '()) '()) (make-tree 7 '() '())))
(occurences-in-tree 5 tree)
;returns 1
(occurences-in-tree 7 tree)
;returns 1

;;Problem 3
"Problem 3"
(define (count-one-child n)
 (cond ((null? n) 0)
       ((and (null? (left n)) (null? (right n)))
             (+ (count-one-child (left n)) (count-one-child (right n))))
      ((xor (not (null? (left n))) (not (null? (right n))))
             (+ 1 (count-one-child (left n)) (count-one-child (right n))))
      ((and (not (null? (left n))) (not (null? (right n))))
             (+ (count-one-child (left n)) (count-one-child (right n))))
 )
)
(define tree1 (make-tree 1 (make-tree 2 (make-tree 3 '() '())'()) (make-tree 4 '() '())))
(count-one-child tree1)
;returns 1 being 2
(define tree2 (make-tree 1 (make-tree 2 (make-tree 3 '() '())'()) (make-tree 4 (make-tree 5'() '())'())))
(count-one-child tree2)
;returns 2 being 2 and 4

;;Problem 4
"Problem 4"
(define (invert-bst T)
 (if (null? T)
     '()
     (make-tree (value T)
                (invert-bst (left (make-tree (value T) (right T) (left T))))
                (invert-bst (right (make-tree (value T) (right T) (left T))))
     )
 )
)

(invert-bst (make-tree 20 (make-tree 10 '() '()) (make-tree 30 '() '())))
;returns (20 (30 () ()) (10 () ())) invert of (20 (10 () ()) (30 () ()))

;;Problem 5
"Problem 5"
(define (element? x T)
 (cond ((null? T) #f)
       ((eq? x (value T)) #t)
       ((< x (value T)) (element? x (left T)))
       ((> x (value T)) (element? x (right T)))
 )
)

(define (values-greaterthan z T)
 (cond ((null? T) 0)
       ((>= (value T) z)
              (+ 1 (values-greaterthan z (left T)) (values-greaterthan z (right T))))
       ((< (value T) z)
             (+ 0 (values-greaterthan z (left T)) (values-greaterthan z (right T))))
 )
)

(define tree3
  (make-tree 8 (make-tree 4
               (make-tree 1 '() '()) (make-tree 5 '() '()))
               (make-tree 11 (make-tree 21 '() '()) '()))
)
(values-greaterthan 5 tree3)
;returns 4 being 8, 5, 11 and 21
(define tree4
  (make-tree 6
  (make-tree 5 (make-tree 2 '() '())
  (make-tree 9 '() '()))(make-tree 11 (make-tree 10 '() '())
  (make-tree 15 '() '())))
)
(values-greaterthan 6 tree4)
;returns 5 being 6, 9, 11, 10, and 15


