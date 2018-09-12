#lang racket
;;Problem 1
"Problem 1"
;a
"a"
(define (list-sum l)
 (if (null? l)
     0
     (+ (car l) (list-sum (cdr l)))
 )
)
(list-sum '(1 2 3))
;returns 6
(list-sum '(1 2 3 4 5))
;returns 15


;b
"b"
(define (mean l)(/ (list-sum l) (length l)))
(mean '(1 2 3))
;returns 2
(mean '(1 2 3 4 5))
;returns 3

;c
"c"
(define (dev-square l)
 (define l-bar (mean l))
  (define (helper l)
      (map (lambda (x) (expt (- x l-bar) 2)) l)
 )
 (helper l)
)

(dev-square '(1 2 3 4 5))
;returns (4 1 0 1 4)
(dev-square '(6 7 8 9 10))
;returns (4 1 0 1 4)

;d
"d"
(define (std-dev l)(sqrt(mean (dev-square l))))
(std-dev '(1 2 3 4 5))
;returns 1.4142135623730951

;e
"e"
(define (map2 f x y) (map (lambda (x y) (f x y)) x y))
(map2 (lambda (x y) (* x y)) '(1 2 3 4 5) '(6 7 8 9 10))
;returns (6 14 24 36 50)))
(map2 (lambda (x y) (/ x y)) '(4 16 18 20) '(2 8 9 10))
;returns (2 2 2 2)

;f
"f"

(define (covariance x y)
 (define xbar (mean x))
 (define ybar (mean y))
  (define (helper x y)
    (if (null? (or y x))
         '()
         (cons (*(- (car x) xbar) (- (car y) ybar)) (helper (cdr x) (cdr y)))
    )
  )
 (helper x y)
)
(covariance '(1 2 3 4 5) '(6 7 8 9 10))
;returns (4 1 0 1 4) 
;g
"g"
(define (pearson-coeff x y)
 (if (not (equal? (length x) (length y)))
     "error"
     (/ (mean (covariance x y)) (* (std-dev x) (std-dev y)))
 )
)
(pearson-coeff '(1 2 3 4 5) '(6 7 8 9 10))
;returns .999999999998

;;Problem 2
"Problem 2"
;a
"a"
(define (ab_pair x y)
 (let* ((b (* (pearson-coeff x y)(/ (std-dev y) (std-dev x))))
        (a (- (mean y)(* b (mean x)))))
        (cons a b)
 )
)

(ab_pair '(1 2 3) '(4 5 6))
;returns (3.0 . 1.0)
(ab_pair '(1 5 9) '(7 5 3))
;returns (-15.0 . 4.0)

;b
"b"
(define (best-fit-fn px py)
 (let ((a (car (ab_pair px py)))
       (b (cdr (ab_pair px py))))
  (lambda (x) (+ a (* x b)))
 )
)
(define X '(160 180 200 220 240 260 280))
(define Y '(126 103 82 75 78 40 20))

((best-fit-fn X Y)5)
(define fitline (best-fit-fn X Y))

;c
"c"
(define (zip2 a b)(map2 vector a b))
(zip2 '(1 2 3) '(4 5 6))
;returns (#(1 4) #(2 5) #(3 6))
(zip2 '(2 4 9) '(8 1 6))
;returns (#(2 8) #(4 1) #(9 6))

(require plot)
(plot-new-window? #t)
(plot (list (axes)
            (function fitline 140 300)
            (points (zip2 X Y))))
;;Problem 3
"Problem 3"
(define (merge list1 list2)
 (cond ((null? list1) (list (car list2)))
       ((null? list2) (list (car list1)))
       ((null? (and list1 list2)) '())
       (else
         (if (>= (car list1) (car list2))
              (cons (car list2) (merge list1 (cdr list2)))
              (cons (car list1) (merge (cdr list1) list2))
          )
        )
 )
)
(define L1 (sort '(15 6 7 2 11 66 94 45 20) <))
(define L2 (sort '(10 16 8 49 36 32 58 91 50) <))
(merge L1 L2)
;returns (2 6 7 8 10 11 15 16 20 32 36 45 49 50 58 66 91 94)