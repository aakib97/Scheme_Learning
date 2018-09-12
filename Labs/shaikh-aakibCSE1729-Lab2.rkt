#lang racket

(define (ancestors n)
  (if (= n 1)
      2
      (* 2 (ancestors (- n 1)))
  )
)

(define (num-ancestors n) (+(ancestors n) (ancestors (- n 1))))



(define ( pi-approx k )
  ;The definition of factorial from the lecture slides
  (define ( factorial k )
    (if (= k 0)
        1
        (* k ( factorial (- k 1)))
    )
  )
  (define ( pi-aux k )
    (if (- k 0)
     1
    (+ (/ (*(factorial (* 4 k))(+ 1103 (* 26390 k))) (*(expt (factorial k) 4)(expt 369 (* 4 k)))) (pi-aux (- k 1)))
    )
  )
  (/ 1 (* (/ (* 2 ( sqrt 2))9801) ( pi-aux (- k 1))))
)

(define (new-sqrt x n)
  (if (= n 1)
      (+ 1 (- x 1))
      (+ 1 (/(- x 1) (+ 2 (/(- x 1) (new-sqrt x (- n 1))))))
  )
)

(define (pell-num n)
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+(* 2 (pell-num (- n 1))) (pell-num (- n 2)))
      )
  )
)

(define (comp-pell-num n)
  (if (= n 0)
      2
      (if (= n 1)
          2
          (+(* 2 (comp-pell-num (- n 1))) (comp-pell-num (- n 2)))
      )
  )
)

(define (root2-approx n) (/(/ 2 (comp-pell-num n)) (pell-num n)))

(root2-approx 6)

