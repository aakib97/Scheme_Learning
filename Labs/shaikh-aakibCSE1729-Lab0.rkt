#lang racket
;;;Currency Conversion
;A function d2e(dollars to euro) is introduced with one argument,dollars to creat a conversion
(define (d2e dollars) (* dollars .93))
;A function e2y(euros to yen) is introduced with one argument,euros to creat a conversion
(define (e2y euro) (* euro (/ 121 73)))
;A function d2y(dollars to yen) is introduced with one argument,
;dollars to creat a conversion using the cpnversion of dollars to euro so it can be used with the euto to yen conversion
(define (d2y dollars) (* (d2e dollars) (/ 121 73)))

;;;Matrix determinant
;A function det is introduced with 4 arguments that calculate the determinant
(define (det a b c d)
  (- (* a d) (* c b) )
)
;Checks if the determine is not equal to 0 in order to learn whether or not its invertible
(if (not (= (det -3 1 2 7) 0)) "True, Matrix N is invertible" "false, Matrix N is 0")

(if (not (= (det 2 -4 -6 12) 0)) "True, Matrix M is invertible" "false, Matrix M is 0")
;A function with six arguments that implement the pervious det funcation to calucalte the derminant for a 3x3 matrix
(define (det3x3 a b c
                d e f
                g h i)
 (+ (- (* a (det e f h i) ) (* b (det d f g i) ) (* c (det d e g h) ) ) )
)
