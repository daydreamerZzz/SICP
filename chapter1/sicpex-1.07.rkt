#lang racket
(define (good-enough? x y)
  (< (/ (abs (- x y)) y) 0.00001))
(define (abs x)
  (if (< x 0)
      (- 0 x)
      x))
(define (sqrt-iter last-guess guess x)
  (if (good-enough? guess last-guess)
      guess
      (sqrt-iter guess (improve guess x) x)))
(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))
(define (square-root x)
  (sqrt-iter 0.5 1.0 x))

(square-root 4)

(square-root 400000000000000)