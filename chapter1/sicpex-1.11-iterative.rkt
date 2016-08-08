#lang planet neil/sicp
(define (f-iter i a b c)
  (if (= i 2)
      c
      (f-iter (- i 1) b c (+ c (* 2 b) (* 3 a)))))
(define (f n)
  (if (or (= n 0) (= n 1) (= n 2))
      n
      (f-iter n 0 1 2)))

(f 3)
(f 4)
(f 5)