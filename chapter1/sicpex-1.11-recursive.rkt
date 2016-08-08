#lang planet neil/sicp
(define (f n)
  (if (or (= n 0) (= n 1) (= n 2))
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))
(f 3)
(f 4)
(f 5)
