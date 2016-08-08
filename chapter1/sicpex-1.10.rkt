#lang planet neil/sicp
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))

(f 1)
(f 2)
(f 3)
(g 1)
(g 2)
(g 3)
(g 4)
(h 1)
(h 2)
(h 3)
(h 4)


;Ackermann is cool!
;(f n)=2n
;(g n)=(* 2 2 2 2 ... 2) 2^n

;(h n) 
;(A 2 n)
;(A (- 2 1) (A 2 (- n 1))
;(A 1 (A (- 2 1) (A 2 (- n 2))))
;(A 1 (A 1 (A 1 (A 1 (- n 3)))))
;so (n-1)(A 1 (A 1 1))
;(n-1)(A 1 (2))
;(n-2)(A 1 (A 1 2))
;2^2^2^2...^2 n '2' s