#lang planet neil/sicp
(define (rec-fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (rec-fast-exp b (/ n 2))))
        (else (* b (rec-fast-exp b (- n 1))))))
(define (square x) (* x x))
(define (even? n)
  (= (remainder n 2) 0))


(define (fast-exp-iter base result exponent)
  (cond ((= exponent 0) result)
        ((even? exponent) (fast-exp-iter (square base) result (/ exponent 2)))
        (else (fast-exp-iter base (* base result) (- exponent 1)))))
  

(define (fast-exp b n)
  (fast-exp-iter b 1 n))
  

(fast-exp 3 8)
(fast-exp 2 16)
(fast-exp 3 0)
(fast-exp 3 1)
