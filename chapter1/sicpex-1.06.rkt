#lang planet neil/sicp
(define (square x) (* x x))
(define (good-enough? guess x)
  (define (abs x) 
    (if (< x 0) (- 0 x) x))
  (< (abs (- (square guess) x)) 0.001)) 

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
(define (improve guess x)
     (average guess (/ x guess)))
(define (average x y)
      (/ (+ x y) 2))
(define (square-root x) 
  (sqrt-iter 1.0 x))

(square-root 3)


;(define (new-if predicate then-c else-c)
;  (cond (predicate then-c)
;        (else else-c)))
;(new-if (= 2 3) 0 5)
;
;(define (new-sqrt-iter guess x)
;  (new-if (good-enough? guess x)
;          guess
;          new-sqrt-iter (improve guess x) x))
;
;(define (new-square-root x)
;  (new-sqrt-iter 1.0 x))
;
;(new-square-root 3)
;
;这个不行- -因为调用new-if的过程中会先求出所有参数值(包括predicate, then-c和else-c)，而new-sqrt-iter里所谓的else-c里会开启新一轮的new-if，无论那个good-enough?返回的是真还是假，然后就停不下来了._.
;但是if函数 (if <predicate> <consequent> <alternate>)会先求出<predicate>，再根据真伪求<consequent>或<alternate>。。。


