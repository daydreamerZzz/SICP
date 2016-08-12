#lang planet neil/sicp

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (unique-pairs n)
  (flatmap (lambda (x)
	      (map 
               (lambda (y) (list y x))
               (enumerate-interval 1 (- x 1))))
	    (enumerate-interval 1 n)))

;;test
(unique-pairs 5)
;(unique-pairs 6)	


;;simplify prime-sum-pairs
(define (smallest-divisor n)
  (find-divisor n 2))

(define (next divisor)
  (if (= divisor 2)
      3
      (+ 2 divisor)))

(define square (lambda (x) (* x x)))

(define (divides? divisor num)
  (= (remainder num divisor) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (prime? num)
  (= num (smallest-divisor num)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))


(prime-sum-pairs 6)