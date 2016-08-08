#lang planet neil/sicp


;; defining map one more time...
(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
	    (map proc (cdr items)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence) (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (define (accumulate-iter seq so-far)
    (if (null? seq)
	so-far
	(accumulate (cdr seq)
		    (op (car seq) so-far))))
  (accumulate-iter sequence initial))

(define (accumulate-original op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate-original op initial (cdr sequence)))))
