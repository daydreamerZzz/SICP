#lang planet neil/sicp

(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
	  (accumulate op init (cdr sequence)))))


(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff
		   (* x higher-terms)))
	      0
	      coefficient-sequence))


;;testing

(horner-eval 2 (list 1 3 0 5 0 1))
(horner-eval 1 (list 1 3 3 1))
		      
