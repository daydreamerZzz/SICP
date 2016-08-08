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
	(accumulate-iter (cdr seq)
		    (op (car seq) so-far))))
  (accumulate-iter sequence initial))

;;how it's defined in the book
(define (accumulate-original op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate-original op initial (cdr sequence)))))


;;actual exercise
(define (new-map p sequence)
  (accumulate
   (lambda (x y)
     (cons (p x)
	   y))
   nil
   sequence))

(define (new-append seq1 seq2)
  (accumulate cons
              seq2
              (reverse seq1)))


(define (new-length sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0
              sequence))