#lang planet neil/sicp

(define (reverse l)
  (define (reverse-iter li il)
    (if (null? li)
	il
	(reverse-iter (cdr li) (cons (car li) il))))
  (reverse-iter l nil))
