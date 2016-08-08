#lang planet neil/sicp

(define (fringe l)
  (cond ((null? l) nil)
	((not (pair? l)) (list l))
	(else
	 (append (fringe (car l))
		 (fringe (cdr l))))))
