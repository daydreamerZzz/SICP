#lang planet neil/sicp

;defining count-leaves with accumulate.

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate +
	      0
	      (map (lambda (sub-tree) 
                    (if (pair? sub-tree)
                         (count-leaves sub-tree)
                         1)) 
                   t)))

;;test
(define tree
  (list 1 (list (list 2 3) 5)
	(list 6 7)))

(count-leaves tree)

