#lang planet neil/sicp

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
	    (map proc (cdr items)))))

(define (square-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (* tree tree))
	(else
	 (cons (square-tree (car tree))
	       (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree-map sub-tree)
	     (* sub-tree sub-tree)))
       tree))
