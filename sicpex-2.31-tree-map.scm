#lang planet neil/sicp

(define (tree-map proc tree)
  (map (lambda (x)
	 (if (pair? x)
	     (tree-map proc x)
	     (proc x)))
       tree))

(define square
  (lambda (x) (* x x)))

(define (square-tree tree)
  (tree-map square tree))

(define the-tree
  (list 1
	(list 2  (list 3 4) 5)
	(list 6 7)))
