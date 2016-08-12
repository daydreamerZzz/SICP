#lang planet neil/sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))


(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map
   (lambda (row) (dot-product row v))
   m))

(define (transpose mat)
  (accumulate-n cons nil mat))

;;wow
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
     (lambda (row) (matrix-*-vector cols row))
     m)))


(define A (list (list 1 2 3 4)
		(list 4 5 6 6)
		(list 6 7 8 9)))

(define B (list (list 1 9)
		(list 2 8)
		(list 3 7)
		(list 4 6)))

(transpose A)
(matrix-*-matrix A B)
