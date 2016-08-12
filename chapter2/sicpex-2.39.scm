#lang planet neil/sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))


(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(define (reverse-right sequence)
  (accumulate (lambda (x y) (append y (list x))) nil sequence))

(reverse-left (list 1 2 3 4 5))
(reverse-right (list 1 2 3 4 5))
