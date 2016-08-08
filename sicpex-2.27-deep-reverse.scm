#lang planet neil/sicp

(define (deep-reverse t)
  (define (first-fun x)
    (if (pair? x)
	 (deep-reverse x)
	 x))
  (define (reverse-iter tr rt)
    (if (null? tr)
	rt
	(reverse-iter (cdr tr) (cons (first-fun (car tr)) rt))))
  (reverse-iter t nil))
