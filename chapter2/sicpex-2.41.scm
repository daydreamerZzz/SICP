#lang planet neil/sicp


(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (unique-pairs n)
  (flatmap (lambda (x)
	      (map 
               (lambda (y) (list y x))  ;;any better way?
               (enumerate-interval 1 (- x 1))))
	    (enumerate-interval 1 n)))

(define (unique-triple n)
  (flatmap (lambda (x) (map (lambda (pair) (append pair (list x))) (unique-pairs (- x 1))))
	   (enumerate-interval 3 n)))

;;test
(unique-triple 5)

(define (sum list)
  (accumulate + 0 list))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


;;function required in the problem
(define (solve-triple n s)
  (filter (lambda (triple) (= (sum triple) s)) (unique-triple n)))

;;test
(solve-triple 15 15)
;;looks correct
  