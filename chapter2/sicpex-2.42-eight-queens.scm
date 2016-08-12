#lang planet neil/sicp

;-------some standard processes------


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

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))


;-------------solution-------------

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list nil) ;;empty board = nil/()
	(filter
	 (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (rest-of-queens)
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))


(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))


(define (safe? k positions) 
  ;;check if queen in last and first columns are safe, chop off first column and repeat.
  (not (or (if (or (null? (cdr positions)) (null? positions) (= k 1))
	       false
	       (let ((cur-first-queen (car positions))
                     (last-queen-pos (list-ref positions (- k 1)))) 
                 (or
                  (= cur-first-queen last-queen-pos) ;horizontal check
                  (= (abs (- last-queen-pos cur-first-queen)) (- k 1)) ;diagonal
                  )))
	   (if (= k 1)
               false     
               (not (safe? (- k 1) (cdr positions)))))))


;----------testing--------

(queens 8)
(length (queens 8)) ; = 92 verified
