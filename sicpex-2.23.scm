#lang planet neil/sicp

(define (for-each proc items)
  (if (null? items)
      nil
      ((proc (car items))
       (for-each proc
		 (cdr items)))
