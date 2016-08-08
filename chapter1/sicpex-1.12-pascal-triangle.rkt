#lang planet neil/sicp
(define (pascal-rec a b)
  (if (> b a) 
      (display "error")
      (if (or (= a b) (= b 0))
          1
          (+ (pascal-rec (- a 1) (- b 1)) (pascal-rec (- a 1) b)))))
           

(pascal-rec 3 3)
(pascal-rec 9 5)
(pascal-rec 3 4)