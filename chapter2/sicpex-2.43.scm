#lang planet neil/sicp

;;The flatmap in Louis's code invokes (queen-cols (- k 1)) [board-size] number of times (= 8 in eight-queens puzzle), and each queen-cols call contains a flatmap that will again call [board-size] number of queen-cols... till (queen-cols 0)
;;In the correct code queen-cols is called only once on each level of recursion...

