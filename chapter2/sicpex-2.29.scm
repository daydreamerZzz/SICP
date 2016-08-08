#lang planet neil/sicp

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch thing)
  (car thing))

(define (right-branch thing)
  (car (cdr thing)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight thing)
  (cond ((null? thing) 0)
        ((not (pair? (branch-structure thing))) (branch-structure thing))
        (else
         (+ (total-weight (left-branch (branch-structure thing)))
            (total-weight (right-branch (branch-structure thing)))))))

(define (balance? structure) 
  (define (left-of branch)
    (left-branch (branch-structure branch)))
  (define (right-of branch)
    (right-branch (branch-structure branch)))
  
  (if (not (pair? (branch-structure structure)))
      true
      (and (= (* (total-weight (left-of structure)) (branch-length (left-of structure)))
              (* (total-weight (right-of structure)) (branch-length (right-of structure))))
           (balance? (left-of structure))
           (balance? (right-of structure)))))

(define branch-a (make-branch 1 12))
(define branch-ba (make-branch 2 5))
(define branch-bb (make-branch 2 5))
(define branch-b (make-branch 1.2 (make-mobile branch-ba branch-bb)))
(define struct (make-mobile branch-a branch-b))
(define branchized-struct (make-branch 0 struct))
(total-weight branchized-struct)
(balance? branchized-struct)

(define branch-c (make-branch 2 2))
(define branch-d (make-branch 2 3))
(define newstruct (make-mobile branch-c branch-d))
(define branchized-newstruct (make-branch 0 newstruct))
(total-weight branchized-newstruct)
(balance? branchized-newstruct)