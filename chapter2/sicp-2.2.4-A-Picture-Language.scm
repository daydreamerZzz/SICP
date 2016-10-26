#lang sicp
(#%require sicp-pict)


;;------Example------
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))


;;------Exercise 2.44------

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(paint (corner-split einstein 7))



;;------Example------

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;; new definition of flipped-pairs
(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))


(define (square-limit-2 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;(paint (square-limit einstein 5))
;(paint (square-limit-2 einstein 5))






;;------Exercise 2.45------
;; basically using structure from the *-splits' definition, replacing below/besides by parameters/arguments where appropriate

(define (split first second)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller ((split first second) painter (- n 1))))
	  (first painter (second smaller smaller))))))

(define us (split below beside))

;(paint (us einstein 5))


;;-----Example-----
;;already implemented... raises error when origin-frame selector is applied to the frame structure implemented internally...

;;(define (eg-frame-coord-map frame)
;;  (lambda (v)
;;    (add-vect
;;     (origin-frame frame)
;;     (add-vect (scale-vect (xcor-vect v)
;;                           (edge1-frame frame))
;;               (scale-vect (xcor-vect v)
;;                           (edge2-frame frame))))))


;;------Exercise 2.46------

;---constructor and selector for vectors---
;(define (make-vect xcor ycor)
;  (display (cons xcor ycor))
;  (cons xcor ycor))
;
;(define (xcor-vect vect)
;  (car vect))
;
;(define (ycor-vect vect)
;  (cdr vect))

;;---implement functions for vector addition, subtraction and scalar multiplication using the above---

;(define (add-vect vect1 vect2)
;  (make-vect (+ (xcor-vect vect1)
;		(xcor-vect vect2))
;	     (+ (ycor-vect vect1)
;		(ycor-vect vect2))))
;
;(define (scale-vect k v)
;  (make-vect (* (xcor-vect v) k)
;	     (* (ycor-vect v) k)))
;
;(define (sub-vect vect1 vect2)
;  (add-vect vect1 (scale-vect -1 vect2)))


;(define a (make-vect 1 3))
;(define b (make-vect 5 7))
;(define c (sub-vect a b))
;(xcor-vect c)
;(ycor-vect c)


;;------Exercise 2.47: constructor/selector for frames------

;;---constructor 1---
;(define (make-frame origin edge1 edge2)
;  (list origin edge1 edge2))

;(car (make-frame (make-vect 0.0 0.0) (make-vect 1.0 0.0) (make-vect 0.0 1.0)))

;;---implement selector appropriate for the above---
;(define (origin-frame frame)
;  (display frame)
;  (list-ref frame 1))
;
;(define (edge1-frame frame)
;  (cadr frame))
;
;(define (edge2-frame frame)
;  (caddr frame))

;;---constructor 2---
(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))


;;---selectors 2---
(define (origin-frame2 frame)
  (car frame))

(define (edge1-frame2 frame)
  (cadr frame))

(define (edge2-frame2 frame)
  (cddr frame))


;;------Example------
;;draw-line isn't really implemented

(define (useless-draw-line v1 v2)
  (make-segment v1 v2))

;;According to this post ->  http://stackoverflow.com/questions/13592352/compiling-sicp-picture-exercises-in-drracket
;;segments->painter itself is implemented in neil/sicp

;(define (eg-segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (useless-draw-line
;	((frame-coord-map frame) (start-segment segment))
;	((frame-coord-map frame) (end-segment segment))))
;     segment-list)))


;;------Exercise 2.48------

;(define (make-segment start-vector end-vector)
;  (cons start-vector end-vector))
;
;(define (start-segment segment)
;  (car segment))
;
;(define (end-segment segment)
;  (cdr segment))



;;------Exercise 2.49------

;;part a, painter that draws outline of designated frame
(define painter-a
  (let ((line-1 (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0)))
	(line-2 (make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0)))
	(line-3 (make-segment (make-vect 1.0 0.0) (make-vect 1.0 1.0)))
	(line-4 (make-segment (make-vect 0.0 1.0) (make-vect 1.0 1.0))))
    (segments->painter (list line-1 line-2 line-3 line-4))))

;;part b, painter that draws an "X"
(define painter-b
  (let ((line-1 (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0)))
	(line-2 (make-segment (make-vect 1.0 0.0) (make-vect 0.0 1.0))))
    (segments->painter (list line-1 line-2))))

;;part c, painter that draws a diamond shape
(define painter-c
  (let ((line-1 (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5)))
	(line-2 (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0)))
	(line-3 (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.5)))
	(line-4 (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0))))
    (segments->painter (list line-1 line-2 line-3 line-4))))

;(paint painter-c)

;;part d, the wave painter



;;------Example: painter transformations------
;; also implemented
;;(define (transform-painter painter origin corner1 corner2)
;;  (lambda (frame)
;;    (let ((m (frame-coord-map frame)))
;;      (let ((new-origin (m origin))) ;origin of the frame that defines the edges of the painter in "absolute" coordinates, originally represented in "NDC" of the big frame.
;;	(painter
;;	 (make-frame new-origin
;;		     (sub-vect (m corner1) new-origin)
;;		     (sub-vect (m corner2) new-origin)))))))



(define (eg-flip-vert painter)
  ((transform-painter (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)) painter))

(define (eg-shrink-to-upper-right painter)
  ((transform-painter (make-vect 0.5 0.5)
		     (make-vect 1.0 0.5)
		     (make-vect 0.5 1.0)) painter))

(define (eg-rotate90 painter)
  ((transform-painter (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)) painter))

;;(define (eg-squash-inwards painter)
;;  ((transform-painter (make-vect 1.0 1.0)
;;		     (make-vect 0.65 0.35)
;;		     (make-vect 0.35 0.65)) painter))


;(paint (eg-rotate90 einstein))


(define (eg-beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   ((transform-painter (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)) painter1))
	  (paint-right
	   ((transform-painter split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0)) painter2)))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))
  
;(paint (eg-beside einstein einstein))


;;------Exercise 2.50------

(define (ex-flip-horiz painter)
  ((transform-painter (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)) painter))

(define (ex-rotate180 painter)
  ((transform-painter (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 0.0)) painter))


(define (ex-counterrotate270 painter)
  ((transform-painter (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)) painter))

;;------Exercise 2.51------
;;Define below in 2 ways: 1.like eg-beside above; 2.using beside and rotations from Ex 2.50

(define (ex-below1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-lower
	   ((transform-painter (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point) painter1))
	  (paint-upper
           ((transform-painter split-point
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0)) painter2)))
      (lambda (frame)
	(paint-lower frame)
	(paint-upper frame)))))

;(paint (ex-below1 einstein einstein))

(define (ex-below2 painter1 painter2)
  (let ((new-painter1 (ex-rotate180 (ex-counterrotate270 painter1)))
	(new-painter2 (ex-rotate180 (ex-counterrotate270 painter2))))
    (ex-counterrotate270 (eg-beside new-painter2 new-painter1))))

;(paint (ex-below2 einstein einstein))


;;------Exercise 2.52------

;;Part b
(define (new-corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right  corner))))))

(paint (new-corner-split einstein 7))
