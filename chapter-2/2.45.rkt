#lang racket
;https://www.zhihu.com/question/20789155
(require ( planet "sicp.ss" ( "soegaard" "sicp.plt" 2 1)))

(define (split f g)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split f g) painter (- n 1))))
        (f painter (g smaller smaller))))))

(define right-split (split beside below))

(define up-split (split below beside))

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

(paint einstein)
(paint (right-split einstein 3))
(paint (corner-split einstein 3))