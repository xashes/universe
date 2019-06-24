#lang typed/racket


(struct posn ([x : Real] [y : Real]) #:transparent)

(: move (-> posn Real Real posn))
(define (move p dx dy)
  (let* ([ox (posn-x p)]
         [oy (posn-y p)]
         [nx (+ ox dx)]
         [ny (+ oy dy)]
         )
    (posn nx ny)))

(module+ test

  (require typed/rackunit typed/rackunit/text-ui)

  (let ([posn1 (posn 0 0)]
        )
    (check-equal? (move posn1 1 1) (posn 1 1)))

  )
