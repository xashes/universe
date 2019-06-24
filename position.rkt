#lang typed/racket

(require math)

(provide [struct-out posn])
(provide move
         move/random)

(struct posn ([x : Real] [y : Real]) #:transparent)

(: move (-> posn Real Real posn))
(define (move p dx dy)
  (let* ([ox (posn-x p)]
         [oy (posn-y p)]
         [nx (+ ox dx)]
         [ny (+ oy dy)]
         )
    (posn nx ny)))

; random walk
(: move/random (-> posn posn))
(define (move/random p)
  (let ([choice : Integer (random 4)]
        [x : Real (posn-x p)]
        [y : Real (posn-y p)]
        )
    (posn
     (cond
       [(= choice 0) (add1 x)]
       [(= choice 1) (sub1 x)]
       [(= choice 2) (add1 y)]
       [else (sub1 y)]
       ) y))
  )

(module+ test

  (require typed/rackunit typed/rackunit/text-ui)

  (let ([posn1 (posn 0 0)]
        )
    (check-equal? (move posn1 1 1) (posn 1 1)))

  )
