#lang racket

(provide
 (contract-out
  [struct position ((x number?) (y number?))]
  [move (-> position? number? number? position?)]
  ))

(struct position (x y) #:transparent)

(define (move p dx dy)
  (let* ([ox (position-x p)]
         [oy (position-y p)]
         [nx (+ ox dx)]
         [ny (+ oy dy)]
         )
    (position nx ny)))


(module+ test

  (require rackunit rackunit/text-ui)

  (let ([posn1 (position 0 0)]
        )
    (check-equal? (move posn1 1 1) (position 1 1)))

  )
