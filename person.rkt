#lang racket

(require "position.rkt")

(provide
 (contract-out
  [struct person ((posn position?))]
  [walk (-> person? person?)]
  ;; [change-direction (-> person? person?)] ;; maybe not provide in interface?
  ))

(struct person (posn) #:transparent #:mutable)

(define (walk p)
  (let* ([old-posn (person-posn p)]
         [new-x (add1 (position-x old-posn))]
         [new-y (position-y old-posn)]
         [new-posn (position new-x new-y)]
         )
    (person new-posn))
  )


(module+ test

  (require rackunit rackunit/text-ui)

  (define p1 (person (position 0 0)))
  (walk p1)

  )
