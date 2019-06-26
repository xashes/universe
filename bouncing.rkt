#lang typed/racket

(require typed/2htdp/universe
         typed/2htdp/image)

(require "position.rkt")

; constants
(: BALL Image)
(define BALL (circle 20 'solid 'red))

(define WIDTH 800)
(define HEIGHT 200)
(define MTS (empty-scene WIDTH HEIGHT))

(define XSPEED 1)
(define YSPEED 3)

(: bouncing (-> posn posn))
(define (bouncing p)
  p
  )
(module+ test

  (require typed/rackunit typed/rackunit/text-ui)

  (check-equal? (bouncing (posn -1 0))
                (posn 0 3))
  (check-equal? (bouncing (posn WIDTH HEIGHT))
                (posn (- WIDTH XSPEED) (- HEIGHT YSPEED)))

  )
