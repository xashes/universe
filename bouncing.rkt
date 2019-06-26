#lang racket

(require 2htdp/image
         2htdp/universe)

(require "position.rkt")

; constants
(define BALL (circle 20 'solid 'red))

(define WIDTH 800)
(define HEIGHT 200)
(define MTS (empty-scene WIDTH HEIGHT))

(define XSPEED 10)
(define YSPEED 30)

(define (bouncing p)
  (let ([x (posn-x p)]
        [y (posn-y p)])
    (cond
      [(or (< x 0) (> x WIDTH)) (begin (set! XSPEED (- XSPEED))
                                         (move p XSPEED YSPEED))]
      [(or (< y 0) (> y HEIGHT)) (begin (set! YSPEED (- YSPEED))
                                          (move p XSPEED YSPEED))]
      [else (move p XSPEED YSPEED)]
      )))

(define (render p)
  (place-image BALL
               (posn-x p) (posn-y p)
               MTS))

(define (run p)
  (big-bang p
            [on-tick bouncing]
            [on-draw render]))

(module+ main

  (run (posn 0 (/ HEIGHT 2)))
  )
