#lang typed/racket

(require typed/2htdp/universe
         typed/2htdp/image)

(require "position.rkt")

; constants
(define WIDTH 800)
(define HEIGHT 800)
(define MTS (empty-scene WIDTH HEIGHT))

(define WALKER (circle 20 'solid 'green))
(define WS0 (posn (/ WIDTH 2) (/ HEIGHT 2)))

; world state
(define-type WorldState posn)

; functions
(: simulator (-> WorldState WorldState))
(define (simulator ws)
  (big-bang ws : WorldState
            [to-draw render]
            [on-tick move/gaussian 1/8]
            )
  )

(: render (-> WorldState Image))
(define (render ws)
  (let ([x (modulo (exact-floor (posn-x ws)) WIDTH)]
        [y (modulo (exact-floor (posn-y ws)) HEIGHT)])
    (place-image WALKER
                 x y
                 MTS)))

(module+ main

  (simulator WS0)
  )
