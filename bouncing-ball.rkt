#lang racket

(require 2htdp/image
         2htdp/universe)

(require math)

; WorldState ::= Location

; constants
(define WIDTH 600)
(define HEIGHT 200)
(define MTS (empty-scene WIDTH HEIGHT))

(define LOC0 (array #[(/ WIDTH 2) (/ HEIGHT 2)]))

(define BALL-PIC (circle 20 'solid 'red))

(define VEL (array->mutable-array (array #[10 30])))

(define (render loc)
  (place-image BALL-PIC
               (array-ref loc #(0))
               (array-ref loc #(1))
               MTS)
  )
(module+ test

  (require rackunit rackunit/text-ui)

  (check-equal? (render LOC0)
                (place-image BALL-PIC
                             (/ WIDTH 2) (/ HEIGHT 2)
                             MTS))
  )

(define (move loc vel)
  (array+ loc vel)
  )

(define (redirect v idx)
  (array-set! v idx (- (array-ref v idx)))
  )

; (-> Loc Loc)
(define (tick loc)
  (let ([x (array-ref loc #(0))]
        [y (array-ref loc #(1))])
    (cond
      [(or (< x 0) (> x WIDTH)) (begin (redirect VEL #(0))
                                       (move loc VEL))]
      [(or (< y 0) (> y HEIGHT)) (begin (redirect VEL #(1))
                                        (move loc VEL))]
      [else (move loc VEL)]))
  )

(define (run loc)
  (big-bang loc
            [on-tick tick]
            [to-draw render]
            )
  )

(run LOC0)
