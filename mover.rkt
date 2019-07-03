#lang racket

(require 2htdp/image
         2htdp/universe)
(require math)

(require "vector.rkt")

; constants
(define WIDTH 600)
(define HEIGHT 600)
(define MTS (empty-scene WIDTH HEIGHT))

; class
(define/contract mover%
  (class/c [get-location (->m (vectorof real?))]
           [get-velocity (->m (vectorof real?))]
           [get-acceleration (->m (vectorof real?))]
           [update (->m void?)]
           [render (->m image? image?)]
           )
  (class object%
    (init location velocity acceleration)

    (define current-location location)
    (define current-velocity velocity)
    (define current-acceleration acceleration)
    (define img (circle 20 'solid 'red))
    (define max-vel 10)

    (super-new)

    (define/public (get-location)
      current-location)

    (define/public (get-velocity)
      current-velocity)

    (define/public (get-acceleration)
      current-acceleration)

    (define/public (get-img)
      img)

    ; motion algorithm
    (define/public (update)
      (set! current-acceleration (random-nd 2 (uniform-dist -1 1)))
      (set! current-location (vector-map + current-location current-velocity))
      (set! current-velocity (vector-limit (vector-map + current-velocity current-acceleration) max-vel))
      )

    (define/public (render bg)
      (let-values ([(x y) (vector->values current-location)])
        (let ([x (modulo (exact-floor x) WIDTH)]
              [y (modulo (exact-floor y) HEIGHT)])
          (place-image img
                       x y
                       bg
                       ))))))

(module+ test
  (require rackunit rackunit/text-ui)

  (define m (new mover%
                 [location (vector 0 0)]
                 [velocity (vector 10 10)]
                 [acceleration (vector 20 30)]))
  (send m update)
  (check-equal? (send m get-location) #(10 10))
  (check-equal? (send m render MTS)
                (place-image (send m get-img)
                             10 10
                             MTS
                             ))
  )

; WorldState ::= mover%
(provide (contract-out [tick (-> (is-a?/c mover%) (is-a?/c mover%))]))
(define (tick m)
  (send m update)
  m
  )

(provide (contract-out [render (-> (is-a?/c mover%) image?)]))
(define (render m)
  (send m render MTS)
  )

(define/contract (simulate m0)
  (-> (is-a?/c mover%) (is-a?/c mover%))
  (big-bang m0
            [to-draw render]
            [on-tick tick]
            )
  )

(module+ main

  (define M0 (new mover%
                  [location (vector 0 0)]
                  [velocity (vector 0 0)]
                  [acceleration #(0 0)]
                  ))
  (simulate M0)
  )
