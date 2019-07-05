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
  (class/c [update-target (->m (vectorof real?) void?)]
           [update (->m void?)]
           [figure (->m image?)]
           [render (->m image? image?)]
           [apply-force (->m (vectorof real?) void?)]
           )
  (class object%
    (init-field [location (vector (/ WIDTH 2) (/ HEIGHT 2))]
                [velocity #(0 0)]
                [acceleration #(0 0)]
                [mass 10]
                [color 'red]
                [top-speed 6]
                )

    (define target (vector 200 200))

    (super-new)

    (define (figure)
      (circle mass 'solid color)
      )

    (define/public (update-target vec)
      (set! target vec)
      )

    (define/private (target-vec)
      (vector-map - target location)
      )

    (define/private (acc-mag)
      (vector-magnitude (target-vec))
      )

    ; -> real?
    (define/private (gravity)
      (/ 6
         (+ (acc-mag) 1)))

    ; -> void?
    (define/private (update-acc)
      (set! acceleration (vector-scale (vector-normalize (target-vec)) (gravity)))
      )

    (define/public (apply-force f)
      (let ([acc (/ f mass)])
        (set! acceleration (vector-map + acc acceleration)))
      )

    ; motion algorithm
    (define/public (update)
      (update-acc)
      (set! velocity (vector-limit (vector-map + velocity acceleration) top-speed))
      (set! location (vector-map + location velocity))
      (set! acceleration #(0 0))
      )

    (define/public (render bg)
      (let-values ([(x y) (vector->values location)])
        (let ([x (modulo (exact-floor x) WIDTH)]
              [y (modulo (exact-floor y) HEIGHT)])
          (place-image (figure)
                       x y
                       bg
                       ))))))

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

(provide (contract-out [mouse-hd (-> (is-a?/c mover%) integer? integer? mouse-event? (is-a?/c mover%))]))
(define (mouse-hd m x y me)
  (let ([target (vector x y)])
    (send m update-target target)
    m)
  )

(define/contract (simulate m0)
  (-> (is-a?/c mover%) (is-a?/c mover%))
  (big-bang m0
            [to-draw render]
            [on-mouse mouse-hd]
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
