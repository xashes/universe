#lang racket

(require 2htdp/image
         2htdp/universe)
(require math)

(require "vector.rkt"
         "force.rkt"
         "liquid.rkt")

; class
(define/contract mover%
  (class/c [update (->m void?)]
           [figure (->m image?)]
           [render (->m image? image?)]
           [apply-force (->m (vectorof real?) void?)]
           )
  (class object%
    (init-field [mass 20]
                [location (vector (/ WIDTH 2) (/ HEIGHT 2))]
                [velocity #(0.01 0)]
                [acceleration #(0 0)]
                [color 'red]
                [top-speed 10]
                )

    (super-new)

    (define/public (figure)
      (circle mass 'solid color)
      )

    (define/public (apply-force f)
      (let ([acc (vector-scale f (/ 1 mass))])
        (set! acceleration (vector-map + acc acceleration)))
      )

    ; motion algorithm
    (define/public (update)
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

; constants
(define WIDTH 600)
(define HEIGHT 600)
(define MTS (empty-scene WIDTH HEIGHT))
(define WATER (new liquid% [location (vector (/ WIDTH 2) (/ HEIGHT 2))]
                   [width 400]
                   [height 100]
                   [coe-drag 0.1]))

; force ::= (vectorof real?)
(provide (contract-out [wind (-> (vectorof real?))]))
(define (wind)
  (vector 0.001 0))

(provide (contract-out [gravity (-> real? (vectorof real?))]))
(define (gravity mass)
  (vector 0 (* 0.1 mass))
  )

; WorldState ::= (listof mover%)
(provide (contract-out [tick (-> (listof (is-a?/c mover%)) (listof (is-a?/c mover%)))]))
(define (tick lom)
  (for/list ([m (in-list lom)])
    (send m apply-force (wind))
    (send m apply-force (gravity (get-field mass m)))
    (send m apply-force (friction (get-field velocity m) 0.01 (* 0.1 (get-field mass m))))
    (send m update)
    m)
  )

(provide (contract-out [render/liquid (-> (is-a?/c liquid%) image? image?)]))
(define (render/liquid lq bg)
  (send lq render bg)
  )

(provide (contract-out [render/movers (-> (listof (is-a?/c mover%)) image? image?)]))
(define (render/movers lom img)
  (for/fold ([bg img])
            ([m (in-list lom)])
    (send m render bg))
  )

(provide (contract-out [render (-> (listof (is-a?/c mover%)) image?)]))
(define (render lom)
  (render/movers lom
                 (render/liquid WATER MTS))
  )

(define/contract (simulate lom)
  (-> (listof (is-a?/c mover%)) (listof (is-a?/c mover%)))
  (big-bang lom
            [to-draw render]
            [on-tick tick]
            )
  )

(provide (contract-out [create-movers (-> positive-integer? (listof (is-a?/c mover%)))]))
(define (create-movers n)
  (for/list ([i (in-range n)])
    (new mover%
         [location (vector (random 0 WIDTH) 0)]
         [mass (random 10 30)]))
  )

(define movers (create-movers 10))

(simulate movers)
