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
    (init-field [mass 20]
                [location (vector (/ WIDTH 2) (/ HEIGHT 2))]
                [velocity #(0 0)]
                [acceleration #(0 0)]
                [color 'red]
                [top-speed 6]
                )

    (define target (vector 200 200))

    (super-new)

    (define/public (figure)
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
      (let ([acc (vector-scale f (/ mass 1))])
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

; force ::= (vectorof real?)
(provide (contract-out [wind (-> (vectorof real?))]))
(define (wind)
  (random-nd 2 (normal-dist 6 16)))

(provide (contract-out [gravity (-> (vectorof real?))]))
(define (gravity)
  #(0 9.8))

; WorldState ::= (listof mover%)
(provide (contract-out [tick (-> (listof (is-a?/c mover%)) (listof (is-a?/c mover%)))]))
(define (tick lom)
  (for/list ([m (in-list lom)])
    (send m apply-force (wind))
    (send m apply-force (gravity))
    (send m update)
    m)
  )

(provide (contract-out [render (-> (listof (is-a?/c mover%)) image?)]))
(define (render lom)
  (for/fold ([bg MTS])
            ([m (in-list lom)])
    (send m render bg))
  )

;; (provide (contract-out [mouse-hd (-> (is-a?/c mover%) integer? integer? mouse-event? (is-a?/c mover%))]))
;; (define (mouse-hd m x y me)
;;   (let ([target (vector x y)])
;;     (send m update-target target)
;;     m)
;;   )

(define/contract (simulate lom)
  (-> (listof (is-a?/c mover%)) (listof (is-a?/c mover%)))
  (big-bang lom
            [to-draw render]
            [on-tick tick]
            )
  )

(define M0 (new mover%))

(provide (contract-out [create-movers (-> positive-integer? (listof (is-a?/c mover%)))]))
(define (create-movers n)
  (for/list ([i (in-range n)])
    (new mover%
         [location (vector (random 0 WIDTH) 0)]
         [mass (random 0 30)]))
  )

(define movers (create-movers 10))

(simulate movers)
