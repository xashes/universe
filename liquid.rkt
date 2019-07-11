#lang racket

(require 2htdp/image
         2htdp/universe)
(require math)

(require "vector.rkt"
         "force.rkt")

(provide liquid%)

(define/contract liquid%
  (class/c (init-field [location (vectorof real?)]
                       [width real?]
                       [height real?]
                       [coe-drag real?])
           [figure (->m image?)]
           [render (->m image? image?)]
           )
  (class object%
    (init-field location
                width
                height
                coe-drag
                )

    (super-new)

    (define/public (figure)
      (rectangle width height 'outline 'lightblue)
      )

    (define/public (render bg)
      (let-values ([(x y) (vector->values location)])
        (place-image (figure)
                     x y
                     bg)))
    )
  )

(module+ test
  (require rackunit rackunit/text-ui)

  (define MTS (empty-scene 200 200))
  (define lq (new liquid% [location #(100 100)]
                  [width 100]
                  [height 60]
                  [coe-drag 0.1]))
  (check-equal? (send lq render MTS)
                (place-image (send lq figure)
                             100 100
                             MTS))
  )
