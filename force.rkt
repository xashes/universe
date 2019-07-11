#lang racket

(require "vector.rkt")

;; dissipative forces

;; Friction ::= static friction | kinetic friction
;; kinetic friction
;; Friction = (* -1 mu N velocity-unit-vector)
;; direction = (* -1 (vector-normalize velocity))
;; magnitude = (* coefficient-of-friction normal-force)
(provide (contract-out [friction (-> (vectorof real?) real? real? (vectorof real?))]))
(define (friction velocity c normal-force)
  (let ([vel-unit-vec (vector-normalize velocity)]
        [mag (* c normal-force)])
    (vector-scale (vector-scale vel-unit-vec -1) mag)
    )
  )

;; viscous force, drag force, fliud resistance
;; (* -1/2 density frontal-area coefficient-of-drag (sqr (vector-magnitude velocity)) velocity-unit-vector)
;; direction = (* -1 (vector-normalize velocity))
;; magnitude = (* 1/2 density frontal-area cd (sqr (vector-magnitude velocity)))
(provide (contract-out [drag-force (-> (vectorof real?) real? real? real? (vectorof real?))]))
(define (drag-force velocity [cd 0.1] [density 1] [frontal-area 1])
  (let ([direction (vector-scale (vector-normalize velocity) -1)]
        [mag (* 1/2
                density
                frontal-area
                cd
                (sqr (vector-magnitude velocity)))])
    (vector-scale direction mag))
  )
