#lang racket

(provide
 (contract-out
  [struct position ((x number?) (y number?))]
  ))

(struct position (x y) #:transparent)
