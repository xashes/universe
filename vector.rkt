#lang racket

(provide (contract-out [vector-magnitude (-> (vectorof real?) real?)]))
(define (vector-magnitude vec)
  (sqrt (apply + (vector->list (vector-map sqr vec))))
  )
(module+ test
  (require rackunit rackunit/text-ui)

  (check-equal? (vector-magnitude #(3 4)) 5)
  )

(provide (contract-out [vector-normalize (-> (vectorof real?) (vectorof real?))]))
(define (vector-normalize vec)
  (vector-map (lambda (x) (/ x (vector-magnitude vec)))
              vec))
(module+ test
  (check-equal? (vector-normalize #(3 4))
                #(3/5 4/5))
  )
