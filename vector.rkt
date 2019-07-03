#lang racket

(require math)

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

; limit magnitude of vec to n
(provide (contract-out [vector-limit (-> (vectorof real?) real? (vectorof real?))]))
(define (vector-limit vec n)
  (let ([mag (vector-magnitude vec)])
    (if (<= mag n)
        vec
        (vector-map (lambda (x) (* x (/ n mag)))
                    vec)))
  )
(module+ test
  (check-equal? (vector-limit #(3 4) 10) #(3 4))
  (check-equal? (vector-limit #(30 40) 10) #(6 8))
  )

(provide (contract-out [vector-max (-> (vectorof real?) real?)]))
(define (vector-max vec)
  (apply max (vector->list vec))
  )
(module+ test
  (check-equal? (vector-max #(2 6 1 8)) 8)
  )

; generate random n-dimension vector
(provide (contract-out [random-nd (-> positive-integer? distribution? (vectorof real?))]))
(define (random-nd n [dist (normal-dist)])
  (define d dist)
  (for/vector ([i (in-range n)])
    (sample d)))
(module+ test
  (check-true (< (vector-max (random-nd 100 (uniform-dist 1)))
                 1))
  (check-true (> (vector-max (random-nd 100 (uniform-dist -1 1)))
                 -1))
  )
