#lang typed/racket

(require math
         plot)

(define normal (normal-dist))
;; (plot (function (distribution-pdf normal) -5 5))

(: mont (-> Real))
(define (mont)
  (let ([r1 (random)]
        [r2 (random)])
    (if (> (sqr r1) r2)
        r1
        (mont)))
  )
(: mont-samples (-> Integer (Listof Real)))
(define (mont-samples n)
  (for/list ([i (in-range n)])
    (mont))
  )

(plot (density (mont-samples 1000)))
