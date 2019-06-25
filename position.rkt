#lang typed/racket

(require math)

(provide [struct-out posn])
(provide move
         move/random
         move/random/track)

(struct posn ([x : Real] [y : Real]) #:transparent)

(: move (-> posn Real Real posn))
(define (move p dx dy)
  (let* ([ox (posn-x p)]
         [oy (posn-y p)]
         [nx (+ ox dx)]
         [ny (+ oy dy)]
         )
    (posn nx ny)))

; random walk
(: move/random (-> posn posn))
(define (move/random p)
  (let ([choice : Integer (random 4)]
        [delta : Integer 10]
        )
    (cond
      [(= choice 0) (move p delta 0)]
      [(= choice 1) (move p (- delta) 0)]
      [(= choice 2) (move p 0 delta)]
      [else (move p 0 (- delta))]))
  )

(: move/random/track (-> (Listof posn) (Listof posn)))
(define (move/random/track lop)
  (cons (move/random (first lop)) lop)
  )

(module+ test

  (require typed/rackunit typed/rackunit/text-ui)

  (let ([posn1 (posn 0 0)]
        )
    (check-equal? (move posn1 1 1) (posn 1 1)))

  )
