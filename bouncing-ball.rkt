#lang typed/racket

(require typed/2htdp/universe
         typed/2htdp/image)

(require math)

(struct ball ([loc : (Array Real)] [vel : (Array Real)]) #:transparent #:mutable)

; constants
(define WIDTH 600)
(define HEIGHT 200)
(define MTS (empty-scene WIDTH HEIGHT))

(define VEL : (Array Real) (array #[1 3]))

(define BALL0 (ball (array #[0 0])
                    VEL))
(define BALL-PIC (circle 20 'solid 'red))

(: render (-> ball Image))
(define (render b)
  (let* ([loc : (Array Real) (ball-loc b)]
         [x (array-ref loc #(0))]
         [y (array-ref loc #(1))]
         )
    (place-image BALL-PIC
                 x y
                 MTS))
  )
(module+ test

  (require typed/rackunit typed/rackunit/text-ui)

  (check-equal? (render BALL0)
                (place-image BALL-PIC
                             0 0
                             MTS))
  )

(: move (-> ball ball))
(define (move b)
  (let* ([loc : (Array Real) (ball-loc b)]
         [x (array-ref loc #(0))]
         [y (array-ref loc #(1))]
         [vel : (Array Real) (ball-vel b)]
         [vx (array-ref vel #(0))]
         [vy (array-ref vel #(1))]
         )
    (cond
      [(or (< x 0) (> x WIDTH)) (begin (set-ball-vel! b (array #((- vx) vy)))
                                       (ball (array+ loc vel) (ball-vel b)))]
      [(or (< y 0) (> y HEIGHT)) (begin (set-ball-vel! b (array #(vx (- vy))))
                                        (ball (array+ loc vel) (ball-vel b)))]
      [else (ball (array+ loc vel)
                  vel)]))
  )

(: run (-> ball ball))
(define (run b)
  (big-bang b : ball
            [on-tick move]
            [to-draw render]
            )
  )

(run BALL0)
