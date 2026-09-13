#lang curly-fn racket
(require racket/control srfi/1 racket/generator)
(require "monad-run.rkt")
(require "state.rkt")
(require "../racket-utilities/utilities-dev.rkt")

;(define bind! (make-effect! State-bind))

(define (run-with-reset thunk_) (reset (thunk_)))

(define-syntax-rule (run-reset body0 body1 ...)
  (reset
   ((thunk body0 body1 ...))))


(define (with-team-color color thunk)
  (with-continuation-mark 'team-color color (thunk)))

(define (get-team-color)
  (define color
    (continuation-mark-set-first #f 'team-color 'chartreuse))
  (printf "color=~a~n" color))

(define (State-put v)
  (get-team-color)
  (λ (s) (StateValue v v)))

(define (State-get)
  (λ (s) (StateValue s s)))

(define (State-run ma initialState)
  (ma initialState))

(define (State-modify func)
  (λ (s)
    (let ((next-s (func s)))
      (StateValue next-s next-s))))


(define result
  (run-reset
   (with-team-color "blue"
     (thunk
      (begin
        (define bind! (make-effect! State-bind))
        (define a (bind! (State-put 34)))
        (define b (bind! (State-put (+ a 12))))
        (define b2 (get-team-color))
        (define d (bind! (State-modify (curry + 11)))))
      (State-return (format "a=~a b=~a d=~a b2=~a" a b d b2))
      ))))

;(displayln (get-team-color))

(letfine (((State-run result 0) => sv)
          ((get-state sv) => final-state)
          ((get-value sv) => final-value)))

(displayln (~a "final state = " (~s final-state)))
(displayln (~a "final value = " (~s final-value)))




