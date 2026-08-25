#lang curly-fn racket
(require racket/control srfi/1 racket/generator)
(require "monad-run.rkt")
(require "state.rkt")
(require "../racket-utilities/utilities-dev.rkt")

(define bind! (make-effect! State-bind))

(define result
   (run
    (thunk
     
     (define a (bind! (State-put 34)))

     (define b (bind! (State-put (+ a 12))))

     (define d (bind! (State-modify (curry + 11))))
     
     (State-return (format "a=~a b=~a d=~a" a b d)))))

(letfine ((sv (State-run result 0))
          ((get-state sv) => final-state)
          ((get-value sv) => final-value)))

(displayln (~a "final state = " (~s final-state)))
(displayln (~a "final value = " (~s final-value)))
