#lang racket
(require racket/control)
(require "monad-run.rkt")
(require "writer.rkt")

(define (add5 x)
  (cons (+ 5 x) (format "Add 5 to ~a" x)))


(define results
   (run
    (thunk
     (define effect! (make-effect! Writer-bind))
     
     (define a 34)
     (define b (effect! (add5 a)))
     (define c (effect! (add5 b)))
     (define d (effect! (add5 19)))
     (define e (effect! (add5 2)))

     (effect! (Writer-log "final log message"))

     (Writer-return e))))


(display (~a "results:\n" (Writer-format results) (~a "done\n")))
(newline)
