#lang racket
(require racket/control)
(require "monad-run.rkt")
(require "writer.rkt")

(define (add5 x)
  (cons (+ 5 x) (format "Add 5 to ~a" x)))


(define bind! (make-effect! Writer-bind))

(define results
   (run
    (thunk
     
     (define a 34)
     (define b (bind! (add5 a)))
     (define c (bind! (add5 b)))
     (define d (bind! (add5 19)))
     (define e (bind! (add5 2)))

     (bind! (Writer-log "final log message"))

     (Writer-return e))))


(display (~a "results:\n" (Writer-format results) (~a "done\n")))
(newline)
