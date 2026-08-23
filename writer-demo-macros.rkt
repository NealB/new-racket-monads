#lang racket
(require racket/control (for-syntax racket/control))
(require "monad-run.rkt")
(require "writer.rkt")

(define-compex Writer Writer-bind Writer-return)

(define (add5 x)
  (cons (+ 5 x) (format "Add 5 to ~a" x)))

  
(define results
  (Writer
   (define a 34)
   (define b (bind! (add5 a)))
   (define c (bind! (add5 b)))
   (define d (bind! (add5 19)))
   (define e (bind! (add5 2)))
   
   (bind! (Writer-log "final log message" ))
   
   (return! e)))
   

(displayln (~a "results:\n" (Writer-format results) "done\n"))
