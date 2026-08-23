#lang racket
(require racket/control)
(require "monad-run.rkt")
(require "writer.rkt")

(define-syntax-rule (letfine ((name value) (name2 value2) ...))
  (begin
       (define name value)
       (define name2 value2) ...))

(define (add5 x)
  (cons (+ 5 x) (format "Add 5 to ~a" x)))

(define (write-log msg . args)
  (cons #f (apply format msg args)))

(define (effect! arg)
  (shift k (Writer.bind arg k)))

(define-syntax let!
  (syntax-rules (_)
    ((_ v expr)
     (begin (define v (effect! expr)) v))))

(define (run fn) (reset (fn)))

;(define-syntax-rule (! expr) (let! _ expr))

(define results
   (run
    (thunk
     (define a (effect! (Writer.return 34)))
     (define b (effect! (add5 a)))
     (define c (effect! (add5 b)))
     (define d (effect! (add5 19)))
     (define e (effect! (add5 2)))


     (for ((i 3))
       (effect! (write-log "final log message ~a" i)))

     (Writer.return e))))




(display (~a "results:\n" (Writer.format results) (~a "done\n")))
(newline)
