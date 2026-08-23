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

(define (effect arg)
  (shift k (Writer.bind arg k)))

(define-syntax let!
  (syntax-rules (_)

    ((_ _ expr)
     (effect expr))
    
    ((_ v expr)
     (begin (define v (effect expr)) v))))

;(define-syntax-rule (! expr) (let! _ expr))

(define results1
   (reset

     (let! a (Writer.return 34))
     (let! b (add5 a))
     (let! c (add5 b))
     (let! d (add5 19))
     (let! e (add5 2))


     (for ((i 3))
       (effect (write-log "final log message ~a" i)))

     (Writer.return e)))

(define results2 results1) ;(reset (results1)))



(display (~a "results:\n" (Writer.format results2) (~a "done\n")))
(newline)
