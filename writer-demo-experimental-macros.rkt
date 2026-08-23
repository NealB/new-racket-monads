#lang racket
(require racket/control racket/syntax (for-syntax racket/control) syntax/parse/define (for-syntax racket/syntax))
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

#;(define (effect! arg)
  (shift k (Writer.bind arg k)))

(define Writer-bind Writer.bind)
(define Writer-return Writer.return)

(define-syntax (define-compex stx)
  (syntax-parse stx
    [(_define-compex name0 bind0 return0)
     (with-syntax ([bind!  (format-id stx "~a!" #'bind0)])
       #'(begin
           (define (bind! arg) (shift k (bind0 arg k)))
              (define return! return0)
              (define-syntax-rule (define! v expr)
                (define v (bind! expr)))
           (define-syntax-rule (name body0 body (... ...))
             (reset body0 body (... ...)))))]))


(define-compex Writer Writer-bind Writer-return)
  
(define results1
  (Writer
   (define a (bind! Writer-return 34))
   (define b (bind! add5 a))
   (define c (bind! add5 b))
   (define d (bind! add5 19))
   (define e (bind! add5 2))
   
   
   (for ((i 3))
     (bind! (write-log "final log message ~a" i)))
   
   (return! e)))
   
(define results2 results1) ;(reset (results1)))


(display (~a "results:\n" (Writer.format results2) (~a "done\n")))
(newline)
