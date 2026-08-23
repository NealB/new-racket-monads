#lang racket
(require racket/control)

(define-syntax-rule (let** ((name value) ... (_ value1)))
  (let* ((name value) ...) value1))
  
(define (Option-bind prev func)
  (match prev
    ((cons 'Error e) prev)
    ((cons 'Some x) (func x))))

(define (Option-return v) (cons 'Some v))

(define (Some x) (cons 'Some x))
(define (Error e) (cons 'Error e))

(define (Option-format opt)
  (match opt
    ((cons 'Error e) (format "Error: ~a" e))
    ((cons 'Some x) x)))


(provide let** Option-bind Option-return Some Error Option-format)
