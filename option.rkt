#lang racket
(require racket/control)

  
(define (Option.bind prev func)
  (match prev
    ((cons 'Error e) prev)
    ((cons 'Some x) (func x))))

(define (Option.return v) (cons 'Some v))

(define (Some x) (cons 'Some x))
(define (Error e) (cons 'Error e))

(define (quo x y)
  (if (= y 0)
      (Error "divide by zero")
      (Some (quotient x y))))

(define (Option.format opt)
  (match opt
    ((cons 'Error e) (format "Error: ~a" e))
    ((cons 'Some x) x)))


(provide Option.bind Option.return Some Error quo Option.format)
