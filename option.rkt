#lang racket
(require racket/control)


(define (~> . args) (apply compose (reverse (map (λ (el) (if (procedure? el) el (const el))) args))))

  
(define (Option.bindr prev func)
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

;(define run-option (curry run Option.return Option.bindr Option.format))


(provide Option.bindr Option.return Some Error quo Option.format)
