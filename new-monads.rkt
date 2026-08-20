#lang racket
(require racket/control)
(require "monad-from-continuations.rkt")
(require "option.rkt")

(define run-option (make-run Option.return Option.bindr))

(define results
  (run-option
   (λ (?)
     (let* ((a (? Some 34))
            (b (? Some 5))
            (c (? quo a b))
            (d (? Some 6))
            (e (? quo a d)))

       (format "~a / ~a = ~a; ~a / ~a = ~a" a b c a d e)))))

(display results)
(newline)
