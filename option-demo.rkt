#lang racket
(require racket/control)
(require "monad-run.rkt")
(require "option.rkt")

(define run-option (make-run Option.return Option.bind))

(define results1
  (run-option
   (λ (?)
     (let* ((a (? Some 34))
            (b (? Some 5))
            (c (? quo a b))
            (d (? Some 6))
            (e (? quo a d)))

       (format "~a / ~a = ~a; ~a / ~a = ~a" a b c a d e)))))

(display results1)
(newline)


(define results2
  (run-option
   (λ (?)
     (let* ((a (? Some 34))
            (b (? Some 5))
            (c (? quo a b))
            (d (? Some 0))
            (e (? quo a d)))

       (format "~a / ~a = ~a; ~a / ~a = ~a" a b c a d e)))))

(display results2)
(newline)
