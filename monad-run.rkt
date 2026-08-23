#lang racket
(require racket/control)




(define (make-run m:return m:bind (m:run identity))
  (define (effect operation . arguments)
    (shift k (m:bind (apply operation arguments) k)))

  (λ (procedure)
    (reset (procedure))))


(provide make-run)
