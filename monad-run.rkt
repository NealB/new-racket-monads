#lang racket
(require racket/control)


(define (make-run m:return m:bind (m:run identity))
  (define (effect operation . arguments)
    (shift k (m:bind (apply operation arguments) k)))

  (λ (procedure)
    (m:run (reset (m:return (procedure effect))))))

(provide make-run)
