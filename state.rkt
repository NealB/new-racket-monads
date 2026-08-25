#lang racket
(require racket/control)

(define (~> . args) (apply compose (reverse (map (λ (el) (if (procedure? el) el (const el))) args))))

(define (StateValue state value)
  `((state . ,state) (value . ,value)))

(define get-state (~> (curry assoc 'state) cdr))
(define get-value (~> (curry assoc 'value) cdr))

(define (State-return value)
  (λ (s) (StateValue s value)))

(define (State-bind ma func)
     (λ (state)
       (let* ((sv (ma state))
              (mb ((~> (get-value sv) func)))
              (sv* ((~> (get-state sv) mb)))) sv*)))


(define (State-put v)
  (λ (s) (StateValue v v)))

(define (State-get)
  (λ (s) (StateValue s s)))

(define (State-run ma initialState)
  (ma initialState))

(define (State-modify func)
  (λ (s)
    (let ((next-s (func s)))
      (StateValue next-s next-s))))
     
(provide StateValue get-state get-value State-bind State-return State-get State-put State-run State-modify)
