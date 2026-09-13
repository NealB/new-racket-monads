#lang racket
(require racket/control)

(define (~> . args) (apply compose (reverse (map (λ (el) (if (procedure? el) el (const el))) args))))

(define (StateValue state value)
  `((state . ,state) (value . ,value)))

(define (get-assoc-element key) (curry assoc key))
(define get-assoc-state (get-assoc-element 'state))
(define get-assoc-value (get-assoc-element 'value))

(define get-state (~> get-assoc-state cdr))
(define get-value (~> get-assoc-value cdr))

(define (State-return value)
  (λ (s) (StateValue s value)))

(define (State-bind ma func)
  (define (run state)

    (let* ((sv (ma state))
           (mb ((~> (get-value sv) func)))
           (sv* ((~> (get-state sv) mb))))
      sv*))
  run)


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
