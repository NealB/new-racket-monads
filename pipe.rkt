#lang racket
(require racket/control)

(define-syntax-rule (let** ((name value) ... (_ value1)))
  (let* ((name value) ...) value1))

(define-syntax-rule (letfine ((name value) (name2 value2) ...))
  (begin
       (define name value)
       (define name2 value2) ...))


(define (procedurize el)
  (match el
    ((? procedure?) el)
    ((? output-port?) #{copy-port (open-input-string* %) el})
    ;((? not) (do-esc))
    (_ (const el))))


(define (Pipe-bind input func)
  (cond
    ((not input) input)
    ((eq? input 'done) input)
    ((and (list? input) (eq? (car input) 'error)) input)
    (else (func input))))

(define (Pipe-return input)
  (procedurize input))


(provide Pipe-bind Pipe-return )
