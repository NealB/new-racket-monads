#lang racket
(require racket/control)
(require "monad-run.rkt" "../racket-utilities/utilities-dev.rkt")

(define-syntax-rule (let** ((name value) ... (_ value1)))
  (let* ((name value) ...) value1))

(define-syntax-rule (letfine ((name value) (name2 value2) ... ))
  (begin
       (define name value)
       (define name2 value2) ...))


(define (procedurize el)
  (match el
    ((? procedure?) el)
    ((? output-port?) #{copy-port (open-input-string* %) el})
    ;((? not) (do-esc))
    (_ (const el))))

(define get-state (~> (curry assoc 'state) cdr))
(define get-value (~> (curry assoc 'value) cdr))

(define compose* (curry apply compose))
(define flatten* (compose flatten list*))

;(define bind! #f)


(define (Pipe-bind input func)
  (cond
    ((not input) (fail! #f))
    ((eq? input 'done) input)
    ((tagged-list? 'error input) (fail! input))
    ((tagged-list? 'tee input) (begin ((second input) input) (func identity)))
           
    ((procedure? input) (func input))
    (else (func (const input)))))



(define (tagged-list? tag input (fn identity))
  (match input
    ((list match-tag rest ...) #:when (eq? tag match-tag) (fn input))
    (_ #f)))


(define (State-bind ma func)
  (define (run state)
    (~> state ma 
        (thunk*
         (define sv (ma state))

         (define mb ((~> (get-value sv) func)))
         ((~> (get-state sv) mb)))))
        
       ; (mb ((~> (get-value sv) func)))
       ; (sv* ((~> (get-state sv) mb))))
      ;sv*))
  run)

#|   (define (run state)
       (let* ((sv (ma state))
              (mb ((~> (get-value sv) func)))
              (sv* ((~> (get-state sv) mb))))
         sv*))
     run)
 |#
(define (~~> . args)
  (let/cc fail!
    (run
     (let ()
       
       (define (Pipe-bind input func)
         (cond
           ((not input) (fail! #f))
           ((eq? input 'done) input)
           ((tagged-list? 'error input) (fail! input))
           ((tagged-list? 'tee input) (begin ((second input) input) (func identity)))
           
           ((procedure? input) (func input))
           (else (func (const input)))))
       
       (set! bind! (make-effect! Pipe-bind))
       
       (letfine ((bnd (map (curry (make-effect! Pipe-bind)) args))
                 (rev (reverse bnd))))

       (compose* (flatten* rev))))))


(define (Pipe-return input)
  (procedurize input))

(provide bind! Pipe-return ~~>)
