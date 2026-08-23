#lang racket
(require racket/control)
(require "monad-run.rkt")
(require "option.rkt")


(define (quo x y)
  (if (= y 0)
      (Error "divide by zero")
      (Some (quotient x y))))


(define bind! (make-effect! Option-bind))
(define Some! (compose1 bind! Some))

(define results1
  (run
   (thunk
    
    (let** ((a (Some! 34))
            (b (Some! 5))
            (c (bind! (quo a b)))
            (d (Some! 6))
            (e (bind! (quo a d)))
            (_  (Option-return (format "~a / ~a = ~a; ~a / ~a = ~a" a b c a d e))))))))

(display results1)
(newline)


(define results2
  (run
   (thunk

    (let** ((a (Some! 34))
            (b (Some! 5))
            (c (bind! (quo a b)))
            (d (Some! 0))
            (e (bind! (quo a d)))
            (_ (Option-return (format "~a / ~a = ~a; ~a / ~a = ~a" a b c a d e))))))))
   
(display results2)
(newline)
