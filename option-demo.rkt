#lang racket
(require racket/control)
(require "monad-run.rkt")
(require "option.rkt")


(define (quo x y)
  (if (= y 0)
      (Error "divide by zero")
      (Some (quotient x y))))



(define results1
  (run
   (thunk
    (define effect! (make-effect! Option-bind))
    (define Some! (compose1 effect! Some))
    
    (let** ((a (Some! 34))
            (b (Some! 5))
            (c (effect! (quo a b)))
            (d (Some! 6))
            (e (effect! (quo a d)))
            (_  (Option-return (format "~a / ~a = ~a; ~a / ~a = ~a" a b c a d e))))))))

(display results1)
(newline)


(define results2
  (run
   (thunk
    (define effect! (make-effect! Option-bind))
    (define Some! (compose1 effect! Some))

    (let** ((a (Some! 34))
            (b (Some! 5))
            (c (effect! (quo a b)))
            (d (Some! 0))
            (e (effect! (quo a d)))
            (_ (Option-return (format "~a / ~a = ~a; ~a / ~a = ~a" a b c a d e))))))))
   
(display results2)
(newline)
