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
    
    (letfine
     ((a (Some! 34))
      (b (Some! 5))
      (c (bind! (quo a b)))
      (d (Some! 6))
      (e (bind! (quo a d)))))
    
    (Option-return (format "~a / ~a = ~a; ~a / ~a = ~a" a b c a d e)))))

(display results1)
(newline)


(define results2
  (run
   (thunk

    (letfine
     ((a (bind! (Some 34)))
      (b (bind! (Some 5)))
      (c (bind! (quo a b)))
      (d (bind! (Some 0)))
      (e (bind! (quo a d)))))
    
    (Option-return (format "~a / ~a = ~a; ~a / ~a = ~a" a b c a d e)))))
   
(display results2)
(newline)
