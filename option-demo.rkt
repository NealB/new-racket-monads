#lang racket
(require racket/control)
(require "writer.rkt")


(struct Option-type (disposition value messages) #:transparent)


(define (Some x) (list 'Some x))
(define (Error e) (list 'Error e))


  
(define (quotient-option x y)
  (if (= y 0)
      (Error "divide by zero")
      (Some (quotient x y))))



(define-syntax-rule (Option exprs ...)
  (reset (let* () (cons 'Some (let* () exprs ...)))))


(define (Option-bind prev cont)
  (match prev
    ((list 'Error (? string?)) prev)
    ((list 'Some (? number? x)) (cont x))
    (_ (Error (format "Option-bind-- bad argument: ~s" prev)))))
  

(define results1
  (Option

   (define initial (Some 3400))


   (define-syntax-rule (Option-bind! prev)
     (shift k (Option-bind prev k)))
   
   (define a (Option-bind! initial))
   (define c (Option-bind! (quotient-option a 2)))
   
   (define e (shift k (Option-bind (quotient-option a 3) k)))
   
   (format "a=~a c=~a e=~a" a c e)))

(display (~a "Reults = " results1))
(newline)

