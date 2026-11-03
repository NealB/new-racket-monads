#lang racket
(require racket/control)
;(require "writer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define (Some x) (list 'Some x))
;(define (Error e) (list 'Error e))

;(define (make-value-func value) (const value))

(struct optionT (disposition value) #:transparent)

(define (Some x) (optionT 'Some x))
(define (Error e) (optionT 'Error e))

  
(define (quotient-option x y)
  (if (= y 0)
      (optionT 'Error "divide by zero")
      (optionT 'Some (quotient x y))))


(define (Option-bind value cont)
  
  (match value
    ((optionT 'Error (? string?)) value)
    ((optionT 'Some (? number? x)) (cont x))
    (_ (raise (format "Option-bind-- bad argument: ~s" value)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (Option exprs ...)
  (reset (let () (cons 'Some (let () exprs ...)))))

(define (Option-bind! prev)
  (shift k (Option-bind prev k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define results1
  (Option

   (define initial (optionT 'Some 3400))
   
   (define a (Option-bind! initial))
   (define c (Option-bind! (quotient-option a 2)))
   (define e (Option-bind! (quotient-option a 3)))

   ; expanded:
   ;(define e (shift k (Option-bind (quotient-option a 3) k)))
   
   (format "a=~a c=~a e=~a" a c e)))

(display (~a "Results = " results1))
(newline)

