#lang racket
(require racket/control)

(define (Some x) (list 'Some x))
(define (Error e) (list 'Error e))
  
(define (quotient-option x y)
  (if (= y 0)
      (Error "divide by zero")
      (Some (quotient x y))))

(define (Option-bind prev cont)
  (match prev
    ((list 'Error (? string?)) prev)
    ((list 'Some (? number? x)) (cont x))
    (_ (raise (format "Option-bind-- bad argument: ~s" prev)))))
  
(define-syntax-rule (Option exprs ...)
  (reset (let () (cons 'Some (let () exprs ...)))))

(define-syntax-rule (Option-define name exprs ...)
  (define name (Option exprs ...)))

(define (Option-bind! prev)
  (shift k (Option-bind prev k)))

;; changes exceptions into Option errors
(define-syntax-rule (Option-catch! expr)
  (shift k ; (Option-bind prev k))
         (with-handlers
             ((exn? (λ (e) (Error (exn-message e)))))
           (Option-bind expr k))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; testing

(Option-define results1

   (define initial (Some 3400))
   
   (define a (Option-catch! initial))
   (define c (Option-catch! (Some (quotient a 3))))
   (define e #f)
   
   (for ((divisor (in-range 4 -4 -1)))
     (printf "divisor = ~a: " divisor)
     
     (let ((result (Option-catch! (Some (quotient a (sub1 divisor))))))
       (printf "result = ~a~%" result)))

   ;(let loop ((result (Option-catch! (Some (quotient 100 (sub1 divisor))))))
   ;  (printf "result = ~a~%" result))
   
   (format "Return from Option: a=~a c=~a e=~a" a c e))

(printf "~%Results = ~s~%" results1)
(newline)

