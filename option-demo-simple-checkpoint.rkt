#lang racket
(require racket/control)


(struct Option-type (disposition value messages) #:transparent)


(define (Some x) (list 'Some x))
(define (Error e) (list 'Error e))

#| (define (Option-bind prev cont)
     (let ((label (car prev)))
       (cond
         ((eq? label 'Error) prev)
      ((eq? label 'Some) (cont (cdr prev))) |#
  
(define (Option-bind prev cont)
  (match prev
    ((list 'Error (? string?)) prev)
    ((list 'Some (? number? x)) (cont x))
    (_ (Error (format "Option-bind-- bad argument: ~s" prev)))))
  

(define (quotient-option x y)
  (if (= y 0)
      (Error "divide by zero")
      (Some (quotient x y))))


;(define (delim-k-bind bind-func arg)
 ; (shift k (bind-func arg k)))

;(define (Option-bind! arg) (delim-k-bind Option-bind arg))

(define (Option . exprs) (reset (cons 'Some exprs)))

(define results1
  (Option

   (let* ((initial (Some 3400))
   
         (a (shift k (Option-bind initial k)))
         (b 5)
         (c (shift k (Option-bind (quotient-option a 2) k)))

         (d 6)
         (e (shift k (Option-bind (quotient-option a 3) k))))
     (format "~a / ~a = ~a; ~a / ~a = ~a" a b c a d e))))

(display results1)
(newline)


#| (define results2
     (Option
      (define a (Option-bind! (Some 34)))
      (define b (Option-bind! (Some 4))) ;(Error "That does not compute!")))
      (define c (Option-bind! (quotient-option a b)))
      (define d (Option-bind! (Some 5)))
      (define e (Option-bind! (quotient-option a d)))
       
      (format "~a / ~a = ~a; ~a / ~a = ~a" a b c a d e)))
      
   (display results2)
   (newline)
 |#