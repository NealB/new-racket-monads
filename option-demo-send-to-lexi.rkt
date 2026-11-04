#lang racket
(require racket/control)

(define (Some x) (list 'Some x))
(define (Error e) (list 'Error e))
  
(define (quotient-option x y)
  (Some (quotient x y)))


(define (Option-bind prev cont)
  (match prev
    ((list 'Error (? string?)) prev)
    ((list 'Some (? number? x)) (cont x))
    (_ (raise (format "Option-bind-- bad argument: ~s" prev)))))
  
(define-syntax-rule (Option-return* exprs ...)
  (reset (let () exprs ...)))
 
#;(define-syntax-rule (Option-bind! expr)
  (shift k
         (with-handlers
             ((exn? (λ (e) (Error (exn-message e)))))
           (Option-bind expr k))))

(define (wrap-exception thunk error-cb)
  (with-handlers
      ((exn? (λ (e) (error-cb (exn-message e)))))
    (thunk)))

;; testing...................
(module+ main
  (define results1
    (Option-return*

      (define a 3400)
   
      (for/fold ((acc #f))
                ((divisor (in-range 1 5 1)))
     
        (shift k
               (wrap-exception
                (thunk (Option-bind (quotient-option a divisor) k))
                (λ (msg) (Error msg)))))))
  
#|       (for/fold ((acc #f))
                   ((divisor (in-range 1 5 1)))
           (printf "divisor = ~a: " divisor)
        
           (let ((result (Option-bind! (quotient-option a divisor))))
             (printf "result = ~a~%" result)
          result)))) |#

  
  (printf "~%Return from Option: results = ~a~%" results1))


