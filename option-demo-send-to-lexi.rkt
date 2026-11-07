#lang racket
(require racket/control)
(require "../racket-utilities/utilities-dev.rkt")

(struct Some (x)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc data port mode)
     (fprintf port "Some ~a" (Some-x data)))])

(struct Error (msg)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc data port mode)
     (fprintf port "Error: ~s" (Error-msg data)))])

(define (quotient-option x y)
  (Some (quotient x y)))

(define (Option-return v) (Some v))


(define (Option-bind m f)
  (~> m
      (match-λ ((Some x) (Some (f x)))
               ((Error msg) (Error msg)))
         
      (match-λ ((Some (Some x)) (Some x))
               ((Some (Error msg)) (Error msg))
               ((var x) x))
      !))

(define-syntax-rule (Option-return* ((vname vexpr) ...) exprs ...)
  (reset (let ((vname vexpr) ...) exprs ...)))


(define (wrap-exception thunk)
  (with-handlers
      ((exn? (λ (e) (Error (exn-message e)))))
    (thunk)))


(define-namespace-anchor nsa)
(define ns (namespace-anchor->namespace nsa))

;; testing...................
(module+ main
  (define results1
    (Option-return* ((a 8000))

      (define fvalue
        (for/fold ((acc #f))
                  ((divisor (in-list '(1 5 1 7 3))))
     
          (shift k (Option-bind (wrap-exception (thapply quotient-option a divisor)) k))))

      (Option-return fvalue)))
  
  
  (printf "~%Return from Option: results = ~a~%" results1)

  (eval `(define results ,results1) ns))

