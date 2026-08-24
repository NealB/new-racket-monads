#lang racket
(require racket/control)
(require syntax/parse/define (for-syntax racket/syntax))

; special thanks to @soegaard
(define-syntax (define-compex stx)
  (syntax-parse stx
    [(_define-compex name0 bind0 return0)
     (with-syntax ()
       #'(define-syntax (name0 s)
           (syntax-parse s
             [(name0 body0 body (... ...))
              (with-syntax ([bind!   (format-id s "~a!" #'bind)]
                            [return! (format-id s "return!")])
                #'(let ()
                    (define (bind! arg) (shift k (bind0 arg k)))
                    (define return! return0)
                    (define-syntax-rule (define! v expr)
                      (define v (bind! expr)))
                    (reset body0 body (... ...))))])))]))

;(define (oops) (printf "oops...~n"))

(define (make-effect! bind)
  (λ (arg) (shift k (bind arg k))))

(define (run fn)
  (reset (fn)))

(provide run define-compex make-effect!)
