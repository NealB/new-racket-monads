#lang racket

(define-syntax-rule (macro-curry new-name (old-name curry-args ...))
  (define-syntax-rule (new-name args (... ...))
    (old-name curry-args ... args (... ...))))


(define-syntax monad-chain

  (syntax-rules ()
    [(_ #:bind bindr #:return return body ...)
     (let ((bind-name bindr) (return-name return))
       (define-syntax monad/helper
         (syntax-rules ()
    
           [(_ #:define! v expr rest (... ...))
            (bindr expr (λ (v) (monad/helper rest (... ...))))]
    
           [(_ #:let! v expr rest (... ...))
            (monad/helper #:define! v expr rest (... ...))]

           [(_ #:do! expr rest (... ...))
            (bindr expr (λ (_) (monad/helper rest (... ...))))]

           [(_ #:begin (expr (... ...)) rest (... ...))
            (begin expr (... ...) (monad/helper rest (... ...)))]

           [(_ #:display (expr (... ...)) rest (... ...))
            (monad/helper #:begin ((displayln (~a expr (... ...)))) rest (... ...))]

           [(_ #:bind-alias bind-alias rest (... ...))
            (monad/helper #:begin ((define bind-alias bindr)) rest (... ...))]
           
           [(_ #:return-alias return-alias rest (... ...))
            (monad/helper #:begin ((define return-alias return)) rest (... ...))]
           
           [(_ #:return expr)
            (return expr)]

           [(_ expr)
            (monad/helper #:return expr)]

           [(_ expr0 expr1 (... ...))
            (monad/helper #:begin (expr0) expr1 (... ...))]


           ))

       (monad/helper body ...)
       
       )]
      
    ))

(provide monad-chain macro-curry)
