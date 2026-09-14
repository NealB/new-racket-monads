#lang curly-fn racket
(require "racket-monad-9-12-simple-outer-syntax-flex.rkt")

(define (~> . args)
  (let ((mapped-args (map (λ (el) (if (procedure? el) el (const el))) args)))
    (foldl compose (car mapped-args) (cdr mapped-args))))

(define-syntax-rule (~>! args ...) ((~> args ...)))

(define (~s* . args) (~>! args (curry apply ~s) displayln))

(define (join* . args) (~>! args (curry map ~a) string-join displayln))

;;;;;;;;;;;;;;;;; Option

(define (Some n) `(#:Some ,n))
(define (None) '(#:None))
   
   
(define (option-bind prev func)
  (match prev
    ('(#:None) (None))
    (`(#:Some ,x) (~>! x func))))
   
   
(define (option-map prev func)
  (match prev
    ('(#:None) (None))
    (`(#:Some ,x) (~>! x func Some))))
   
   
(define (option-return v) (Some v))
   
(macro-curry option-chain (monad-chain
                           #:bind option-bind
                           #:return option-return))
   
   
(define (odiv numerator . denominators)
  (if (member 0 denominators)
      (None)
      (Some (foldl (λ (el acc) (quotient acc el)) numerator denominators))))
   
(define opt-chain
  (option-chain #:let! x (odiv 1000 3)
                #:let! x (odiv x 3)
                #:let! x (Some (- x 100))
                #:let! x (odiv 123 x)
                x))
   
(displayln (~a "opt-chain = " opt-chain "\n\n"))
   
   
   
;;;;;;;;;;;;;; State
   
(struct StateValue (state value))
   
(macro-curry state-chain (monad-chain
                          #:bind (λ (smonad func)
                                   (λ (s0)
                                     (match-define (StateValue s1 value) (smonad s0))
                                     ((func value) s1)))
                             
                          #:return (λ (value) #{StateValue % value})))
   
   
(define (dump-state) (state-chain
                      #:let! s (λ (st) (StateValue st st))
                      (join* "dump state:" s)))
   
(define chain
  (state-chain #:bind-alias bind
   
               (define get-state #{StateValue % %})
               (define get get-state)
                  
               (define (fmap func) (bind #{StateValue (func %) #f} (thunk* get)))
                  
               #:let! a get-state
               #:let! q (fmap (λ (i) (+ i -3)))
   
               (define r 500)
                  
               (define o 55)
               (define xxx 13)
               (displayln (~a "xxx = " xxx " q = " r))
   
               (define (squareme x) (* x x))
                  
               #:let! res (state-chain
                           #:let! inner-res (fmap (λ (i) (* i 321)))
                           inner-res)
   
               (join* "res" res)
                  
               #:let! y (fmap (λ (i) (+ (squareme i) xxx)))
               #:let! ss get
   
               #:do! (fmap (λ (i) (* i 600)))
   
                  
               (format "A = ~a Q = ~a Y = ~a ss = ~a o = ~a"  a q y ss o)))
   
(define chain-final  (chain 80))
   
   
(printf "Final value: '~a'~n"  (~>! chain-final StateValue-value))
(printf "Final state: '~a'~n" (~>! chain-final StateValue-state))


