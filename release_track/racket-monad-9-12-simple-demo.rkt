#lang curly-fn racket
(require "racket-monad-9-12-simple-outer-syntax-flex.rkt")


(struct StateValue (state value))

(define (Some n) (list '#:Some n))
(define (None) '(#:None))

(define (option-bindr prev func)
  (match prev
    ('(#:None) (None))
    ((list '#:Some x) (func x))))

(define (option-return v) (Some v))

(macro-curry option-chain (monad-chain
                           #:bind option-bindr
                           #:return option-return))


(define (odiv numerator . denominators)
  (if (member 0 denominators)
      (None)
      (Some (foldl (λ (el acc) (quotient acc el)) numerator denominators))))

(define opt-chain
  (option-chain #:define! x (odiv 1000 3)
                #:define! x (odiv x 3)
                #:define! x (Some (- x 111))
                #:define! x (odiv 1 x)
                x))

(displayln (~a "opt-chain = " opt-chain "\n\n"))


(macro-curry state-chain (monad-chain
                          #:bind (λ (smonad func)
                                   (λ (s0)
                                     (match-define (StateValue s1 value) (smonad s0))
                                     ((func value) s1)))
                          
                          #:return (λ (value) #{StateValue % value})))

(define chain
  (state-chain #:bind-alias bind
               
               (define get #{StateValue % %})
               (define (fmap func) (bind #{StateValue (func %) #f} (thunk* get)))

               #:define! a get
               #:define! q (fmap (λ (i) (+ i -3)))

               (define r 500)
               
               (define o 55)
               (define xxx 13)
               (displayln (~a "xxx = " xxx " q = " r))


               (define (squareme x) (* x x))

               #:define! y (fmap (λ (i) (+ (squareme i) xxx)))
               #:define! ss get

               #:do! (fmap (λ (i) (* i 600)))

               (format "A = ~a Q = ~a Y = ~a ss = ~a o = ~a"  a q y ss o)))


(match-define (StateValue finalState finalValue) (chain 80))
(printf "Final value: '~a'~n"  finalValue)
(printf "Final state: '~a'~n" finalState)
   
