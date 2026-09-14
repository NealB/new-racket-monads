#lang curly-fn racket
(require "racket-monad-9-12-simple-outer-syntax-flex.rkt")

(define (~> . args)
  (let ((mapped-args (map (λ (el) (if (procedure? el) el (const el))) args)))
    (foldl compose (car mapped-args) (cdr mapped-args))))
(define-syntax-rule (~>! args ...) ((~> args ...)))
(define (~s* . args) (~>! args (curry apply ~s) displayln))
(define (~a* . args) (~>! args (curry apply ~a) displayln))
(define (join* . args) (~>! args (curry map ~a) string-join displayln))

(define (make-interp . keys-functions)
  (define interp-hash (apply hash keys-functions))
  (lambda (key)
    ((hash-ref interp-hash key))))



(define Storyteller-Return ~a)
  
(define Storyteller-Bind (λ (prev next)
                           (define n (next ""))
                           (if (string=? prev "")
                               n
                               (~a prev "\n" n))))


(define Simulator-Return (λ _ identity))
(define Simulator-Bind (λ (prev next)
                         (λ (s)
                           (define s* (prev s))
                           (define nextAction (next (void)))
                           (nextAction s*))))


(struct FrogState (Height Hunger) #:transparent)

(define simulator (make-interp
                   'jump (thunk (λ (s) (FrogState (+ 1 (FrogState-Height s)) (+ 1 (FrogState-Hunger s)))))
                   'croak (thunk (λ (s) (FrogState (FrogState-Height s) (+ 1 (FrogState-Hunger s)))))
                   'eat_fly (thunk (λ (s) (FrogState (FrogState-Height s) 0)))

                   'bind (thunk Simulator-Bind)
                   'return (thunk Simulator-Return)
                   'initial (thunk (const (FrogState 0 0)))
                   'zero (thunk identity)))


(define storyteller (make-interp
                     'jump (thunk "Froggy jumps up!")
                     'croak (thunk "Ribbit!")
                     'eat_fly (thunk "Yum, a fly!")

                     'bind (thunk Storyteller-Bind)
                     'return (thunk Storyteller-Return)
                     'initial (const "")
                     'zero (thunk "")))


(define (adventure interp)

  (monad-chain
   #:bind (interp 'bind)
   #:return (interp 'return)

   #:do! (interp 'jump)
   #:do! (interp 'croak)
   #:do! (interp 'jump)
   #:do! (interp 'eat_fly)

   #:let! result (interp 'zero)
   result))


(define (format-final result)
  (printf "Result: '~s'~n" result)
  (match result
    ((? string? str) str)
    ((? procedure? p) (p (FrogState 0 0)))
    (_ "unknown result type")))

(define (print-result result)
  (~a* "Final result: " result))

(define (inline-print msg)
  (λ ((arg #t))
    (displayln msg)
    arg))

(~>! #f (adventure storyteller) format-final (~> print-result))
(~>! (FrogState 0 0) (adventure simulator) format-final (~> print-result))



;(printf "Raw: ~a, Final Height: ~a, Hunger: ~a~n" final-state (FrogState-Height final-state) (FrogState-Hunger final-state))

