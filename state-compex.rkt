#lang racket
(require racket/match racket/struct)
(require "../../racket-utilities/utilities.rkt")
;(require "computation-expression-style.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; monad builder (bindr and return come from the environment)
(define-syntax comp
  (syntax-rules (let! do! let begin <-)

    ;[(_ (let! v expr) first rest ...)
    ; (bind expr (λ (v) (comp first rest ...)))]

    ;[(_ (let v expr) first rest ...)
    ; (let ((v expr)) (comp first rest ...))]

    ;[(_ (begin exprs ...) first rest ...)
    ; (let () exprs ... (comp first rest ...))]
    
    [(_ (do! expr) first rest ...)
     (bind expr (λ (_) (comp first rest ...)))]
    
     ;(comp (let! _ expr) rest ...)]
    
    ;[(_ (v <- expr) rest ...)
    ; (comp (let! v expr) rest ...)]

    [(_ ((bind_ return_)) rest ...)
     (let ()
       ;(define bind bind_)
       ;(define return return_)
       rest ...)]
    
    [(_ value)
     (return value)]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (printfn . args) (apply printf args) (newline))

(define (~> . args) (apply compose (reverse (map (λ (el) (if (procedure? el) el (const el))) args))))

;(struct StateMonad (Run))
;(struct StateValue (state value) #:transparent)
(define (StateValue state value)
  `(StateValue #:state ,state #:value ,value))

(define (State.return value)
  (λ (s) (StateValue s value)))

(define (State.bind smonad func)
  (~>
      (curry State.run smonad)
      struct->list
      (curry apply (λ (s^ t) (State.run (func t) s^)))))

(define (State.get)
  (λ (s) (StateValue s s)))

  ;(StateMonad (λ (s) (StateValue s s))))


(define (State.run state initialState)
  (state initialState))


#;(define (State.run state initialState)
  ((StateMonad-Run state) initialState))


(define (State.modify func)
  (State.bind (λ (s) (StateValue (func s) #f)) (λ (_) (State.get))))

(define (procedurize el)
  (match el
    ((? procedure?) el)
    ((? output-port?) #{copy-port (open-input-string* %) el})
    (_ (const el))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; bind and return for State monad
    (define bind State.bind)
    (define return State.return)

(define-syntax-rule (simple-monad #| (bind_ return_) |# arg ... last)
  (let ()
    (comp
     (do! (State.return (procedurize arg))) ... last)))

(define result-monad (simple-monad (const 33)
                                   add1
                                   displayln))

(define result-monad (result #f))

(StateValue-state result-monad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; like F# computation expressions
;(define bind State.bind)
;(define return State.return)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(define chain2 (comp
                (x <- State.get)
                (let y (string-length x))
                (format "x = \"~a\"~ny = ~a" x y)))


;(match-define (StateValue finalState finalValue) (State.run chain2 14))
;(printfn "Final value:~n~a~n"  finalValue)
;(printfn "Final state: ~a" finalState)

