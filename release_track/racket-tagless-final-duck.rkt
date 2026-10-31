#lang racket

(define (free-return value)
  (vector (quote return) value))

(define (free-bind expression procedure)
  (vector (quote bind) expression procedure))

(define (free-croak)
  (vector (quote croak)))

(define (free-jump)
  (vector (quote jump)))

(define (free-eat-fly)
  (vector (quote eat-fly)))



(define (program return bind croak jump eat-fly)

  (define (sequence operation . operations)
    (if (null? operations)
        operation
        (bind operation
              (lambda (_)
                (apply sequence operations)))))

  (sequence
   (jump)
   (croak)
   (eat-fly)))

#| Define specific interpreter |#

(define (simulator-return value)
  (lambda ()
    value))

(define (simulator-bind expression procedure)
  (lambda ()
    ((procedure (expression)))))

(define (simulator-croak)
  (lambda ()
    (display "Ribbit!")
    (newline)))

(define (simulator-jump)
  (lambda ()
    (display "Froggy jumps up!")
    (newline)))

(define (simulator-eat-fly)
  (lambda ()
    (display "Yum, a fly!")
    (newline)))

#| Pass interpreter as arguments to the program |#

(define executable
  (program simulator-return
           simulator-bind
           
           simulator-croak
           simulator-jump
           simulator-eat-fly))

#| Run the program |#

(executable)
