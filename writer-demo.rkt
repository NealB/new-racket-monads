#lang racket
(require racket/control)
(require "monad-run.rkt")
(require "writer.rkt")


(define (add5 x)
  (cons (+ 5 x) (format "Add 5 to ~a" x)))

(define (write-log msg . args)
  (cons #f (apply format msg args)))

(define results1
  (Writer.do
   (λ (!)
     (let* ((a (! Writer.return 34))
            (b (! add5 a))
            (c (! add5 b))
            (d (! add5 19))
            (e (! add5 2)))

       #f)

     (for ((i 3))
       (! write-log "final log message ~a" i))
     )))

(display (~a "results:\n" results1 (~a "done\n")))
(newline)
