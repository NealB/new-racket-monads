#lang curly-fn racket
(require racket/control srfi/1 racket/generator)
(require "pipe.rkt")
(require "../racket-utilities/shell-like.rkt")
(require "../racket-utilities/utilities-dev.rkt")



(define args-dft (~~> "--date 1/1/1970 --cost 500 --color 'sky blue' file1.ext file2.ext"
                      split-respecting-quotes))

(when-falsy $# (argv args-dft))

(define (tee fn)
  `(call ,fn))

(define results
  (~~> (current-command-line-arguments)
       #{vector-append % #(#f)}
       in-vector
       `(tee ,(λ (state)
                (display "State:\n")
                (pretty-print state)
                (newline)))
       sequence->generator))


(let loop ()
  (define result (results))
  (when result
    (display
     (if (string-prefix? result "-") "\n" " "))
     (display (~a " " result))
    (loop)))
        
