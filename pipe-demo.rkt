#lang curly-fn racket
(require racket/control srfi/1 racket/generator)
(require "monad-run.rkt")
(require "pipe.rkt")
(require "../racket-utilities/shell-like.rkt")
(require "../racket-utilities/utilities-dev.rkt")

(define bind! (make-effect! Pipe-bind))

(define (procedurize el)
  (match el
    ((? procedure?) el)
    (_ (const el))))

(define (~~> . args)
  (let** ((prc (map procedurize args))
          (bnd (map (curry bind!) prc))
          (rev (reverse bnd))
          (_   (apply compose rev)))))

(define args-dft ((~~> "--date 1/1/1970 --cost 500 --color 'sky blue' file1.ext file2.ext"
                       split-respecting-quotes)))

(when-falsy $# (current-command-line-arguments args-dft))

(define ~run~> (compose run ~~>))

(define results
  (~run~> (current-command-line-arguments)
          ;(list 'error "this is only a test")
          #{vector-append % #(#f)}
          in-vector
          sequence->generator))


(let loop ()
  (define result (results))
  (when result
    (displayln (~a "result: " result))
    (loop)))
        
