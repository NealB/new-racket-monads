#lang racket
(require racket/control)
(require "monad-run.rkt")

(define (ensure-list v) (if (list? v) v (list v)))

(define (append-log next-log prev-log)
  (append (ensure-list next-log) (ensure-list prev-log)))
  
(define (Writer-bind prev func)
  (let* ((pval (car prev))
         (plog (cdr prev))
         (next (func pval))
         (nval (car next))
         (nlog (cdr next)))
    (cons nval (append-log nlog plog))))
         
(define (Writer-return v) (cons v '()))

(define (Writer-log msg . args)
  (cons #f (apply format msg args)))

(define (Writer-format w)
  (apply ~a (map (curry format "~a~n") (reverse (cdr w)))))


(provide Writer-bind Writer-return Writer-log Writer-format)
