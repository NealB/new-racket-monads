#lang racket
(require racket/control)
(require "monad-run.rkt")

(struct Write-data (value log) #:prefab)

(define (ensure-list v) (if (list? v) v (list v)))

(define (append-log next-log prev-log)
  (append (ensure-list next-log) (ensure-list prev-log)))
  
(define (Writer-bind m f)
  (let* ((mval (Write-data-value m))
         (mlog (Write-data-log m))
         (nextm (f mval))
         (nmval (Write-data-value nextm))
         (nmlog (Write-data-log nextm)))
    (Write-data nmval (append-log nmlog mlog))))


(define (Writer-return v) (Write-data v '()))

(define (Writer-log msg . args)
  (cons #f (apply format msg args)))

(define (Writer-format w)
  (apply ~a (map (curry format "~a~n") (reverse (cdr w)))))


(provide Writer-bind Writer-return Writer-log Writer-format)
