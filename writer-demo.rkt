#lang racket
(require racket/control)

(define-syntax-rule (show-and-do op arg1 arg2)
  (let ()
    (printf "~s ~s ~s~%" op arg1 arg2)
    (op arg1 arg2)))

(struct Write-data (value log)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc data port mode)
     (fprintf port "value=~s; log=~s" (Write-data-value data) (Write-data-log data)))])


(define (make-write-data value log)
  (Write-data value log))

(define (ensure-list v) (if (list? v) v (list v)))

  
(define (Writer-bind m f)
  (let* ((mval (Write-data-value m))
         (nextm (f mval))
         (nmval (Write-data-value nextm))
         (mlog (Write-data-log m))
         (nmlog (Write-data-log nextm)))
    (Write-data nmval (cons mlog nmlog))))

(define (Writer-return v) (Write-data v '()))

(define (add5 x i)
  (Write-data (+ 5 x) (format "Add 5 to ~a (i=~a)" x i)))

(define-syntax-rule (Writer-return* exprs ...)
  (reset (let () exprs ...)))

;; testing...................
(module+ main
  
  (define results
    (Writer-return*
     
     (define init 0)


     (let* ((a (shift k (Writer-bind (add5 init 0) k)))
            (a (shift k (Writer-bind (add5 a 1) k)))
            (a (shift k (Writer-bind (add5 a 2) k)))
            (a (shift k (Writer-bind (add5 a 3) k))))

       (Writer-return a))))


  (display "results:\n")
  (pretty-print results)
  (display "done\n")
  (newline))
  