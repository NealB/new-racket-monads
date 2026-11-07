#lang racket
(require racket/control)
(require "../racket-utilities/utilities-dev.rkt")

(struct Write-data (value log)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc data port mode)
     (fprintf port "value=~s; log=~s" (Write-data-value data) (Write-data-log data)))])


(define (make-write-data value log)
  (Write-data value log))


(define (Writer-bind m f)
  (~> m
      (match-λ ((Write-data mval mlog)
                (Write-data (f mval) mlog)))
      (match-λ ((Write-data (Write-data mval mlog2) mlog1)
                (Write-data mval (cons mlog1 mlog2))))
      !))



(define (Writer-return v) (Write-data v '()))

(define-syntax-rule (Writer-return* exprs ...)
  (reset (let () exprs ...)))

(define (with-output-capture thunk)
  (let* ((os (open-output-string))
         (value (parameterize ((current-output-port os)) (thunk)))
         (str (get-output-string os))) 
    (Write-data value str)))


(define (add5 x i)
  (printf "Add 5 to ~a (i=~a)." x i)
  (define newx (+ 5 x))
  (printf " New=~a." newx)
  newx)

(define-namespace-anchor nsa)
(define ns (namespace-anchor->namespace nsa))

;; testing...................
(module+ main
  
  (define results
    (Writer-return*
     
     (define init 0)


     (let* ((a (shift k (Writer-bind (with-output-capture (thapply add5 init 0)) k)))
            (a (shift k (Writer-bind (with-output-capture (thapply add5 a 1)) k)))
            (a (shift k (Writer-bind (with-output-capture (thapply add5 a 2)) k)))
            (a (shift k (Writer-bind (with-output-capture (thapply add5 a 3)) k))))

       (Writer-return a))))


  (display "results:\n")
  (pretty-print results)
  (display "done\n")

  (eval `(define results ,results) ns))

  