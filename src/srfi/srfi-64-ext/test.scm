(define-module (srfi srfi-64-ext test)
  #:use-module (srfi srfi-64)

  #:re-export (test-error
               test-equal
               test-group
               test-assert
               test-expect-fail)

  #:export (test?)

  #:export-syntax (define-test))

(define-syntax define-test
  (syntax-rules ()
    ((_ test-name e ...)
     (begin
       (define-public (test-name) e ...)
       (set-procedure-property! test-name 'srfi-64-test? #t)))))

(define (test? proc)
  "Checks if PROC is a test."
  (and (procedure? proc) (procedure-property proc 'srfi-64-test?)))
