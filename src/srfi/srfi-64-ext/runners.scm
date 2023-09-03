(define-module (srfi srfi-64-ext runners)

  #:use-module (srfi srfi-64-ext utils)
  #:use-module (srfi srfi-64-ext formatting)
  #:use-module (srfi srfi-64-ext reporters)

  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)

  #:export (test-runner-default
            test-runner-run64))

(define (default:on-group-begin runner name count)
  (format #t "~a> ~a\n"
          (string-repeat "-" (length (test-runner-group-stack runner)))
          name))

(define (default:on-group-end runner)
  (format #t "<~a ~a\n"
          (string-repeat "-"
                         (1- (length (test-runner-group-stack runner))))
          (car (test-runner-group-stack runner))))

(define (default:on-test-end runner)
  (format #t "[~a] ~a\n"
          (test-result-ref runner 'result-kind)
          (if (test-result-ref runner 'test-name)
              (test-runner-test-name runner)
              "<>"))
  (case (test-result-kind runner)
    ((fail)
     (if (test-result-ref runner 'expected-value)
         (format #t "~a:~a\n -> expected: ~s\n -> obtained: ~s\n\n"
                 (test-result-ref runner 'source-file)
                 (test-result-ref runner 'source-line)
                 (test-result-ref runner 'expected-value)
                 (test-result-ref runner 'actual-value))))
    (else #t)))

(define (default:on-final runner)
  (format #t "Source: ~a\nAsserts: pass = ~a, xfail = ~a, fail = ~a\n\n"
          (test-result-ref runner 'source-file)
          (test-runner-pass-count runner)
          (test-runner-xfail-count runner)
          (test-runner-fail-count runner)))

(define* (test-runner-default
          #:key (runner (test-runner-null))
          (on-group-begin default:on-group-begin)
          (on-group-end default:on-group-end)
          (on-test-end default:on-test-end)
          (on-final default:on-final))
  (test-runner-on-group-begin! runner on-group-begin)
  (test-runner-on-group-end! runner on-group-end)
  (test-runner-on-test-end! runner on-test-end)
  (test-runner-on-final! runner on-final)
  runner)

;;; run64
;; https://git.systemreboot.net/run64/tree/bin/run64

(define (run64:on-group-begin runner suite-name count)
  (when (null? (test-runner-group-stack runner))
    (headline "test session starts" bold))
  (display suite-name)
  (display " "))

(define run64:on-group-end
  (lambda _
    (newline)))

(define (run64:on-test-end runner)
  (let ((name (test-runner-test-name runner))
        (result (string-upcase
                 (symbol->string (test-result-kind runner))))
        (result-alist (test-result-alist runner)))
    (display (case (test-result-kind runner)
               ((pass) (green "."))
               ((fail) (red "F"))
               ((xfail xpass) (yellow "X"))
               ((skip) (yellow "S"))))
    (when (eq? (test-result-kind runner)
               'fail)
      ;; Prepend test failure details to aux value.
      (test-runner-aux-value!
       runner
       (cons (cons (cons 'test-name (test-runner-test-name runner))
                   (test-result-alist runner))
             (test-runner-aux-value runner))))))

(define (run64:on-final runner)
  (run64-report runner))

(define run64:aux-value '())

(define* (test-runner-run64
          #:key (runner (test-runner-null))
                (on-group-begin run64:on-group-begin)
                (on-group-end run64:on-group-end)
                (on-test-end run64:on-test-end)
                (on-final run64:on-final)
                (aux-value run64:aux-value))
  (test-runner-on-group-begin! runner on-group-begin)
  (test-runner-on-group-end! runner on-group-end)
  (test-runner-on-test-end! runner on-test-end)
  ;; Initialize aux value to the empty list.
  (test-runner-on-final! runner on-final)
  (test-runner-aux-value! runner aux-value)
  runner)
