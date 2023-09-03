(define-module (srfi srfi-64-ext reporters)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-64-ext formatting)
  #:export (run64-report))

(define (run64-report runner)
  (unless (zero? (test-runner-fail-count runner))
    (headline "FAILURES" red)
    (for-each (lambda (failure)
                (let ((name (assq-ref failure 'test-name))
                      (file (assq-ref failure 'source-file))
                      (line (assq-ref failure 'source-line)))
                  (when file
                    (display file)
                    (display ":")
                    (when line
                      (display line)
                      (display ":"))
                    (display " "))
                  (display name)
                  (newline)))
              (test-runner-aux-value runner))
    (newline))
  (headline
   (string-join
    (filter-map (lambda (count text color)
                  (if (zero? count)
                      #f
                      (color (string-append (number->string count)
                                            " " text))))
                (list (test-runner-pass-count runner)
                      (test-runner-fail-count runner)
                      (test-runner-xpass-count runner)
                      (test-runner-xfail-count runner)
                      (test-runner-skip-count runner))
                (list "passed" "failed"
                      "unexpected passes"
                      "expected failures"
                      "skipped")
                (list green red yellow yellow yellow))
    ", ")
   (cond
    ((not (zero? (test-runner-fail-count runner)))
     red)
    ((or (not (zero? (test-runner-xpass-count runner)))
         (not (zero? (test-runner-xfail-count runner))))
     yellow)
    (else green))))
