(define-module (srfi srfi-64-ext utils)
  #:use-module (srfi srfi-1)

  #:export (string-repeat))

(define (string-repeat s n)
  "Returns string S repeated N times."
  (fold (lambda (_ str) (string-append str s)) "" (iota n)))
