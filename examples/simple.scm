(define-module (examples simple)
  #:use-module (srfi srfi-64-ext test))

(define (f x) (* x 2))

(define-test simple-example
  (test-group "simple example"
    (test-equal (f 3) 6)
    (test-equal (f 3) 7)))
