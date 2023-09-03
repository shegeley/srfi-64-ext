(define-module (srfi srfi-64-ext formatting)

  #:export (color
            bold
            red
            green
            yellow
            magent
            headline))

(define (color code str)
  (string-append
   (string #\esc)
   "["
   (number->string code)
   "m"
   str
   (string #\esc)
   "[0m"))

(define (bold str)
  (color 1 str))

(define (red str)
  (color 31 str))

(define (green str)
  (color 32 str))

(define (yellow str)
  (color 33 str))

(define (magenta str)
  (color 35 str))

(define (headline text color)
  "Display headline TEXT in COLOR. COLOR is a function that wraps a
given string in an ANSI escape code."
  (display (color (string-append "==== " text)))
  (newline))
