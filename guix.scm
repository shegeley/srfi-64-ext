(use-modules
 (guix gexp)
 ((guix licenses) #:prefix license:)
 (guix packages)
 (guix git-download)
 (guix download)
 (guix utils)
 (guix build-system guile)
 (gnu packages guile)
 (gnu packages package-management))

(define %source-dir
  (dirname (current-filename)))

(define-public guile-srfi-64-ext
  (package
   (name "guile-srfi-64-ext")
   (version "0.0.1")
   (source
    (local-file
     %source-dir
     #:recursive? #t
     #:select? (git-predicate %source-dir)))
   (build-system guile-build-system)
   (inputs '())
   (arguments
    (list
     #:source-directory "src"
     #:compile-flags '(list
                       "--r6rs"
                       "-Wunbound-variable"
                       "-Warity-mismatch")))
   (native-inputs
    (list
     guile-3.0-latest
     guix))
   (synopsis
    "A little testing framework build around (srfi srfi-64)")
   (description
    "Simple (srfi srfi-64) wrappers from Andrew's Tropin RDE project to make testing easier in any Guile Scheme project")
   (license license:gpl3+)
   (home-page "https://github.com/shegeley/srfi-64-ext")))

guile-srfi-64-ext
