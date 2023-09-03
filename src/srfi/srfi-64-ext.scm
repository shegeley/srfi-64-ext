(define-module (srfi srfi-64-ext)

  #:use-module (srfi srfi-64-ext runners)
  #:use-module (srfi srfi-64-ext test)

  #:use-module (guix discovery)
  #:use-module (srfi srfi-64)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 ftw)

  #:export
  (%previous-runner
   get-test-module
   get-module-tests

   rerun-tests
   run-test
   run-module-tests
   run-project-tests
   run-project-tests-cli

   default-resolver
   test-runner-summary))

(define (default-resolver module)
  (let [(l (last (module-name module)))]
    (if (string-suffix?
         "-test"
         (symbol->string l))
        l
        (symbol-append l '-test))))

(define* (get-test-module
          #:optional (module* (current-module))
          #:key (resolver default-resolver))
  ;; TODO: Handle the case, when test module doesn't exist.
  "Return a test module related to the current one.  Usually it's a module with
-test prefix.  Return current module if it already contains -test prefix."
  (resolve-module
   (resolver module*)))

(define (test-runner-summary runner)
  "Return alist of helpful statistics for the test-runner RUNNER."
  `((pass . ,(test-runner-pass-count runner))
    (xfail . ,(test-runner-xfail-count runner))
    (xpass . ,(test-runner-xpass-count runner))
    (fail . ,(test-runner-fail-count runner))))

(define (test-runner-test-results-stack runner)
  (or (assoc-ref (test-runner-aux-value runner) 'test-results-stack)
      '()))

(define (test-runner-test-results runner)
  (reverse (test-runner-test-results-stack runner)))

(define (record-test-run-result runner old-fail-count t)
  (let* ((aux-value (test-runner-aux-value runner))
         (test-result (if (< old-fail-count (test-runner-fail-count runner))
                          `((test . ,t) (status . fail))
                          `((test . ,t) (status . pass))))
         (test-results-stack (test-runner-test-results-stack runner))
         (new-test-results (cons test-result test-results-stack)))
    (test-runner-aux-value! runner
                            (assoc-set! aux-value 'test-results-stack new-test-results))))

(define* (run-test
          t
          #:key (runner (test-runner-create)))
  (let ((old-fail-count (test-runner-fail-count runner)))
    (test-with-runner runner
      (t)
      (record-test-run-result runner old-fail-count t)))
  runner)

(define (get-module-tests module)
  (fold-module-public-variables
   (lambda (variable acc)
     (if (test? variable) (cons variable acc) acc))
   '()
   (list module)))

(define* (run-module-tests
          module
          #:key (runner (test-runner-create)))
  (define module-tests (get-module-tests module))
  ;; TODO: Load test module if it's not loaded yet.
  ;; (reload-module module)
  (test-with-runner runner
    (let ((test-name (format #f "module ~a" (module-name module))))
      (test-group test-name
        (map (lambda (t) (run-test t #:runner runner)) module-tests))))
  runner)

(define* (run-project-tests
          test-modules
          #:key
          (runner (test-runner-default)))
  (test-with-runner runner
    (test-group "PROJECT TEST"
      (map (lambda (m)
             (let ((module-tests (get-module-tests m)))
               (when (not (null? module-tests))
                 (run-module-tests m #:runner runner))))
           test-modules)))
  runner)

(define* (run-project-tests-cli test-modules #:rest keys)
  (let* ((summary (test-runner-summary (apply run-project-tests (cons test-modules keys))))
         (fail-count (assoc-ref summary 'fail)))
    (exit (zero? fail-count))))

(define* (rerun-tests
          previous-runner
          #:key
          (runner (test-runner-create))
          (filter-fn (const #t)))
  (when previous-runner
    (let* ((test-results (test-runner-test-results previous-runner))
           (get-test (lambda (x) (assoc-ref x 'test)))
           (filtered-tests (map get-test (filter filter-fn test-results))))
      (test-with-runner runner
        (test-group "RERUN TESTS"
          (map (lambda (t) (run-test t #:runner runner)) filtered-tests)))))
  runner)

;; NOTE: COMMENTS FROM RDE
;; (set! %previous-runner (run-project-tests))
;; (rerun-tests %previous-runner
;;              #:filter-fn (lambda (x) (equal? 'fail (assoc-ref x 'status))))

;; (module-clear! (resolve-module '(rde serializers nginx-test)))
;; (get-module-tests (resolve-module '(rde serializers nginx-test)))

;; (run-module-tests (resolve-module '(rde serializers nginx-test)))

;; (use-modules (ice-9 pretty-print))

;; (rerun-tests %previous-runner)
;; (define prev-runner (test-runner-default))

;; (let ((runner %previous-runner))
;;   (run-module-tests
;;    (resolve-module '(rde serializers nginx-test))
;;    #:runner runner)
;;   (pretty-print (test-runner-test-results runner)))

;; (re-run-failed-tests prev-runner)

;; https://www.mail-archive.com/geiser-users%40nongnu.org/msg00323.html
;; https://rednosehacker.com/revisiting-guile-xunit

;; Test runners:
;; https://github.com/aconchillo/guile-json/blob/master/tests/runner.scm
;; https://luis-felipe.gitlab.io/guile-proba/
;; https://git.systemreboot.net/run64/tree/bin/run64
;; https://framagit.org/Jeko/guile-spec

;; Common lisp testing frameworks:
;; https://sabracrolleton.github.io/testing-framework

;; Clojure testing libraries:
;; https://jakemccrary.com/blog/2014/06/22/comparing-clojure-testing-libraries-output/

;; Scheme testing libraries:
;; https://github.com/tali713/mit-scheme/blob/master/tests/unit-testing.scm
;; https://code.call-cc.org/svn/chicken-eggs/release/5/test/trunk/test.scm

;; (define-test our-super-test-suite
;;   (test-group "Something"
;;     (few asserts)))

;; (run-tests
;;  test-runner
;;  ;; (select-all-loaded-tests)
;;  (select-only-every-second-loaded-test))

;; TODO:
;; - Make test-assert to show line, where it fails.
;; - Implement test-match, which uses ice-9 match like patterns and provides
;;   meaningful report.

;; - Write ADR for serializers or/and implement template/interface for serializers.
