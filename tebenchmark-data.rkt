#lang racket

;; To make 'lib' available, add TEBenchmark's 'script' dir to PLTCOLLECT env var
(require lib/util)
(require lib/tip)
(require json)

;; Data aquisition and transformation

(define (assert-equal x y msg)
  (unless (equal? x y)
    (error msg x y)))

(define (strev s)
  (list->string (reverse (string->list s))))

(assert-equal (strev "hello")
              "olleh"
              "strrev reverses")

;; String containing Isabelle definitions, and nothing else
(define isabelle-defs-string
  (let* ((raw (file->string (string-append (getenv "tebIsabelle")
                                           "/A.thy")))
         (pre (string-replace raw         #rx".*begin" "" #:all? #f))
         (rev (string-replace (strev pre) #rx"dne"     "" #:all? #f)))
    (strev rev)))

;; Split each string in the given list, before each occurrence of sep,
;; and concatenate the resulting lists.
(define (split-at strs sep)
  (append-map (lambda (x)
                (let ((headless (string-split x sep #:trim? #f)))
                  (cons (car headless)
                        (map (curry string-append sep)
                             (cdr headless)))))
              strs))

(assert-equal (split-at '("foo" "bar" "baz") "a")
              '("foo" "b" "ar" "b" "az")
              "split-at works")

;; List of Isabelle definitions (including datatypes and functions)
(define isabelle-defs
  (let* ((datatypes (split-at (list isabelle-defs-string) "datatype"))
         (functions (split-at datatypes                   "function")))
    (split-at functions "definition")))

;; (define (isabelle-type-of f)
;;   #f)


(define all-smt
  (read-benchmark (file->string (getenv "smtlib"))))

(define all-function-names
  (lowercase-names all-smt))

;; Some definitions are discarded since Isabelle doesn't understand them
(define to-strip
  (let ((fixes (string->jsexpr (file->string (getenv "FIXES")))))
    (append (hash-ref (hash-ref fixes 'nontypes)   'encoded)
            (hash-ref (hash-ref fixes 'dependents) 'encoded))))

(define remaining-functions
  (remove* to-strip all-function-names))

;; (define function-types
;;   (map (lambda (f) (tip-type-to-isabelle (type-of (def-of f))))
;;        remaining-functions))

(define data
  `((function-names . ,(map symbol->string remaining-functions))
    (isabelle-defs  . ,isabelle-defs)))

;; Write out a JSON object, so jq can look up data for sampled names

(write-json (make-immutable-hash data))
