#!/usr/bin/env racket
#lang racket

;; To make 'lib' available, add TEBenchmark's 'script' dir to PLTCOLLECT env var
(require lib/util)
(require lib/tip)
(require json)
(require racket/hash)
(require racket/set)
(require rackunit)
(require syntax/macro-testing)

;; FIXME: This is redundant once we're on newer tebenchmarks which exposes it
(require/expose lib/tip (toplevel-function-names-in))

;; Test data

(define test-datatype
  "datatype 'a T1 = C1 | C2 \"'a\"")

(define test-definition
  "definition x :: \"T1\" where \"f = g\"")

(define test-function
  "function f :: \"T1 => T2\" where \"f x = g x\"
   sorry termination sorry")

(define test-mutual
  "function odds  :: \"'a List => 'a List\"
        and evens :: \"'a List => 'a List\"
   where \"odds  (nil      ) = nil              \"
       | \"odds  (cons x xs) = cons x (evens xs)\"
       | \"evens (nil      ) = nil              \"
       | \"evens (cons x xs) = odds xs          \"
   sorry termination sorry")

;; Data aquisition and transformation

(define (assert-equal x y msg)
  (unless (equal? x y)
    (error msg x y)))

(define (value->bool x)
  (not (not x)))

(define (strev s)
  (list->string (reverse (string->list s))))

(assert-equal (strev "hello")
              "olleh"
              "strrev reverses")

;; String containing Isabelle definitions, and nothing else
(define isabelle-defs-string
  (let* ((raw (file->string (getenv "tebIsabelle")))
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
  (let* ((datatypes   (split-at (list isabelle-defs-string) "datatype"))
         (functions   (split-at datatypes                   "function"))
         (definitions (split-at functions                   "definition")))
    ;; Filter out entries which only contain whitespace
    (filter (lambda (def)
              (not (empty? (string-split def))))
            definitions)))

;; Returns the first double-quoted substring from the given string
(define (first-quoted str)
  (substring (second (split-at (list str) "\"")) 1))

(assert-equal (first-quoted "fee fi \"fo fum\" tweedle dee \"tweedle dum\"")
              "fo fum"
              "first-quoted extracts as needed")

;; The keyword used to begin an Isabelle definition
(define (isabelle-keyword def)
  (car (string-split def)))

(assert-equal (isabelle-keyword test-function)
              "function"
              "Extracted Isabelle function keyword")

(assert-equal (isabelle-keyword test-definition)
              "definition"
              "Extracted Isabelle definition keyword")

(assert-equal (isabelle-keyword test-mutual)
              "function"
              "Extracted Isabelle function keyword from mutual recursion")

(define (defines-isabelle-value? def)
  (value->bool (member (isabelle-keyword def) (list "function" "definition"))))

(assert-equal (defines-isabelle-value? test-function)
              #t
              "Isabelle functions are values")

(assert-equal (defines-isabelle-value? test-definition)
              #t
              "Isabelle constants are values")

(assert-equal (defines-isabelle-value? test-datatype)
              #f
              "Isabelle types are not values")

;; We need to take mutual recursion into account
(define (isabelle-types-of def)
  (unless (defines-isabelle-value? def)
    (error "Cannot get type of " def))
  (map (lambda (part)
         (string-normalize-spaces
          (first-quoted (second (split-at (list part)
                                          "::")))))
       (split-at (list def) "and")))

(assert-equal (isabelle-types-of test-function)
              (list "T1 => T2")
              "Got Isabelle function type")

(assert-equal (isabelle-types-of test-definition)
              (list "T1")
              "Got Isabelle constant type")

(assert-equal (isabelle-types-of test-mutual)
              (list "'a List => 'a List"
                    "'a List => 'a List")
              "Got types of mutually recursive functions")

;; We need to take mutual recursion into account
(define (isabelle-names-of def)
  (unless (defines-isabelle-value? def)
    (error "Cannot get name of " def))
  (map (lambda (part)
         ;; Trim leading whitespace, in case any is left over from 'and'
         (second (string-split (string-normalize-spaces part))))
       (split-at (list def) "and")))

(assert-equal (isabelle-names-of test-function)
              (list "f")
              "Can get function names")

(assert-equal (isabelle-names-of test-definition)
              (list "x")
              "Can get constant names")

(assert-equal (isabelle-names-of test-mutual)
              (list "odds" "evens")
              "Can get mutually recursive names")

(define (isabelle-names-types-of def)
  (make-immutable-hash (foldl (lambda (name type result)
                                (cons (cons (string->symbol name) type)
                                      result))
                              '()
                              (isabelle-names-of def)
                              (isabelle-types-of def))))

(assert-equal (isabelle-names-types-of test-definition)
              (make-immutable-hash '((x . "T1")))
              "Can get name/type mapping from constant")

(assert-equal (isabelle-names-types-of test-function)
              (make-immutable-hash '((f . "T1 => T2")))
              "Can get name/type mapping from function")

(assert-equal (isabelle-names-types-of test-mutual)
              (make-immutable-hash '((odds  . "'a List => 'a List")
                                     (evens . "'a List => 'a List")))
              "Can get name/type mapping from mutually recursive functions")

(define all-smt
  (read-benchmark (file->string (getenv "smtlib"))))

(define all-function-names
  (append-map toplevel-function-names-in all-smt))

;; Some definitions are discarded since Isabelle doesn't understand them
(define to-strip
  (let ((fixes (string->jsexpr (file->string (getenv "FIXES")))))
    (map string->symbol
         (append (hash-ref (hash-ref fixes 'nontypes)   'encoded)
                 (hash-ref (hash-ref fixes 'dependents) 'encoded)))))

(define remaining-functions
  (remove* to-strip all-function-names))

(define isabelle-types-map
  (foldl (lambda (def result)
           (with-handlers
               ;; Catch-all and report which def it was
               ([(lambda (v) #t)
                 (lambda (v)
                   (error "Exception handling definition" def v))])
             (if (defines-isabelle-value? def)
                 (hash-union result (isabelle-names-types-of def))
                 result)))
         (make-immutable-hash '())
         isabelle-defs))

;; Sanity checks: definitions from Isabelle should match those of TIP

(let ((from-tip      (list->set remaining-functions))
      (from-isabelle (list->set (hash-keys isabelle-types-map))))
  (unless (set=? from-tip from-isabelle)
    (error "Mismatch between TIP and Isabelle function names"
           `(missing-from-isabelle . ,(set-subtract from-tip from-isabelle))
           `(extras-in-isabelle    . ,(set-subtract from-isabelle from-tip)))))

(define data
  (make-immutable-hash `((types . ,isabelle-types-map))))

;; Write out a JSON object, so jq can look up data for sampled names. If we have
;; an env var called "out" we use that as our output path (this allows us to use
;; this script as a Nix builder); otherwise we use stdout.

(define out-file
  (getenv "out"))

(if out-file
    (call-with-output-file out-file
      (curry write-json data))
    (write-json data))
