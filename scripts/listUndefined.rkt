#!/usr/bin/env racket
#lang racket

;; To make 'lib' available, add TEBenchmark's 'script' dir to PLTCOLLECT env var
(require json)
(require lib/impure)
(require lib/normalise)
(require lib/tip)
(require lib/util)

(define sample
  (let ((names (getenv "SAMPLE")))
    (if names
        (map string->symbol (string-split names "\n"))
        (error "No SAMPLE env var given"))))

(define destructor-functions
  (filter (lambda (name)
            (string-prefix? (decode-string (symbol->string name))
                            "destructor-"))
          sample))

(when (null? destructor-functions)
  (eprintf "No destructor functions; short-circuiting\n")
  (write-to-out "")
  (exit))

(eprintf "Finding undefined cases for destructors ~a\n" destructor-functions)

;; Associate each destructor function with the destructor it wraps
(define destructors
  (foldl (lambda (func result)
           (hash-set result func
                     (encode-lower-name
                      (string->symbol
                       (substring (decode-string (symbol->string func))
                                  (string-length "destructor-"))))))
         (hash)
         destructor-functions))

;; This should come from the te-benchmark version used to gather these samples
;; (otherwise the sampled names might not exist, e.g. if different canonical
;; names were chosen)
(define tip-benchmark
  (with-input-from-string
    (string-append "(" (file->string (getenv "tipBenchmark")) ")")
    read))

(eprintf "Read ~a benchmark entries\n" (length tip-benchmark))

;; Find the TIP expression which defines each destructor. Try to only traverse
;; tip-benchmark once.
(define type-defs
  (foldl (lambda (entry result)
           (define defined
             (expression-destructors entry))

           (foldl (lambda (func result)
                    (if (member (hash-ref destructors func) defined)
                        (hash-set result func entry)
                        result))
                  result
                  destructor-functions))
         (hash)
         tip-benchmark))

(define constructors-of
  (foldl (lambda (func result)
           (hash-set result func
                     (expression-constructors (hash-ref type-defs func))))
         (hash)
         destructor-functions))

(define (replace-with-range f lst)
  (map f (range 0 (length lst))))

(define patterns
  (foldl (lambda (func result)
           (define (contains-destructor? x)
             (member (hash-ref destructors func)
                     (symbols-in x)))

           (define types-defined-alongside-destructor
             (match (hash-ref type-defs func)
               [`(declare-datatypes ,_ ,types) types]))

           (define type-containing-destructor
             (first (filter contains-destructor?
                            types-defined-alongside-destructor)))

           (define all-constructor-defs
             (rest type-containing-destructor))

           (define other-constructors
             (filter (compose not contains-destructor?)
                     all-constructor-defs))

           ;; Patterns look like this:
           ;;   destructor-foo(Constructor1(free1, free2))
           (define (constructor->pattern c)

             ;; Replace any constructor args with free variables. We assume that
             ;; there's nothing in context called 'free0', 'free1', etc., which
             ;; is a safe assumption if we're dealing with hex-encoded names.
             (define constructor-args
               (string-join (replace-with-range (lambda (n) (format "free~s" n))
                                                (rest c))
                            ", "))

             ;; If the constructor is nullary, don't call it
             (define constructor-call
               (if (equal? 1 (length c))
                   (format "~s"     (first c))
                   (format "~s(~s)" (first c) constructor-args)))

             (define destructor-call
               (format "~s(~s)" func constructor-call))

             ;; The final result, for splicing into IsaCoSy calls
             (format "Trm.change_frees_to_fresh_vars @{term \"~s\"}"
                     destructor-call))

           (hash-set result func
                     (map constructor->pattern other-constructors)))
         (hash)
         destructor-functions))

(define pattern-list
  (append-map (lambda (func) (hash-ref patterns func)) destructor-functions))

(eprintf (format "Writing ~a patterns to output ~a\n"
                 (length pattern-list)
                 (getenv "out")))
(write-to-out (string-join pattern-list ",\n"))
