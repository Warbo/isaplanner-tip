#!/usr/bin/env racket
#lang racket

;; To make 'lib' available, add TEBenchmark's 'script' dir to PLTCOLLECT env var
(require json)
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

(eprintf "DESTRUCTORS ARE ~a\n" destructors)

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

           ;; FIXME: Not sure what pattern must be given to Isabelle. Once we
           ;; find out, update this to generate it.
           (define (constructor->pattern c)
             (cons (car c)
                   (map (lambda (destructor) '_)
                        (cdr c))))

           (hash-set result func
                     (map constructor->pattern other-constructors)))
         (hash)
         destructor-functions))

(eprintf "PATTERNS ARE ~a\n" patterns)

(exit 1)
