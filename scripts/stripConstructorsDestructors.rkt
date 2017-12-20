#!/usr/bin/env racket
#lang racket

;; To make 'lib' available, add TEBenchmark's 'script' dir to PLTCOLLECT env var
(require lib/util)
(require lib/tip)
(require lib/conjectures)
(require json)
(require racket/hash)
(require racket/set)
(require rackunit)
(require syntax/macro-testing)

;; FIXME: This is redundant once we're on newer tebenchmarks which exposes it
(require/expose lib/tip (toplevel-function-names-in))

(require/expose lib/conjectures (equation-to-jsexpr
                                 parse-json-equation
                                 parse-json-equations))

(define given-eqs
  (parse-json-equations (port->string)))

(define allowed-names
  (let ((raw (getenv "SAMPLE")))
    (if raw (string-split raw)
            '())))

(when (empty? allowed-names)
  (error "No SAMPLE env var given, or it was empty"))

;; We only sample functions, including constructor and destructor wrappers, yet
;; Isabelle might find raw constructors anyway (which it find based on the
;; types involved). If this happens, we only want to keep those which correspond
;; to a sampled constructor wrapper.
(define allowed-wrappers
  (filter (lambda (name)
            (or (string-prefix? name "constructor-")
                (string-prefix? name (string-append "global"
                                                    (encode16 "constructor-")))
                (string-prefix? name "destructor-")
                (string-prefix? name (string-append "global"
                                                    (encode16 "destructor-")))))
          allowed-names))

;; Remove 'pre' from the start of 'str', if it appears
(define (strip-prefix str pre)
  (if (string-prefix? str pre)
      (substring str (string-length pre))
      str))

;; Remove any "constructor-" or "destructor-" prefix from 'str', even if hex
;; encoded
(define (strip-prefices str)
  (let ((con-enc (string-append "global" (encode16 "constructor-")))
        (des-enc (string-append "global" (encode16 "destructor-"))))
    (cond
      [(string-prefix? str "constructor-") (strip-prefix str "constructor-")]
      [(string-prefix? str "destructor-")  (strip-prefix str "destructor-")]
      [(string-prefix? str con-enc) (string-append
                                     "Global"
                                     (strip-prefix str con-enc))]
      [(string-prefix? str des-enc) (string-append
                                     "global"
                                     (strip-prefix str des-enc))])))

(define mapping
  (make-immutable-hash (foldl (lambda (wrapper result)
                                (cons (cons (string->symbol
                                             (strip-prefices wrapper))
                                            (string->symbol wrapper))
                                      result))
                              '()
                              allowed-wrappers)))

;; Replace a raw constructor or destructor with its wrapped version
(define (normalise-name name)
  (hash-ref mapping name name))

;; Replace all 'constant' names with normalised versions
(define (normalise-expr x)
  (match x
    [(list '~=    l r)    `(~=    ,(normalise-expr l) ,(normalise-expr r))]
    [(list 'apply l r)    `(apply ,(normalise-expr l) ,(normalise-expr r))]
    [(list 'variable _ _) x]
    [(list 'constant n t) `(constant ,(normalise-name n) ,t)]
    [(cons l r)           (cons    (normalise-expr l)  (normalise-expr r))]
    [_                    x]))

(define normalised-eqs
  (map normalise-expr given-eqs))

(define (all-names-in-syms expr)
  (match expr
    [(list '~=    l r)    (append (all-names-in-syms l) (all-names-in-syms r))]
    [(list 'apply l r)    (append (all-names-in-syms l) (all-names-in-syms r))]
    [(list 'constant n _) (list n)]
    [(list 'variable _ _) '()]
    [(cons l r)           (append (all-names-in-syms l) (all-names-in-syms r))]
    [_                    '()]))

(define (all-names-in expr)
  (map symbol->string (all-names-in-syms expr)))

(define (equation-allowed eq)
  (subset? (all-names-in eq) allowed-names))

;; Remove equations involving non-sampled names; in practice, this will be
;; constructors whose wrappers aren't in the sample.
(define allowed-equations
  (filter equation-allowed normalised-eqs))

;; Whether or not the sides of an equation are equivalent. This can happen if
;; IsaCoSy spots that a constructor function is equal to its corresponding
;; constructor. Since this is an artefact of our benchmarking methodology, we
;; strip these out to avoid penalising IsaCoSy's precision and recall.
(define (sides-distinct? eq)
  (match eq
    [(list '~= lhs rhs) (exprs-distinct? lhs rhs)]))

(define (exprs-distinct? x y)
  (match (list x y)
    [(list (list 'constant n1 _)
           (list 'constant n2 _)) #f]
    [(list (list 'variable i1 _)
           (list 'variable i2 _)) #f]
    [(list (list 'apply l1 r1)
           (list 'apply l2 r2))   (or (exprs-distinct? l1 l2)
                                      (exprs-distinct? r1 r2))]
    [(list (cons a1 b1)
           (cons a2 b2))          (or (exprs-distinct? a1 a2)
                                      (exprs-distinct? b1 b2))]
    [_                            (not (equal? x y))]))

(define distinct-eqs
  (filter sides-distinct? allowed-equations))

(write-json (map equation-to-jsexpr distinct-eqs))
