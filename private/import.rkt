#lang racket/base
(require racket/list)
(require racket/generic (for-syntax racket/base syntax/parse))
(require data/queue)
(provide import-list
         import-list-export
         import-list-require
         import:
         include:
         include-head:)

(define-generics c-import
  (gen-c-import c-import)
  (gen-name c-import))

(struct info:import (name)
 #:methods gen:c-import
    [(define (gen-c-import nfo)
       (format "#include \"generated/~a.h\""
               (info:import-name nfo)))
     
     (define (gen-name nfo)
       (info:import-name nfo))])

(struct info:include (name)
 #:methods gen:c-import
    [(define (gen-c-import nfo)
       (format "#include ~s"
               (info:include-name nfo)))
     
     (define (gen-name nfo)
       (info:include-name nfo))])

;;;;;;;;;;;

(define (import: . xs)
  (for-each (λ (x)
         (enqueue! (import-list) (info:import x)))
       xs))

(define-syntax (include-head: stx)
  (syntax-parse stx
    [(_ x ...)
     #'(begin
        (enqueue-front! (import-list) (info:include 'x)) ...)]))

(define-syntax (include: stx)
  (syntax-parse stx
    [(_ x ...)
     #'(begin
        (enqueue! (import-list) (info:include 'x)) ...)]))

;;;;;;;;;;;

(define import-list
  (make-parameter (make-queue)))

(define (import-list-export)
  (map gen-c-import (queue->list (import-list))))

(define (import-list-require)
  (define (f nfo)
    `(require ,(format "~a.cc.rkt" (gen-name nfo))))
  (map f
       (filter info:import? (queue->list (import-list)))))
  
(module+ test
  (require rackunit)
  
  (import: "matrix" "vector")
  (include: <stdlib.h>)
  (include: "local.h")
  (include-head: "first.h")
  
  (check-equal?
   (import-list-export)
   '("#include \"first.h\""
     "#include \"generated/matrix.h\""
     "#include \"generated/vector.h\""
     "#include <stdlib.h>"
     "#include \"local.h\""))
  
  (check-equal?
   (import-list-require)
   '((require "matrix.cc.rkt") (require "vector.cc.rkt"))))
