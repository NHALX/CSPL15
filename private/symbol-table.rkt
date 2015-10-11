#lang racket/base
(require racket/dict
         racket/list
         racket/string
         racket/generic
         racket/path
         racket/match
         racket/function
         (only-in scribble/text output)
         syntax/parse (for-syntax syntax/parse))

(require "../../misc/tree.rkt")
(require "../../misc/NHA.rkt")

;; TODO: contracts/documentation
(provide symbol-table
         with-empty-symbol-table
         current-symbol-path
         symbol-table-empty
         symbol-table-ref
         symbol-table-set!
         symbol-table-set!*
         symbol-table-export
         symbol-table-render
         symbol-table-print
         ss->full-path
         ss->path
         ss->scope
         ss->string
         ss-append
         ss-last-node
         ss-strip-hidden
         gen-key
         gen-c-declaration
         m-xf
         info:block
         gen:c-declaration)


;; ＤＡＴＡ， ＩＮＴＥＲＦＡＣＥＳ， ＰＡＲＡＭＥＴＥＲＳ 

(define-generics c-declaration
  (gen-c-declaration c-declaration xs)
  (gen-key           c-declaration))

(struct info:comment (key txt)
  #:transparent
  #:methods gen:c-declaration
  [(define (gen-c-declaration nfo _)  (format "/* ~a */" (info:comment-txt nfo)))
   (define (gen-key nfo)              (ss->path (info:comment-key nfo)))])

(struct info:block (key)
  #:transparent
  #:methods gen:c-declaration
  [(define (gen-c-declaration nfo xs) xs)
   (define (gen-key nfo)              (ss->path (info:block-key nfo)))])


(define current-symbol-path
  (make-parameter (string->path "/")))

(define (symbol-table-empty)
  (tree (info:block (string->path "/")) empty))

(define symbol-table
  (make-parameter (symbol-table-empty)))

(define-syntax-rule (with-empty-symbol-table f ...)
  (parameterize [[symbol-table (symbol-table-empty)]]
    (begin f ...)))


;; ＴＥＳＴ： ＣＯＭＭＯＮ 

(module+ test
  (require rackunit)

  (define some-comment
    (info:comment (string->path "some-comment") "test"))
  
  (define reference-tree
     (tree (info:block (string->path "/"))
           (list (tree some-comment empty)))))



;; ＥＸＰＯＲＴ 

(define (symbol-table-export #:filter [want? (const #t)])
  (define (f node)
    (match-define (tree x xs)
      node)
    (if (want? x)
        (gen-c-declaration x (map f xs))
        empty))
  
  (f (symbol-table)))

(module+ test
  (with-empty-symbol-table
    (symbol-table-set! "/" some-comment)
    (check-equal? (symbol-table-export)
                  (list "/* test */"))))


(define (symbol-table-print)
  (tree->mathematica-graph (tree-map gen-key (symbol-table))))
            
(define (symbol-table-render f)
  (with-empty-symbol-table
    (f)
    (define out (open-output-string))
    (output (symbol-table-export) out)
    (get-output-string out)))


;; ＵＴＩＬ 

(define (m-xf fmt v #:reference [r ""])
  (if (equal? r v)
      ""
      (format fmt v)))

(module+ test
  (check-equal? (m-xf "~a +++" "x")
                "x +++")
  (check-equal? (m-xf "~a +++" "")
                ""))


;; ＳＹＭＢＯＬ： Ｐａｔｈ Ｍａｎｉｐｕｌａｔｉｏｎ 

(define (ss->path x)
  
  (define (symbol->path x)
    (string->path (symbol->string x)))
  
  (cond
       [(symbol? x) (symbol->path x)]
       [(string? x) (string->path x)]
       [else        x]))

(define (ss->string s)
  (cond
    [(path? s)   (path->string s)]
    [(symbol? s) (symbol->string s)]
    [(string? s) s]))

(define ss-last-node
 (∘ value:1 split-path ss-strip-hidden ss->path))

(define (ss-append . xs)
  (string->path (apply string-append (map ss->string xs))))


(define (ss->full-path x)
  (define path
    (ss->path x))
  
  (if (relative-path? path)
      (build-path (current-symbol-path) path)
      path))

(module+ test
  (check-equal? (ss->full-path "some/thing")
                (string->path "/some/thing")))



(define (ss-strip-hidden s)
  #;((∘ (⤷ string-join "/")
        (⤶ filter-not (⤶ regexp-match #px"\\.."))
        (⤷ string-split "/" #:trim? #f))
  s)
  (define (same x)
    (cond
      [(path? s)   (string->path x)]
      [(symbol? s) (string->symbol x)]
      [(string? s) x]))
    
  (same
   (regexp-replace* #px"(/|^)(\\.[^/]*/?)"
                    (ss->string s) "\\1")))

(module+ test
  (test-case
   "ss-strip-hidden"
   (check-equal? (ss-strip-hidden "/test/.public/")
                 "/test/")
   (check-equal? (ss-strip-hidden "test/.public")
                 "test/")
   (check-equal? (ss-strip-hidden "/test/.public/something/")
                 "/test/something/")
   (check-equal? (ss-strip-hidden "/./test/...../1")
                 "/test/1")
   (check-equal? (ss-strip-hidden "/test/x.../1")
                 "/test/x.../1")   
   (check-equal? (ss-strip-hidden ".private/test/...../1")
                 "test/1")))



(define (ss->scope s)
  ((∘ (⤷ string-replace "/" "::")
      (⤷ string-trim "/")
      ss->string
      ss-strip-hidden)
   s))

(module+ test
  (check-equal? (ss->scope "/test/.public/field")
                "test::field"))



;; ＳＹＭＢＯＬ： ＧＥＴ／ＳＥＴ 

(define (symbol-table-ref path)
  
  (define v
    (tree-ref (symbol-table)
              (explode-path (ss->path path)) #:key gen-key))
  (and v (tree-value v)))

(module+ test
  (with-empty-symbol-table
   (define block
     (info:block "bind_attributes_-2714433558703842513"))
   
    (symbol-table-set! "/" block)
    (check-equal? (symbol-table-ref "/bind_attributes_-2714433558703842513")
                  block)))

  
;; Insert z under /x/y/ where path=/x/y/
(define (symbol-table-set! path v)
  (tree-insert! (symbol-table)
                (explode-path (ss->path path)) v #:key gen-key))

(module+ test
    
  (with-empty-symbol-table
    (symbol-table-set! "/" some-comment)
    (symbol-table-set! "/" some-comment)
    (check-equal? (symbol-table) reference-tree)))



;; Insert (f z) under /x/y/ where path=/x/y/z
(define (symbol-table-set!* path f)  
  (define-values (base tail _)
    (split-path (ss->full-path path)))
  (let [[x (f tail)]]
    (symbol-table-set! base x)
    x))

(module+ test
    
  (with-empty-symbol-table
    (symbol-table-set! "/" some-comment)
    (check-equal? (symbol-table) reference-tree)))




