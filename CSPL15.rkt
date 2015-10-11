#lang at-exp racket/base
(require "exports.rkt")
(require syntax/strip-context
         racket/port
         syntax/modresolve)
(require (prefix-in r: scribble/reader))

(provide (rename-out [new-read read]
                     [new-read-syntax read-syntax]))



;;﻿｜ Ｒｅａｄｅｒ ｜

(define (make-$-readtable)
  
  (define read-dollar
    (case-lambda
      [(ch in)
       (let* [[prefix (open-input-string "@begin/text{\n   ")]
              [block  (r:read (input-port-append #f prefix in))]]
         
         (append block '(#\newline)))]
      
      [(ch in src line col pos)
       (let* [[prefix (open-input-string "@begin/text{\n   ")]
              [block (r:read-syntax src (input-port-append #f prefix in))]]
         
         (datum->syntax block
                        (append (syntax->list block) '(#\newline))))]))

  (make-readtable (current-readtable)
                  #\{ 'terminating-macro read-dollar))

 

(define (new-read in)
  (syntax->datum (new-read-syntax #f in)))


(define (new-read-syntax src in)
  
  (define (read-syntax-all src port)
    (define (get-expr x)
      (define y (read-syntax src x))
      (unless (eof-object? y) (cons y x)))
    
    (unfold get-expr port))
  
  (strip-context
   (parameterize ([current-readtable (make-$-readtable)])
     #`(module main (file #,(path->string
                             (resolve-module-path "exports.rkt" __FILE__)))
         (require scribble/text)
         (require syntax/location)
         (require racket/cmdline)
         (require racket/file)
         (define export-only? (make-parameter #f))
         (define unit-test?   (make-parameter #f))
         
         (command-line
          #:program "CSPL15"
          #:once-any
          [("-e" "--exports") "Output header definitions only"
           (export-only? #t)]
          [("-t" "--test") "Run unit test"
           (unit-test? #t)])

         (include: "cspl15-rebind.h")
         (include: "cspl15-support.h")
         (include: <stdexcept>)
         
         #,@(read-syntax-all src in)
         
         (if (unit-test?)
             (run-unit-test #,(directory-from-path __FILE__)
                            #,src
                            (quote-module-path test))
             (export-all (export-only?)))))))



;; ﻿｜ Ｔｅｓｔ ｜
  
(module+ test 
  
  (define module-test
   (new-read (open-input-string "
      (displayln '====)

      (class: /Object)
      (ƒ: /Object/.public/set_m (name m)
          \"documenation for this function\"
          :: (virtual rw) => \"const char *\" → \"int\" → \"int\"
      {
          int st1;
          int st2;
          return 1234;
      })

      (module+ test
        (displayln \"entered test\")
        (include: <stdio.h>)
        (ƒ: /main (argc argv) ∷ \"int\" → \"char **\" → \"int\"
          (𝑣: x = \"Object()\")
          { printf(\"test: %d\\n\", x.set_m(\"xxx\", 0)); }))
      ")))
  
  module-test
  
  (eval module-test)
  
  (parameterize [[current-command-line-arguments (vector "-t")]]
    (dynamic-require ''main #f)))




