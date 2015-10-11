#lang racket/base
(require (for-syntax racket/base))
(require (for-syntax syntax/parse))
(require syntax/strip-context
         syntax/srcloc
         syntax/location)
(require racket/file)
(require scribble/text (for-syntax scribble/text))
(require (prefix-in r: scribble/reader))

(require racket/match
         racket/port
         racket/dict
         racket/function)

(require "../misc/NHA.rkt"
         "../misc/monad.rkt"
         "../misc/shell-pipe.rkt"
         (for-syntax
          "../misc/NHA.rkt"
          "private/data.rkt"
          "private/type-sig.rkt"))

(require "private/symbol-table.rkt"
         "private/loops.rkt"
         "private/function.rkt"
         "private/import.rkt"
         "private/data.rkt")

(provide (all-from-out "private/function.rkt"
                       "private/symbol-table.rkt"
                       "private/import.rkt"
                       "private/data.rkt"
                       "private/loops.rkt"
                       "../misc/shell-pipe.rkt"
                       "../misc/monad.rkt"
                       "../misc/NHA.rkt")
         (for-syntax
          (all-from-out "../misc/NHA.rkt"
                        "private/data.rkt"
                        "private/type-sig.rkt")))


(provide (except-out (all-from-out racket/base))
         (for-syntax
          (all-from-out racket/base))
         (all-from-out racket/function)
         (all-from-out scribble/text))

 
;; ｜﻿﻿Ｌｉｎｅ Ｉｎｆｏ ｜

(provide __FILE__ __LINE__ C hash:__FILE__:__LINE__)
 
(define C list)

(define-syntax (__FILE__ stx)
  (with-syntax ([file (syntax-source stx)])
    (syntax-case stx ()
      [_ #''file])))

(define-syntax (__LINE__ stx)
  (with-syntax ([line (syntax-line stx)])
    (syntax-case stx ()
      [_ #'line])))

(define-syntax (hash:__FILE__:__LINE__ stx)
    (syntax-case stx ()
      [(_ prefix) #`(let [[h (equal-hash-code  
                              (format "~a:~a"
                                      #,(syntax-source stx)
                                      #,(syntax-line stx)))]]
             (if (< h 0 )
                 (string-append prefix "1" (number->string (abs h)))
                 (string-append prefix "0" (number->string h))))]))




;;｜ ﻿Ｅｘｐｏｒｔ ｜

(provide export-all)


(define (export-all [export-only? #f]
                    [port (current-output-port)])
  
  (output
   (string-join (import-list-export) "\n"
                #:after-last "\n")
   port)
  
  (output
   (symbol-table-export
    #:filter
    (if export-only?
        (∘ not info:ƒ-body?)
        (const #t)))
   
   port))





