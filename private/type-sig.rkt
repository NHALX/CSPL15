#lang racket/base
(require "../../misc/NHA.rkt")
(require syntax/parse
         racket/format
         racket/list
         net/base64
         file/sha1
         file/gunzip
         file/gzip)

(require (for-syntax racket/base
                     racket/list
                     syntax/parse))

(provide mangle
         mangle⁻¹
         mangle-sig
         (struct-out typesig)
         (for-syntax type-annotation))

(struct typesig (function linkage storage override access domain codomain)
  #:transparent)

(begin-for-syntax
  
  (define-syntax-class double-arrow
    (pattern (~or (~datum ⇒)
                  (~datum =>))))
  
  (define-syntax-class function-arrow
    (pattern (~or (~datum →)
                  (~datum ->))))
  
  (define-syntax-class datum-∷
    (pattern (~or (~datum ∷) 
                  (~datum ::))))  
  
  (define-syntax-class access-mode
    (pattern (~or (~datum r)
                  (~datum w)
                  (~datum rw))))

  (define-syntax-class linkage-specifier
    (pattern (~datum C-link)))

  (define-syntax-class storage-class
    (pattern (~or (~datum extern)
                  (~datum auto)
                  (~datum register)
                  (~datum static))))
  
  (define-syntax-class function-specifier
    (pattern (~or (~datum inline)
                  (~datum explicit)
                  (~datum virtual))))
  
  (define-syntax-class override-specifier
    (pattern (~or (~datum override)
                  (~datum final)
                  (~datum new)
                  (~datum pure))))
  
    
  (define-splicing-syntax-class type-annotation
    (pattern
     (~and
      (~seq :datum-∷
           (~optional
            (~seq
             ((~optional function:function-specifier)
              (~optional linkage:linkage-specifier)
              (~optional storage:storage-class)
              (~optional override:override-specifier)
              (~optional access:access-mode))
             :double-arrow))
           (~seq
            (~or domain:str
                 domain:id
                 domain:expr)
            (~seq _:function-arrow
                  (~or co-domain:str
                       co-domain:id
                       co-domain:expr)) ...))
      
      (~bind [sig
              #`(let-values
                    [[(args result)
                      (split-at-right
                       (cons domain (list co-domain ...))
                       1)]]
                      
                  (typesig
                   '#,(attribute function)
                   '#,(attribute linkage)
                   '#,(attribute storage)
                   '#,(attribute override)
                   '#,(attribute access)
                   args
                   (car result)))])))))


(module+ test
  (define-syntax (f stx)
    (syntax-parse stx
      [(_ type:type-annotation)     
       #`#,(attribute type.sig)]))
  
  (define xxxx "1111")
  
  (f ∷ (C-link static) => "int")
  (f ∷ (extern) => "int")
  (f ∷ "int")
  (mangle-sig
   (f :: (virtual)
     => "void * const"
     -> xxxx 
     -> "osg::Geometry *"
     -> "void *")))



(define mangle-sig
  (∘ mangle
     (⤷ ~a)
     (⤷ drop 1)
     vector->list
     struct->vector))

(define (mangle x) 
  (define out
    (open-output-bytes))
  
  (deflate (open-input-bytes
            (string->bytes/utf-8 x)) out)
  
  (bytes->hex-string (get-output-bytes out)))

(define (mangle⁻¹ x) 
  (define out
    (open-output-bytes))
  
  (inflate (open-input-bytes
             (hex-string->bytes x)) out)
  
  (bytes->string/utf-8 (get-output-bytes out)))

(module+ test
  (require rackunit)
  (define id (∘ mangle⁻¹ mangle))
  (check-equal? (id "12345") "12345"))
