#lang at-exp racket/base

(require (only-in scribble/text output))
(require (for-syntax racket/base))
(require (for-syntax syntax/parse))
(require racket/string
         racket/list
         racket/path
         racket/dict
         racket/generic
         racket/function
         racket/local
         (only-in scribble/text begin/text))

(require "symbol-table.rkt"
         "data.rkt"
         "type-sig.rkt"
         (for-syntax "symbol-table.rkt"))

(require "../../misc/NHA.rkt"
         "../../misc/tree.rkt")

(provide ƒ: info:ƒ-body? constructor: destructor:)

(module+ test
  (require rackunit))
;; = Datatypes =

(define (access-mode->c:const x)
  (case x
    ['r "const"]
    ['rw #f]
    ['w  #f]
    ['#f #f]))

(define (linkage->c:linkage x)
  (case x
    ['C-link "extern \"C\""]
    [else x]))

(define (trace x)
  (displayln x)
  x)

(define (mangle-function nfo)
  (format "~a_~a"
          (info:ƒ-function-path nfo)
          (mangle-sig (info:ƒ-sig nfo))))
                   

(define (pad-r x)
  (m-xf "~a "  x #:reference #f))

(define (pad-l x)
  (m-xf " ~a"  x #:reference #f))


(struct info:ƒ (function-path sig)
  
 #:methods gen:c-declaration
    [(define (gen-key nfo)
       (file-name-from-path
        (string->path (mangle-function nfo))))
     
     (define (gen-c-declaration nfo xs)
       (define sig
         (info:ƒ-sig nfo))
       
       (format "~a~a~a~a(~a)~a~a;\n"
               (pad-r (linkage->c:linkage (typesig-linkage sig)))
               (pad-r (typesig-storage sig))
               (pad-r (typesig-codomain sig))
               (file-name-from-path (info:ƒ-function-path nfo))
               (string-join (typesig-domain sig) ", ")
               (pad-l (typesig-override sig))
               (pad-l (access-mode->c:const
                       (typesig-access sig)))))])



;; = C Code Gen =
(struct info:ƒ-body (mangled-function-name
                     declaration-path
                     initializers
                     parameters
                     definition)

  #:methods gen:c-declaration
    [(define (gen-key nfo)
       (string->path
        (info:ƒ-body-mangled-function-name nfo)))
     
     (define (gen-c-declaration nfo xs)
       (define decl
         (symbol-table-ref (info:ƒ-body-declaration-path nfo)))
       (define sig
         (info:ƒ-sig decl))
       
       (define function-head
         (format "~a~a(~a)~a"
                 (pad-r (typesig-codomain sig))
                 (ss->scope (info:ƒ-function-path decl))
                 (string-join
                  (map (λ (x y) (string-append x " " (symbol->string y)))
                       (typesig-domain sig)
                       (info:ƒ-body-parameters nfo))
                  ", ")
                 (m-xf ":\n    ~a" (info:ƒ-body-initializers nfo))))

       @list{
             
       @|function-head|
       {
           @|(add-between (info:ƒ-body-definition nfo) #\newline)|
       }

       })])
  




#| FUNCTION |#

(define-syntax (function stx)
  (syntax-parse stx
    [(_ symbol-path vars
       doc
       sig
       init body ...
       no-return)
     
     #`(begin
         (let*-values
             [[(path) (ss->full-path symbol-path)]
              [(decl) (info:ƒ path sig)]]

           (symbol-table-set!* symbol-path (const decl))
           
           (define path-ttt
             (build-path
              (value:0 (split-path path))
              (gen-key decl)))
           
           (define symbol-name
             (format "body~a" (string-replace
                                (mangle-function decl)
                                "/" "::")))

           
           ;; create empty node to break data dependency cycle
           (symbol-table-set! "/" (info:block symbol-name))
           
           (define info-body
             (parameterize [[current-symbol-path
                             (build-path "/" symbol-name)]]
               
               (info:ƒ-body symbol-name
                            path-ttt
                            init
                            vars
                            (begin/text body ...))))
         
           (symbol-table-set! "/" info-body)))]))


(define-syntax (ƒ: stx)
  (syntax-parse stx
    [(_ symbol-path:id rest ...)
     #'(ƒ: 'symbol-path rest ...)]
    
    [(_ symbol-path (var ...)
        (~optional doc:str #:defaults [(doc #'"")])
        type:type-annotation
        body ...)
     
     #`(function symbol-path
                 (list 'var ...)
                 doc
                 type.sig
                 ""
                 body ...
                 #f)]))





(module+ test
  (define xxxx
    "osg::Geode &")
  (with-empty-symbol-table
   
       (class: /A)
       
       (ƒ: (format "/uniform_~a" 'suffix) (x y)
           :: "int"
           -> (symbol->string 'yyy)
           -> "void"
           @list{ return; })
             
       (ƒ: /A/.public/bind_attributes (ctx geode g)
           "Bind Vertex attributes"
           :: (virtual)
           => "void * const"
           -> xxxx
           -> "osg::Geometry *"
           -> "void *"
         #|
         (𝑣: self :: "BlenderObject *"
             cast: 'ctx )
         
         (𝑣: target :: "bind_target"
             (: bind_target self/material/getProgram self g))
         
         (𝑣: env :: "ss_env" 
             (: ss_env_enter
                _scheme
                ((self/_material/getName)/c_str)))
         
         (: ss_call1p   _scheme "bind-attributes" &target)
         (: ss_env_exit _scheme  env)
       
         #;(return: ctx)
         |#
       @list{
          BlenderObject *self = (BlenderObject*) ctx;
          bind_target target  = {self->_material->getProgram(), self, g};

          ss_env env = ss_env_enter(_scheme, self->_material->getName().c_str());
          ss_call1p(_scheme, "bind-attributes", &target);
          ss_env_exit(_scheme, env);
   
          return ctx; })


       (ƒ: /A/.protected/bind_attributes (ctx geode g)
           "Bind Vertex attributes"
           :: (C-link)
           => "void * const"
           -> "osg::Geode &"
           -> "osg::Geometry *"
           -> "const void *"
       
       @list{ return; })
    #;(symbol-table-print)
    (output (symbol-table-export))

    (displayln "===========")
    
    (output (symbol-table-export
             #:filter
             (∘ not info:ƒ-body?)))))



                 
(define-syntax (destructor: stx)
  (syntax-parse stx
    [(_ path
        (~optional doc:str #:defaults [(doc #'"")])
        type:type-annotation
        body ...)
     #`(function
        (build-path (ss->path 'path)
                    (ss-append "~" (ss-last-node 'path)))
        empty doc
        type.sig
        ""
        body ...
        #t)]))



(define-syntax (constructor: stx)
  (syntax-parse stx
    [(_ path (var ...)
        (~optional doc:str #:defaults [(doc #'"")])
        type:type-annotation
        (~optional init:str #:defaults [(init #'"")])
        body ...)
     
     #`(function
        (build-path (ss->path 'path)
                    (ss-last-node 'path))
        (list 'var ...)
        doc
        type.sig
        init
        body ...
        #t)]))


(module+ test
  
  (with-empty-symbol-table
   
      (class: /XObject)
      (class: /ZObject)
      
      (constructor: /XObject/.public (index letter)
        ∷ (virtual) => "int" -> "char" -> "void"
        "member(0)"
       @list{ return; })
      
      (destructor: /XObject/.protected/ ∷ (virtual) => "void"
       @list{ // TODO: free stuff
              return; })
    
      (destructor: /ZObject/.protected ∷ "void"
       @list{ // TODO: free stuff
              return; })

      (output (symbol-table-export))))

