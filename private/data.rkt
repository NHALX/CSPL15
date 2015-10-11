#lang at-exp racket/base
(require racket/string
         racket/dict
         racket/path
         racket/list
         racket/match
         racket/function
         (only-in scribble/text output)
         (for-syntax syntax/parse
                     racket/base
                     racket/list))

(require "symbol-table.rkt" (for-syntax "symbol-table.rkt"))
(require "../../misc/tree.rkt")
(require "../../misc/NHA.rkt")
(require "type-sig.rkt")
(provide 𝑣:
         (struct-out info:var)
         transform-codomain (for-syntax assignment)
         typedef: class: struct: union: enum: namespace:)



;; ＶＡＲ

(define/match (convert-spec _)
  [('C-link) "extern \"C\""]
  [(x)       (symbol->string x)])


(define current-error-handler
  (make-parameter
   (λ (msg)
     (format "throw std::runtime_error(\"error: ~a at \" __FILE_LINE__);"
             msg))))

(struct info:var (name value cast? post-condition sig)
  #:methods gen:c-declaration
  
  [(define (gen-key nfo)
     (info:var-name nfo))
   
   (define (gen-c-declaration nfo _)
     
     (let* [[extra
             (m-xf "~a "
                   (string-join
                    (map convert-spec
                         (filter
                          identity
                          (list
                           (typesig-linkage (info:var-sig nfo))
                           (typesig-storage (info:var-sig nfo)))))))]
            
            [type  (typesig-codomain (info:var-sig nfo))]
            [name  (info:var-name nfo)]
            [cast  (if (not (info:var-cast? nfo))
                       ""
                       (format "(~a) " (typesig-codomain (info:var-sig nfo))))]
            
            [init  (strip-newline (info:var-value nfo))]
            [pcn (info:var-post-condition nfo)]
            [expr (m-xf "(~a)"
                        (and pcn
                             (if (equal? '? (car pcn))
                                 (cadr pcn)
                                 (join (cons name pcn)))))]
            
            [post  (if (not (info:var-post-condition nfo))
                       ""                      
                       @list{
                             if (!@expr)
                               { @((current-error-handler) expr) }
                             @#\newline
                       })]]
       
       (if (info:var-value nfo)
           @list{@|extra|@|type| @|name| = @|cast|@|init|;@#\newline@post}
           @list{@|extra|@|type| @|name|;@#\newline})))])


(require (for-syntax syntax/parse))


(begin-for-syntax

  (define-syntax-class comparison-class
    (pattern
     (~or (~datum ?)
          (~datum !=)
          (~datum ==)
          (~datum !=)
          (~datum <)
          (~datum <=)
          (~datum >)
          (~datum >=))))
  
  (define-splicing-syntax-class post-condition
    (pattern (~seq cmp:comparison-class expr:str)))
  
  (define-splicing-syntax-class assignment
    (pattern
     (~optional
         (~seq (~datum =)
               (~optional (~and (~datum cast:)
                                (~bind [cast #'#t]))
                          #:defaults [[cast #'#f]])
               val
               (~optional test:post-condition
                          #:defaults [[test #'#f]
                                      [test.cmp #'#f]
                                      [test.expr #'#f]]))
         
         #:defaults [[cast #'#f]
                     [val  #'#f]
                     [test     #'#f]
                     [test.cmp #'#f]
                     [test.expr #'#f]]))))

(define-syntax (𝑣: stx)
  (syntax-parse stx
        
    [(_ (~seq
         path
         init:assignment
         (~optional type:type-annotation
                    #:defaults [[type.sig #'(typesig #f #f #f #f #f
                                                     empty
                                                     "auto")]])) ...+)
     
     #`(list
        ((⤷ gen-c-declaration empty)
         (symbol-table-set!*
          'path
          (⤷ info:var init.val init.cast 'init.test type.sig)))
        
        ...)]))






(module+ test
  (require rackunit)
  

  (define (var-test-0)
    
    (𝑣: /whatever = 7        :: (C-link static) => "int")
    (𝑣: /whatever2 = cast: 7 ∷ (extern) => "int"
        /whatever3 = 9       ∷ "int"
        /whatever4           ∷ "char")
    
    (𝑣: txt = "hi"     ? "strcmp(txt) == 0"
        ptr = "&txt"  != "NULL"
            :: "const char **"))

  
  (check-equal? (symbol-table-render var-test-0)
   
   ((⤷ string-join "\n")
    (list
     "extern \"C\" static int whatever = 7;"
     "extern int whatever2 = (int) 7;"
     "int whatever3 = 9;"
     "char whatever4;"
     "auto txt = hi;"
     "if (!(strcmp(txt) == 0))"
     (format "  { ~a }"
             ((current-error-handler) "(strcmp(txt) == 0)"))
     ""
     "const char ** ptr = &txt;"
     "if (!(ptr != NULL))"
     (format "  { ~a }"
             ((current-error-handler) "(ptr != NULL)"))
     ""
     ""))))


(define (transform-codomain f sig)
  (struct-copy typesig sig
               [codomain (f (typesig-codomain sig))]))

(module+ test
  (with-empty-symbol-table
    (𝑣: /whatever5 ∷ "char")
    (check-equal?
     (transform-codomain
      (⤶ format "const ~a * const")
      (info:var-sig (symbol-table-ref "/whatever5")))
     (typesig #f #f #f #f #f '() "const char * const"))))



;; ＴＹＰＥＤＥＦ

(struct info:typedef (type alias)
  #:methods gen:c-declaration
  
  [(define (gen-key nfo) (info:typedef-alias nfo))
   (define (gen-c-declaration nfo _)
     (format "typedef ~a ~a;\n"
             (info:typedef-type nfo)
             (info:typedef-alias nfo)))])

(define-syntax-rule (typedef: type path)
  (symbol-table-set!* 'path
               (λ (x) (info:typedef 'type x))))

(module+ test

  (with-empty-symbol-table
    (check-equal? (symbol-table-render
                   (λ () (typedef: int /whatever)))
                  
                  "typedef int whatever;\n")))



#|
enum    --> done
goto    --> TODO
if      --> TODO
else    --> TODO
return  --> TODO
sizeof  --> TODO
case    --> match
default --> match
switch  --> match
typedef --> typedef
union   --> record
struct  --> record
break   --> TODO
continue--> TODO
do      --> map/fold/for-each
for     --> map/fold/for-each
while   --> map/fold/for-each
|#


;; ＳＴＲＵＣＴ 

(struct info:struct (name prefix super)
  #:methods gen:c-declaration
  
  [(define (gen-key nfo) (info:struct-name nfo))
   
   (define (gen-c-declaration nfo xs)
     (define (get access i)
       (define z (list-ref xs i))
       (if (empty? z)
           empty
           (list "\n" access ":\n  " z)))
     
     (let*-values [[(prefix)    (info:struct-prefix nfo)]
                   [(name)      (info:struct-name   nfo)]
                   [(inherit)   (m-xf ": ~a " (info:struct-super nfo))]
                   [(_ rest)    (split-at xs 3)]]
       @list{
             
       @|prefix| @|name| @|inherit|
       {
           @(list rest
                  (get "public"  0)
                  (get "private" 1)
                  (get "protected" 2))
       };
        
       }))])

(define-syntax (record-t-fields stx)
  (syntax-parse stx
    [(_ path) 
     #'(void)]
    [(_ path xs ...)
     #`(parameterize [[current-symbol-path path]]
         xs ...)]))

(define-for-syntax (record-t rectype stx)
  (syntax-parse stx
    [(_ path
        (~optional (~or
                    (~seq (~or (~datum ∈)
                               (~datum ∊))
                          inherit:str) 
                    (~seq #:inherit inherit:str))
                   #:defaults ([inherit #'""]))
       (~or
        ((~datum public:)    xs ...)
        ((~datum private:)   ys ...)
        ((~datum protected:) zs ...)
        qs) ...)
     
     #`(begin
         (let* [[root      (ss->full-path 'path)]
                [public    (build-path root ".public")]
                [private   (build-path root ".private")]
                [protected (build-path root ".protected")]]
           
           (symbol-table-set!*
            root
            (λ (x) (info:struct x '#,rectype 'inherit)))
           
           ;; TODO: gen-c-declaration for info:struct depends on this
           ;;       insertion order.
           (symbol-table-set!* public    (⤶ info:block))
           (symbol-table-set!* private   (⤶ info:block))
           (symbol-table-set!* protected (⤶ info:block))

           (record-t-fields public    xs ... ...)
           (record-t-fields private   ys ... ...)
           (record-t-fields protected zs ... ...)
           (record-t-fields root      qs ...)))]))



;; TODO: tagged unions

;; TODO: union and namespace accept meaningless args due to sharing
;;       implementation with class/struct.
(define-syntax (class: stx)     (record-t 'class stx))
(define-syntax (struct: stx)    (record-t 'struct stx))
(define-syntax (union: stx)     (record-t 'union stx))
(define-syntax (namespace: stx) (record-t 'namespace stx))

(module+ test

  (define (test-class-s)
    (class: /some_class)
    (union: /some_class/.public/u32)
    (𝑣:     /some_class/.public/u32/some_field3 :: "char")
    (𝑣:     /some_class/.public/some_field2     :: "int"))


  (check-equal? (symbol-table-render test-class-s)
"
class some_class 
{

    public:

      union u32 
      {
          char some_field3;

      };
      int some_field2;

};
")

  
  (define (test-ns)
    (namespace: /ns)
    ; TODO: static is ignored...
    (𝑣:         /ns/hello ∷ (static) ⇒ "int"))

  (check-equal? (symbol-table-render test-ns)
"
namespace ns 
{
    static int hello;

};
")

  (define (test-class-w)    
    (struct: /whatever ∊ "osg::Node"
      (public:
       (𝑣: program ∷ "osg::Program *")
       (𝑣: node    ∷ "osg::Node *"))
      
      (private:
       (class: some_class_xxxxxxxxxxxx
         (𝑣: missing_x7a2      :: "void*")
         (𝑣: missing_x343qq43  ∷ "osg::Program *"))
       
       (𝑣: x34343  ∷ "osg::Program *")
       (𝑣: x24242  ∷ "osg::Node *"))
      
      (protected:
       (𝑣: zsds    ∷ "osg::Program *")
       (𝑣: ff311   ∷ "osg::Node *")))
    
    ;; extend struct from within root scope
    (𝑣: /whatever/x ∷ "char*"))

  
  (check-equal?
   (symbol-table-render test-class-w)
"
struct whatever : osg::Node 
{
    char* x;

    public:
      osg::Program * program;
      osg::Node * node;

    private:

      class some_class_xxxxxxxxxxxx 
      {
          void* missing_x7a2;
          osg::Program * missing_x343qq43;

      };
      osg::Program * x34343;
      osg::Node * x24242;

    protected:
      osg::Program * zsds;
      osg::Node * ff311;

};
")

)


;; ＥＮＵＭ

(struct info:enum (name type mappings)
  #:methods gen:c-declaration
  
  [(define (gen-key nfo)
     (info:enum-name nfo))
   
   (define (gen-c-declaration nfo _)
     
     (let* [[name  (info:enum-name nfo)]
            [type  (m-xf ": ~a " (info:enum-type nfo) #:reference #f)]
            [f     (match-lambda**
                    [((list k v)) (format "~a = ~a,\n" k v)]
                    [((list k))   (format "~a,\n" k)]
                    [(k)          (format "~a,\n" k)])]]
       @list{
             
       enum @|name| @|type|{
           @|(map f (info:enum-mappings nfo))|
       };
        
       }))])

(define-syntax (enum: stx)
  (syntax-parse stx
    [(_ path
        (~optional type:type-annotation)
       xs ...)
     #`(symbol-table-set!*
        'path
        (λ (name) (info:enum name
                             #,(attribute type.domain)
                             (list 'xs ...))))]))

(module+ test

  (define (test-enum)
    (enum: flags ∷ "int"
           (FLAG_A 1)
           (FLAG_B 2)
           (FLAG_C  )
           (FLAG_F 5))
    
    (enum: flags22
           FLAG_D FLAG_E))

  (check-equal? (symbol-table-render test-enum)
"
enum flags : int {
    FLAG_A = 1,
    FLAG_B = 2,
    FLAG_C,
    FLAG_F = 5,

};

enum flags22 {
    FLAG_D,
    FLAG_E,

};
"))


