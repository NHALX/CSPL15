#lang at-exp racket/base ; reader "../../CSPL15/CSPL15.rkt"

(require syntax/parse (for-syntax racket/base syntax/parse))
(require racket/format)
(require scribble/text)

(require "../../CSPL15/private/type-sig.rkt")
(require "../../misc/NHA.rkt")

(provide uid-counter
         for-each: find: map: foldl:
         λ:)




(define (new-uid-counter)
  
  (define uid-total 0)
  
  (define (count prefix)
    (set! uid-total (+ uid-total 1))
  
    (format "~a~a" prefix uid-total))
  
  count)


(define uid-counter (new-uid-counter))





(define-syntax-rule (for-each: f xs)
  (let [[temp (format "~a_it"
               (uid-counter "__c_for_each"))]]
  @begin/text{
   for (auto&& @|temp|: @|`xs|)
       @|`f|(@|temp|); }))

(define-syntax-rule (find: pred xs)
  @begin/text{
   std::find_if(std::begin(@|`xs|),
                std::end(@|`xs|), @|`pred|)})



(define-syntax-rule (map: f xs)
  
  (let* [[input-type  (format "(* (~a).begin())" `xs)]
         [output-type (format "~a(~a)" `f input-type)]]
    
   ;; wraped in closure to become an r-value
    
   @begin/text{
    [&@|`f|, &@|`xs|] // (map: @|`f| @|`xs|)
    {
      rebind<decltype(@|`xs|),
             decltype(@|output-type|)>::type result;
    
      std::transform(@|`xs|.begin(),
                     @|`xs|.end(),
                     std::back_inserter(result),
                     @|`f|);
      return result;
    }() }))


(define-syntax (λ: stx)
  (syntax-parse stx
    [(_ name (args ...) body ...)
     #`@begin/text{
        auto @|`name| = [](@(string-join
                              (list (format "auto ~a" `args) ...)
                              ", "))
             { @(strip-newline (list body ...))}; }]
    
    [(_ (args ...) body ...)
     #`@begin/text{
        [](@|`args ...|){
             @|body ...| }}]))

    
(define-syntax (foldl: stx)
  (syntax-parse stx
    [(_ f init xs)
     #`@begin/text{
        std::accumulate(@|`xs|.begin(),
                        @|`xs|.end(),
                        @|init|, @|`f|)}]))
