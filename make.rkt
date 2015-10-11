#lang racket/base
(require racket/port
         racket/path
         racket/undefined
         racket/match
         racket/file)

(require scribble/text)
(require "shell-pipe.rkt")
(require "dep-graph.rkt")
(require "../misc/NHA.rkt")
(require "../misc/monad.rkt")
(require graph)

(provide gen-obj-file outdated? subsys module-deps S make 
         file-extension?
         file-in-subdir?
         exec exec-simulate
         cat
         (all-from-out graph)
         (all-from-out racket/base)
         (all-from-out racket/port)
         (all-from-out racket/path)
         (all-from-out scribble/text)
         (all-from-out "../misc/NHA.rkt"))


;; Ｏｂｊｅｃｔ Ｆｉｌｅｓ

(define (gen-obj-file dir x)
  
  (define (sign-extension v)
    (if (< v 0)
        (format "1~X" (abs v))
        (format "0~X" (abs v))))
  
  ;; TODO: deal with hash collisions
  (full-filename
   (simplify-path
    (string->path
     (format "~a/~a-~a.~a.o"
             dir
             (sign-extension (equal-hash-code x))
             (sign-extension (equal-secondary-hash-code x)) 
             (file-name-from-path x))))))


;; Ｒｅｂｕｉｌｄ Ｔｅｓｔｓ

(define (newer? a b)
  (or
   (not (file-exists? b))
   (> (file-or-directory-modify-seconds a)
      (file-or-directory-modify-seconds b))))


(define/match (outdated? deps _)
  [(#f   (cons in out)) (newer? in out)]
  [(deps (cons in out))
   (define full-in (full-filename in))
   
   (printf "OUTDATED: ~a\n" in)
   (for-each
    (λ (i)
      (printf "(newer? ~a) = ~a\n" i (newer? i out)))
    (cons full-in
          (subset deps (full-filename in))))
   
   (ormap (λ (i) (newer? i out))
          (cons full-in
                (subset deps full-in)))])


;; Ｍｉｓｃ

(define S string-append)


(define (exec-simulate 
         #:in-fp [in #f]
         #:out-fp [out #f]
         #:exists [exists-flags 'error]
         . xs)
  (begin
    (displayln (string-join xs " | "))
    #t))

(define subsys
  (make-parameter exec-simulate))



;; Ｐａｔｈ 

(define full-filename
  path->complete-path)

(define (file-extension? suffix)
  (define criteria
    (regexp (S ".*" (regexp-quote suffix))))

  (λ (x) (regexp-match? criteria 
                        (if (path? x)
                            (path->string x)
                            x))))

(define (file-in-subdir? cwd)
  (let* {[prefix    (regexp-quote cwd)]
         [criteria  (regexp (string-append prefix ".*"))]}
      
    (λ (x) (regexp-match? criteria 
                          (if (path? x)
                              (path->string x)
                              x)))))


;; Ｄｅｐｅｎｄｅｎｃｙ Ｇｒａｐｈ

(define (module-deps criteria m)
  (graph-map full-filename
       (resolve-deps criteria m)))



;; Ｂｕｉｌｄ

(define (make transform files #:dep-tree [deps #f])

  (define cwd
    (current-directory))
  
  (define (apply-cons f x)
    (f (car x) (cdr x)))
  
  (define-values (need skip)
    (partition (⤶ outdated? deps) files))
  
  
  (for-each (λ (x)
              (printf "[skipping]: ~a ⟹ ~a\n"
                      (find-relative-path cwd (car x))
                      (find-relative-path cwd (cdr x))))
            skip)
  
  (for-each (⤶ apply-cons transform) need))

 


(define (cat a b c)
  (monad/cps:
   header ← (call-with-input-file a)
   input  ← (call-with-input-file b)
   output ← (call-with-output-file #:exists 'replace c)
   (return
    (copy-port (input-port-append #f header input)
               output))))
   


(define (call-with-tempfile prefix use)
  
  (let [[tmp-file #f]]
    (dynamic-wind
      (λ () (set! tmp-file (make-temporary-file prefix)))
      (λ () (use tmp-file))
      (λ () (when tmp-file (delete-file tmp-file))))))



(define (edit x)
  (exec (format "/usr/bin/emacsclient ~a" x)))


(define (ccr⟹obj in out)
  
  (define (gen-interm in suffix)
    (build-path "generated"
                (file-name-from-path
                 (path-replace-suffix in suffix))))

  (define src-cc
    (gen-interm in ".cc"))

  ((subsys)   
   #:out-fp (gen-interm in ".h")
   #:exists 'replace 
   (format "~a ~a --exports" racket in))
    
  ((subsys)   
   #:out-fp src-cc
   #:exists 'replace 
   (format "~a ~a" racket in))

  ((subsys)
   #:in-fp src-cc
   (format "~a ~a -c ~a -o ~a" c++ src-cc c-flags out))

  (void))


(define (build include-dir cspl15-file a.out-file cc-file cc-port)

  (define (error-break! restart recompile exn)
  
    (displayln "==============================================")
    (displayln (exn-message exn))
    
    (define (input-break)
      ;; TODO: auto generate based on composition
      (displayln (string-join 
                  (list "error-break! options:"
                        "q: quit"
                        "d: display C source "
                        "e: edit C source and retry compilation"
                        "b: edit CSPL15 source and restart"
                        "r: retry"
                        "i: ignore")
                  "\n> "))
      
      (define cmd (read-line))
      
      (case cmd
        [("q" eof) (error 'error-break! "user quit")]
        [("d")     (exec cspl15-file cc-port)]
        [("e")     (begin (edit cc-file)     (recompile))]
        [("b")     (begin (edit cspl15-file) (restart))]
        [("r")     (restart)]
        [("i")     (void)]
        [else
         (printf "unknown option: ~a\n" cmd)
         (input-break)]))
  
    (input-break))

  
  (define:re-entrant (build)
    
    (file-position cc-port 0)
    ;(export-all #f cc-port)
    (exec cspl15-file cc-port)
    (flush-output cc-port)

    retry:build)

  (define:re-entrant (compile retry:build)
      
    (with-handlers ([exn:fail? (⤶ error-break! retry:build retry:compile)])
        
      (begin
        (define build-cmd
          (format "/usr/bin/g++ -DWITH_LINEAR_ALGEBRA -DWITH_UNIFORM -ggdb -g3 -g -O0 -I~a -I~a -o ~a -std=c++1y ~a"
                  include-dir
                  (directory-from-path cspl15-file)
                  a.out-file
                  cc-file))
          
        ;;(displayln build-cmd)
        (exec build-cmd)
        (list retry:build retry:compile))))

  (define:re-entry (run retry:build retry:compile)
    (with-handlers ([exn:fail? (⤶ error-break! retry:build retry:compile)])
        
      (begin
        (displayln "\nBuild complete. Running tests...")
        (exec (path->string a.out-file)) )))
  
  (∘ run build compile))


(define (run-unit-test include-dir source-file sub-module)
  (printf "unit-test: ~a : ~a\n" source-file sub-module)
  (dynamic-require sub-module #f)
  
  (monad/cps:
   cc     ← (call-with-tempfile "~a_unittest.cc")
   a.out  ← (call-with-tempfile "~a_unittest.a.out")
   port   ← (call-with-output-file #:exists 'replace cc)
   
   (return (build include-dir source-file a.out cc port))))
