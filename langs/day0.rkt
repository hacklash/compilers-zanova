#lang racket/base
(require racket/contract
         racket/match
         (only-in plai define-type type-case)
         (prefix-in x86: "asm.rkt"))

#| A valid program is a number [0,256), whose exit code, when run is that number
   e = <num>
|#

(define-type E [num (n number?)]) ;XX instead of number? 32bitnum?

(define parse
  (match-lambda
   [(? byte? b) ;XX instead of byte? 32bitnum?
    (num b)]))

(define (to-asm pp)
  (type-case E pp
   [num (b)
    (x86:seqn
     (x86:mov x86:eax b))]))

(define (interp pp)
  (type-case E pp
   [num (b)
    b]))

(provide
 (contract-out
  [parse (-> any/c E?)]
  [to-asm (-> E? x86:asm?)]
  [interp (-> E? any/c)]))