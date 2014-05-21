#lang racket/base
(require racket/contract
         racket/match
         plai/datatype
         "../substitution-cipher.rkt"
         (prefix-in x86: "asm.rkt"))

#| 
   E = <num> | (<binop> E E) | (<unaop> E)
   <binop> = + | - | bitwise-and | bitwise-ior | bitwise-xor
   <unaop> = add1 | sub1 | bitwise-not
|#

(define-type E 
  [Num (n number?)]
  [Binop (op binop-src?) (lhs E?) (rhs E?)]
  [Unaop (opor unaop-src?) (opand E?)])

(define-subst-cipher
  binop
  [src          rkt         asm]
  ['+           +           x86:add]
  ['-           -           x86:sub]
  ['bitwise-and bitwise-and x86:and]
  ['bitwise-ior bitwise-ior x86:or]
  ['bitwise-xor bitwise-xor x86:xor])

(define-subst-cipher
  unaop
  [src          rkt         asm]
  ['add1        add1        x86:inc]
  ['sub1        sub1        x86:dec]
  ['bitwise-not bitwise-not x86:not])

(define parse
  (match-lambda
    [(list (? binop-src? operator) lhs rhs)
     (Binop operator (parse lhs) (parse rhs))]
    [(list (? unaop-src? operator) operand)
     (Unaop operator (parse operand))]
    [(? byte? b)
     (Num b)]))

(define (to-asm pp)
  (type-case E pp
    [Binop 
     (op lhs rhs)
     (x86:seqn
      (x86:push x86:ebx)
      (to-asm rhs)
      (x86:mov x86:ebx x86:eax)
      (to-asm lhs)
      ((binop-src->asm op) x86:eax x86:ebx)
      (x86:pop x86:ebx))]
    [Unaop 
     (operator operand)
     (x86:seqn
      (to-asm operand)
      ((unaop-src->asm operator) x86:eax))]
    [Num 
     (b)
     (x86:seqn
      (x86:mov x86:eax b))]))

(define (interp pp)
  (type-case E pp
    [Binop 
     (opor lhs rhs)
     ((binop-src->rkt opor)
      (interp lhs)
      (interp rhs))]
    [Unaop 
     (opor opand)
     ((unaop-src->rkt opor)
      (interp opand))]
    [Num (b) b]))

(provide
 (contract-out
  [parse (-> any/c E?)]
  [to-asm (-> E? x86:asm?)]
  [interp (-> E? any/c)]))