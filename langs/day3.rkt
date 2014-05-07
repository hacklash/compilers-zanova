#lang racket/base
(require racket/contract
         racket/match
         (only-in plai define-type type-case)
         (prefix-in x86: "asm.rkt"))

#| Add comparators (= < <= > >=) to the binops
   E = <num> | (<binop> E E) | (<unaop> E)
   <binop> =  + | - | bitwise-and | bitwise-ior | bitwise-xor
            | * | quotient | remainder
            | = | < | <= | > | >=
   <unaop> = add1 | sub1 | bitwise-not
|#

(define-type E 
  [num (n number?)]
  [binop (op binop-src?) (lhs E?) (rhs E?)]
  [unaop (opor unaop-src?) (opand E?)])

(define (cmpop->asm op)
  (match-lambda* [(list x86:eax (? x86:register? r))
                  (x86:seqn
                   (x86:cmp x86:eax r)
                   #;(x86:mov x86:eax 0)
                   (op x86:al))]))

(define binops
  (hash '+ x86:add
        '- x86:sub
        'bitwise-and x86:and
        'bitwise-ior x86:or
        'bitwise-xor x86:xor
        '* (match-lambda* [(list x86:eax (? x86:register? r)) 
                           (x86:imul r)])
        'quotient (match-lambda* [(list x86:eax (? x86:register? r)) 
                                  (x86:idiv r)])
        'remainder (match-lambda* [(list x86:eax (? x86:register? r))
                                   (x86:seqn
                                    (x86:idiv r)
                                    (x86:mov x86:eax x86:edx))])
        '= (cmpop->asm x86:sete)
        '< (cmpop->asm x86:setl)
        '<= (cmpop->asm x86:setle)
        '> (cmpop->asm x86:setg)
        '>= (cmpop->asm x86:setge)))
(define (binop-src? op) (hash-has-key? binops op))
(define (binop-src->asm op) (hash-ref binops op))

(define unaops
  (hash 'add1 x86:inc
        'sub1 x86:dec
        'bitwise-not x86:not))
(define (unaop-src? op) (hash-has-key? unaops op))
(define (unaop-src->asm op) (hash-ref unaops op))

(define parse
  (match-lambda
    [(list (? binop-src? operator) lhs rhs)
     (binop operator (parse lhs) (parse rhs))]
    [(list (? unaop-src? operator) operand)
     (unaop operator (parse operand))]
    [(? byte? b)
     (num b)]))

(define (to-asm pp)
  (type-case E pp
    [binop (op lhs rhs)
     (x86:seqn
      (x86:push x86:ebx)
      (to-asm rhs)
      (x86:mov x86:ebx x86:eax)
      (to-asm lhs)
      ((binop-src->asm op) x86:eax x86:ebx)
      (x86:pop x86:ebx))]
    [unaop (operator operand)
     (x86:seqn
      (to-asm operand)
      ((unaop-src->asm operator) x86:eax))]
    [num (b)
     (x86:seqn
      (x86:mov x86:eax b))]))

(define (cmpop->rkt op)
  (match-lambda* [(list l r) (if (op l r) 1 0)]))

(define op-src->rkt
  (match-lambda
    ['+ +]
    ['- -]
    ['bitwise-and bitwise-and]
    ['bitwise-ior bitwise-ior]
    ['bitwise-xor bitwise-xor]
    ['add1 add1]
    ['sub1 sub1]
    ['bitwise-not bitwise-not]
    ['* *]
    ['quotient quotient]
    ['remainder remainder]
    ['= (cmpop->rkt =)]
    ['< (cmpop->rkt <)]
    ['<= (cmpop->rkt <=)]
    ['> (cmpop->rkt >)]
    ['>= (cmpop->rkt >=)]))

(define (interp pp)
  (type-case E pp
    [binop (opor lhs rhs)
     ((op-src->rkt opor)
      (interp lhs)
      (interp rhs))]
    [unaop (opor opand)
     ((op-src->rkt opor)
      (interp opand))]
    [num (b)
     b]))

(provide
 (contract-out
  [parse (-> any/c E?)]
  [to-asm (-> E? x86:asm?)]
  [interp (-> E? any/c)]))