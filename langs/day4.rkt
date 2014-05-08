#lang racket/base
(require racket/contract
         racket/match
         plai/datatype
         (prefix-in x86: "asm.rkt"))

#| Add if0
   Fe = E | (if0 E Fe Fe)
   E  = <num> | (<binop> E E) | (<unaop> E)
   <binop> =  + | - | bitwise-and | bitwise-ior | bitwise-xor
            | * | quotient | remainder
            | = | < | <= | > | >=
   <unaop> = add1 | sub1 | bitwise-not
|#

(define-type E 
  [num (n number?)]
  [binop (op binop-src?) (lhs E?) (rhs E?)]
  [unaop (opor unaop-src?) (opand E?)])

(define-type Fe
  [fe-E (e E?)]
  [if0 (test E?) (trueb Fe?) (falsb Fe?)])

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

(define parse-fe
  (match-lambda
    [(list 'if0 test trueb falsb)
     (if0 (parse-e test) (parse-fe trueb) (parse-fe falsb))]
    [expr (fe-E (parse-e expr))]))

(define parse-e
  (match-lambda
    [(list (? binop-src? operator) lhs rhs)
     (binop operator (parse-e lhs) (parse-e rhs))]
    [(list (? unaop-src? operator) operand)
     (unaop operator (parse-e operand))]
    [(? byte? b)
     (num b)]))

(define parse parse-fe)

(define (fe-to-asm pp)
  (type-case Fe pp
    [if0 
     (test trueb falsb)
         (let ([falsb-start (x86:make-label 'falsb-start)]
               [if0-end (x86:make-label 'if0-end)])
           (x86:seqn
            (e-to-asm test)
            (x86:cmp x86:eax 0)
            (x86:jne falsb-start)
            (fe-to-asm trueb)
            (x86:jmp if0-end)
            (x86:label-mark falsb-start)
            (fe-to-asm falsb)
            (x86:label-mark if0-end)))]
    [fe-E (e) (e-to-asm e)]))

(define (e-to-asm pp)
  (type-case E pp
    [binop 
     (op lhs rhs)
     (x86:seqn
      (x86:push x86:ebx)
      (e-to-asm rhs)
      (x86:mov x86:ebx x86:eax)
      (e-to-asm lhs)
      ((binop-src->asm op) x86:eax x86:ebx)
      (x86:pop x86:ebx))]
    [unaop 
     (operator operand)
     (x86:seqn
      (e-to-asm operand)
      ((unaop-src->asm operator) x86:eax))]
    [num 
     (b)
     (x86:seqn
      (x86:mov x86:eax b))]))

(define to-asm fe-to-asm)

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

(define (fe-interp pp)
  (type-case Fe pp
    [if0 
     (test trueb falsb)
         (if (= 0 (e-interp test))
             (fe-interp trueb)
             (fe-interp falsb))]
    [fe-E (e) (e-interp e)]))

(define (e-interp pp)
  (type-case E pp
    [binop 
     (opor lhs rhs)
     ((op-src->rkt opor)
      (e-interp lhs)
      (e-interp rhs))]
    [unaop 
     (opor opand)
     ((op-src->rkt opor)
      (e-interp opand))]
    [num (b) b]))

(define interp fe-interp)

(provide
 (contract-out
  [parse (-> any/c Fe?)]
  [to-asm (-> Fe? x86:asm?)]
  [interp (-> Fe? any/c)]))