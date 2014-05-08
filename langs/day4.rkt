#lang racket/base
(require racket/contract
         racket/match
         plai/datatype
         (prefix-in x86: "asm.rkt"))

#| Add if0
   K = E | (if0 E K K)
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

(define-type K
  [k-E (e E?)]
  [if0 (test E?) (trueb K?) (falsb K?)])

(define (binop-src->asm-helper ->asm)
  (match-lambda*
    [(list x86:eax (? x86:register? r))
     (->asm r)]))

(define (comparison-operator->asm op)
  (binop-src->asm-helper
   (λ (r)
     (x86:seqn
      (x86:cmp x86:eax r)
      #;(x86:mov x86:eax 0)
      (op x86:al)))))

(define binops
  (hash '+ x86:add
        '- x86:sub
        'bitwise-and x86:and
        'bitwise-ior x86:or
        'bitwise-xor x86:xor
        '* (binop-src->asm-helper x86:imul)
        'quotient (binop-src->asm-helper x86:idiv)
        'remainder (binop-src->asm-helper 
                    (λ (r) 
                      (x86:seqn
                       (x86:idiv r)
                       (x86:mov x86:eax x86:edx))))
        '= (comparison-operator->asm x86:sete)
        '< (comparison-operator->asm x86:setl)
        '<= (comparison-operator->asm x86:setle)
        '> (comparison-operator->asm x86:setg)
        '>= (comparison-operator->asm x86:setge)))
(define (binop-src? op) (hash-has-key? binops op))
(define (binop-src->asm op) (hash-ref binops op))

(define unaops
  (hash 'add1 x86:inc
        'sub1 x86:dec
        'bitwise-not x86:not))
(define (unaop-src? op) (hash-has-key? unaops op))
(define (unaop-src->asm op) (hash-ref unaops op))

(define parse-k
  (match-lambda
    [(list 'if0 test trueb falsb)
     (if0 (parse-e test) (parse-k trueb) (parse-k falsb))]
    [expr (k-E (parse-e expr))]))

(define parse-e
  (match-lambda
    [(list (? binop-src? operator) lhs rhs)
     (binop operator (parse-e lhs) (parse-e rhs))]
    [(list (? unaop-src? operator) operand)
     (unaop operator (parse-e operand))]
    [(? byte? b)
     (num b)]))

(define (k->asm pp)
  (type-case K pp
    [if0 
     (test trueb falsb)
     (let ([falsb-start (x86:make-label 'falsb-start)]
           [if0-end (x86:make-label 'if0-end)])
       (x86:seqn
        (e->asm test)
        (x86:cmp x86:eax 0)
        (x86:jne falsb-start)
        (k->asm trueb)
        (x86:jmp if0-end)
        (x86:label-mark falsb-start)
        (k->asm falsb)
        (x86:label-mark if0-end)))]
    [k-E (e) (e->asm e)]))

(define (e->asm pp)
  (type-case E pp
    [binop 
     (op lhs rhs)
     (x86:seqn
      (x86:push x86:ebx)
      (e->asm rhs)
      (x86:mov x86:ebx x86:eax)
      (e->asm lhs)
      ((binop-src->asm op) x86:eax x86:ebx)
      (x86:pop x86:ebx))]
    [unaop 
     (operator operand)
     (x86:seqn
      (e->asm operand)
      ((unaop-src->asm operator) x86:eax))]
    [num 
     (b)
     (x86:seqn
      (x86:mov x86:eax b))]))

(define (comparison-operator->rkt op)
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
    ['= (comparison-operator->rkt =)]
    ['< (comparison-operator->rkt <)]
    ['<= (comparison-operator->rkt <=)]
    ['> (comparison-operator->rkt >)]
    ['>= (comparison-operator->rkt >=)]))

(define (interp-k pp)
  (type-case K pp
    [if0 
     (test trueb falsb)
     (if (= 0 (interp-e test))
         (interp-k trueb)
         (interp-k falsb))]
    [k-E (e) (interp-e e)]))

(define (interp-e pp)
  (type-case E pp
    [binop 
     (opor lhs rhs)
     ((op-src->rkt opor)
      (interp-e lhs)
      (interp-e rhs))]
    [unaop 
     (opor opand)
     ((op-src->rkt opor)
      (interp-e opand))]
    [num (b) b]))

(provide
 (contract-out
  [rename parse-k parse (-> any/c K?)]
  [rename k->asm to-asm (-> K? x86:asm?)]
  [rename interp-k interp (-> K? any/c)]))