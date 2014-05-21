#lang racket/base
(require racket/contract
         racket/match
         plai/datatype
         "../substitution-cipher.rkt"
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
  [Num (n number?)]
  [Binop (op binop-src?) (lhs E?) (rhs E?)]
  [Unaop (opor unaop-src?) (opand E?)])

(define-type K
  [Return (e E?)]
  [If0 (test E?) (trueb K?) (falsb K?)])

(define (binop-src->asm-helper ->asm)
  (match-lambda*
    [(list x86:eax (? x86:register? r))
     (->asm r)]))

(define (cmpop->asm op)
  (binop-src->asm-helper
   (λ (r)
     (x86:seqn
      (x86:cmp x86:eax r)
      #;(x86:mov x86:eax 0)
      (op x86:al)))))

(define (cmpop->rkt op)
  (match-lambda* [(list l r) (if (op l r) 1 0)]))

(define-subst-cipher
  binop
  [src          rkt         asm]
  ['+           +           x86:add]
  ['-           -           x86:sub]
  ['bitwise-and bitwise-and x86:and]
  ['bitwise-ior bitwise-ior x86:or]
  ['bitwise-xor bitwise-xor x86:xor]
  ['*           *           (binop-src->asm-helper x86:imul)]
  ['quotient    quotient    (binop-src->asm-helper x86:idiv)]
  ['remainder   remainder   (binop-src->asm-helper 
                             (λ (r) 
                               (x86:seqn
                                (x86:idiv r)
                                (x86:mov x86:eax x86:edx))))]
  ['=       (cmpop->rkt =)  (cmpop->asm x86:sete)]
  ['<       (cmpop->rkt <)  (cmpop->asm x86:setl)]
  ['<=      (cmpop->rkt <=) (cmpop->asm x86:setle)]
  ['>       (cmpop->rkt >)  (cmpop->asm x86:setg)]
  ['>=      (cmpop->rkt >=) (cmpop->asm x86:setge)])

(define-subst-cipher
  unaop
  [src          rkt         asm]
  ['add1        add1        x86:inc]
  ['sub1        sub1        x86:dec]
  ['bitwise-not bitwise-not x86:not])

(define parse-k
  (match-lambda
    [(list 'if0 test trueb falsb)
     (If0 (parse-e test) (parse-k trueb) (parse-k falsb))]
    [expr (Return (parse-e expr))]))

(define parse-e
  (match-lambda
    [(list (? binop-src? operator) lhs rhs)
     (Binop operator (parse-e lhs) (parse-e rhs))]
    [(list (? unaop-src? operator) operand)
     (Unaop operator (parse-e operand))]
    [(? byte? b)
     (Num b)]))

(define (k->asm pp)
  (type-case K pp
    [If0 
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
    [Return (e) (e->asm e)]))

(define (e->asm pp)
  (type-case E pp
    [Binop 
     (op lhs rhs)
     (x86:seqn
      (x86:push x86:ebx)
      (e->asm rhs)
      (x86:mov x86:ebx x86:eax)
      (e->asm lhs)
      ((binop-src->asm op) x86:eax x86:ebx)
      (x86:pop x86:ebx))]
    [Unaop 
     (operator operand)
     (x86:seqn
      (e->asm operand)
      ((unaop-src->asm operator) x86:eax))]
    [Num 
     (b)
     (x86:seqn
      (x86:mov x86:eax b))]))

(define (interp-k pp)
  (type-case K pp
    [If0 
     (test trueb falsb)
     (if (= 0 (interp-e test))
         (interp-k trueb)
         (interp-k falsb))]
    [Return (e) (interp-e e)]))

(define (interp-e pp)
  (type-case E pp
    [Binop 
     (opor lhs rhs)
     ((binop-src->rkt opor)
      (interp-e lhs)
      (interp-e rhs))]
    [Unaop 
     (opor opand)
     ((unaop-src->rkt opor)
      (interp-e opand))]
    [Num (b) b]))

(provide
 (contract-out
  [rename parse-k parse (-> any/c K?)]
  [rename k->asm to-asm (-> K? x86:asm?)]
  [rename interp-k interp (-> K? any/c)]))