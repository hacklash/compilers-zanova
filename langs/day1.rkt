#lang racket/base
(require racket/contract
         racket/match
         (prefix-in x86: "asm.rkt"))

;; + - and or xor inc dec not

(struct e () #:prefab)
(struct num e (n) #:prefab)

(struct binop e (opor lhs rhs) #:prefab)
(struct unaop e (opor opand) #:prefab)

(define binop-assoc?
  (match-lambda
    [(or '- (? (λ (op) (equal? op x86:sub)))) #f]
    [_ #t]))

(define binops
  (hash '+ x86:add
        '- x86:sub
        'bitwise-and x86:and
        'bitwise-ior x86:or
        'bitwise-xor x86:xor))
(define unaops
  (hash 'add1 x86:inc
        'sub1 x86:dec
        'bitwise-not x86:not))

(define parse
  (match-lambda
    [(list (? (λ (s) (and (symbol? s) (hash-has-key? binops s))) 
              operator)
           lhs
           rhs)
     (binop (hash-ref binops operator) (parse lhs) (parse rhs))]
    [(list (? (λ (s) (and (symbol? s) (hash-has-key? unaops s)))
              operator)
           operand)
     (unaop (hash-ref unaops operator) (parse operand))]
    [(? byte? b)
     (num b)]))

;; eax is default return value
;; for binary operators do one side first, store on stack and do the other side
(define to-asm
  (match-lambda
    [(binop o l r)
     (x86:seqn
      (to-asm l)
      (x86:push x86:eax)
      (to-asm r)
      (if (binop-assoc? o)
          (x86:pop x86:ebx)
          (x86:seqn
           (x86:mov x86:ebx x86:eax)
           (x86:pop x86:eax)))
      
      (o x86:eax x86:ebx))]
    [(unaop operator operand)
     (x86:seqn
      (to-asm operand)
      (operator x86:eax))]
    [(num b)
     (x86:seqn
      (x86:mov x86:eax b))]))

(define (x86-op->racket-op o)
  (cond
    [(equal? o x86:add) +]
    [(equal? o x86:sub) -]
    [(equal? o x86:and) bitwise-and]
    [(equal? o x86:or) bitwise-ior]
    [(equal? o x86:xor) bitwise-xor]
    [(equal? o x86:inc) add1]
    [(equal? o x86:dec) sub1]
    [(equal? o x86:not) bitwise-not]))

(define interp
  (match-lambda
    [(binop opor lhs rhs)
     ((x86-op->racket-op opor)
      (interp lhs)
      (interp rhs))]
    [(unaop opor opand)
     ((x86-op->racket-op opor)
      (interp opand))]
    [(num b)
     b]))


(provide
 (contract-out
  [struct e ()]
  [struct (num e) ([n byte?])]
  [parse (-> any/c e?)]
  [to-asm (-> e? x86:asm?)]
  [interp (-> e? any/c)]))
