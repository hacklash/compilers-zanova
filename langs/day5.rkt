#lang racket/base
(require racket/contract
         racket/match
         racket/set
         racket/list
         plai/datatype
         "../substitution-cipher.rkt"
         (prefix-in x86: "asm.rkt"))

#| Add (define app). Functions don't return
   P  = (D ... K) | K
   D  = (define (<id> <id> ...4) K)
   K = (app <id> E ...4) | (if0 E K K) | E
   E  = <id> | <num> | (<binop> E E) | (<unaop> E)
   <binop> =  + | - | bitwise-and | bitwise-ior | bitwise-xor
            | * | quotient | remainder
            | = | < | <= | > | >=
   <unaop> = add1 | sub1 | bitwise-not
   <id> = [^ ()[]{}",'`:#\| ]+ and not a reserved word
|#

(define (valid-id? pid) ;XX enforce slightly stricter than racket symbols
  #;(and (regexp-match? #rx"[^\\(\\)\\[\\]{}\\\",'`;#\\|\\\\]+" 
                        (symbol->string pid)))
  (and (symbol? pid) 
       (not (or (unaop? pid) (binop? pid) (eq? 'if0 pid) 
                (eq? 'define pid) (eq? 'app pid)))))

(define-type E
  [Num (n number?)]
  [Binop (op binop-src?) (lhs E?) (rhs E?)]
  [Unaop (opor unaop-src?) (opand E?)]
  [Id (i valid-id?)])

(define-type K
  [App 
   (function Id?) 
   (inputs (λ (is) (andmap E? is)))]
  [If0 (test E?) (trueb K?) (falsb K?)]
  [Return (e E?)])

(define-type D
  [Def 
   (naming Id?) 
   (variables (λ (vs) (andmap Id? vs)))
   (body K?)])

(define-type P 
  [Program 
   (defines (λ (ds) (andmap D? ds)))
   (body K?)]
  [P-K (k K?)])

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

(define parse-p
  (match-lambda
    [(list (and (list 'define _ _) defs) ... app-body)
     (Program (map parse-d defs) (parse-k app-body))]
    [k-expr (P-K (parse-k k-expr))]))

(define parse-d
  (match-lambda
    [(list 'define (list (? valid-id? the-id) (? valid-id? ids) ...) body)
     (Def (Id the-id) (map Id ids) (parse-k body))]))

(define parse-k
  (match-lambda
    [(list 'app (? valid-id? fid) params ...)
     (App (Id fid) (map parse-e params))]
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
     (Num b)]
    [(? valid-id? i)
     (Id i)]))

(define (Id-name e)
  (type-case E e
    [Id (i) i]
    [Num (b) (error)][Unaop (r d) (error)][Binop (o l r) (error)]))
(define (Def-name d)
  (type-case D d [Def (naming vars body) naming]))

(define end-label (x86:make-label 'program-end))
(define stack-offset (make-parameter 0))

(define to-asm (λ (pp) (p->asm pp (hash))))
(define (p->asm pp gamma)
  (type-case P pp
    [Program
     (defs body)
     (let ([g
            (for/fold ([g gamma])
              ([def defs])
              (hash-set g
                        (Def-name def)
                        (x86:make-label
                         (Id-name (Def-name def)))))])
       (x86:seqn
        (k->asm body g)
        (apply x86:seqn 
               (for/list ([def defs])
                 (x86:seqn
                  (x86:label-mark (hash-ref g (Def-naming def)))
                  (d->asm def g))))
        (x86:label-mark end-label)))]
    [P-K (k) (k->asm k gamma)]))

(define (d->asm pp gamma)
  (type-case D pp
    [Def
     (naming vars body)
     (x86:seqn
      (k->asm body
              (for/fold ([g gamma])
                ([v vars]
                 [n (in-naturals)])
                (hash-set g v (cons 'stack-pos n))))
      (x86:jmp end-label))]))

(define (k->asm pp gamma)
  (type-case K pp
    [If0 
     (test trueb falsb)
     (let ([falsb-start (x86:make-label 'falsb-start)]
           [if0-end (x86:make-label 'if0-end)])
       (x86:seqn
        (e->asm test gamma)
        (x86:cmp x86:eax 0)
        (x86:jne falsb-start)
        (k->asm trueb gamma)
        (x86:jmp if0-end)
        (x86:label-mark falsb-start)
        (k->asm falsb gamma)
        (x86:label-mark if0-end)))]
    [App
     (function inputs)
     (x86:seqn
      ;; calculate and place on the stack the value of each input
      (apply x86:seqn
             (for/list ([i (reverse 
                            (take (append inputs 
                                          (map parse-e (list 0 0 0 0))) 
                                  4))]
                        [so (in-naturals)])
               (x86:seqn
                (parameterize ([stack-offset so])
                  (e->asm i gamma))
                (x86:push x86:eax))))
      ;;XX clear the stack of anything but the new input values
      (let ([stack-clean-end (x86:make-label 'stack-clean-end)])
        (x86:seqn
         (x86:comment "Clean the stack")
         (x86:mov x86:eax x86:esp)
         (x86:sub x86:eax x86:ebp)
         (x86:mov x86:ebx 28)
         (x86:cmp x86:eax x86:ebx)
         (x86:jne stack-clean-end)
         (x86:mov x86:eax (x86:esp+ 0))
         (x86:mov (x86:esp+ 16) x86:eax)
         (x86:mov x86:eax (x86:esp+ 4))
         (x86:mov (x86:esp+ 20) x86:eax)
         (x86:mov x86:eax (x86:esp+ 8))
         (x86:mov (x86:esp+ 24) x86:eax)
         (x86:mov x86:eax (x86:esp+ 12))
         (x86:mov (x86:esp+ 28) x86:eax)
         (x86:pop x86:eax)(x86:pop x86:eax)(x86:pop x86:eax)(x86:pop x86:eax)
         (x86:label-mark stack-clean-end)))
      (x86:jmp (hash-ref gamma function)))]
    [Return (e) (e->asm e gamma)]))

(define (e->asm pp gamma)
  (type-case E pp
    [Binop 
     (op lhs rhs)
     (x86:seqn
      (x86:push x86:ebx)
      (parameterize ([stack-offset (add1 (stack-offset))])
        (x86:seqn
         (e->asm rhs gamma)
         (x86:mov x86:ebx x86:eax)
         (e->asm lhs gamma)
         ((binop-src->asm op) x86:eax x86:ebx)))
      (x86:pop x86:ebx))]
    [Unaop 
     (operator operand)
     (x86:seqn
      (e->asm operand gamma)
      ((unaop-src->asm operator) x86:eax))]
    [Num 
     (b)
     (x86:seqn
      (x86:mov x86:eax b))]
    [Id
     (i)
     (x86:seqn
      (x86:mov x86:eax
               ((match-lambda 
                  [(cons 'stack-pos sp) 
                   (x86:esp+ (* 4 (+ sp (stack-offset))))])
                (hash-ref gamma (Id i)))))]))

(define interp (λ (pp) (interp-p pp (hash))))
(define (interp-p pp gamma)
  (type-case P pp
    [Program
     (defs body)
     (interp-k body
               (for/fold ([g gamma])
                 ([def defs])
                 (hash-set g (Def-naming def) def)))]
    [P-K (k) (interp-k k gamma)]))

(define (interp-k pp gamma)
  (type-case K pp
    [If0 
     (test trueb falsb)
     (if (= 0 (interp-e test gamma))
         (interp-k trueb gamma)
         (interp-k falsb gamma))]
    [App
     (function inputs)
     (let ([fd (hash-ref gamma function)])
       (interp-k (Def-body fd)
                 (for/fold ([g gamma])
                   ([var (map Id-name (Def-variables fd))]
                    [val (map (λ (i) (interp-e i gamma)) inputs)])
                   (hash-set g var val))))]
    [Return (e) (interp-e e  gamma)]))

(define (interp-e pp gamma)
  (type-case E pp
    [Binop 
     (opor lhs rhs)
     ((binop-src->rkt opor)
      (interp-e lhs gamma)
      (interp-e rhs gamma))]
    [Unaop 
     (opor opand)
     ((unaop-src->rkt opor)
      (interp-e opand gamma))]
    [Num (b) b]
    [Id (i) (let ([v (hash-ref gamma i)])
              (if (E? v) (interp-e v gamma) v))]))

(provide
 (contract-out
  [rename parse-p parse (-> any/c P?)]
  [to-asm (-> P? x86:asm?)]
  [interp (-> P? any/c)]))