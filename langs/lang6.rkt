#lang racket/base
(require racket/contract
         racket/match
         racket/list
         plai/datatype
         "../substitution-cipher.rkt"
         (prefix-in x86: "asm.rkt"))

#| Add func-addr. 
   Applications expect a function-pointer as their first arg
   P  = (D ... K) | K
   D  = (define (<id> <id> ...4) K)
   K = (app E E ...4) | (if0 E K K) | E
   E  = <id> | <num> | (<binop> E E) | (<unaop> E) | (func-addr <id>)
   <binop> =  + | - | bitwise-and | bitwise-ior | bitwise-xor
            | * | quotient | remainder
            | = | < | <= | > | >=
   <unaop> = add1 | sub1 | bitwise-not
   <id> = [^ ()[]{}",'`:#\| ]+ and not a reserved word
|#

(define (valid-id? pid)
  (and (symbol? pid) 
       (not (or (unaop? pid) (binop? pid) (eq? 'if0 pid) 
                (eq? 'define pid) (eq? 'app pid)))))

(define-type E
  [Num (n number?)]
  [Binop (op binop-src?) (lhs E?) (rhs E?)]
  [Unaop (opor unaop-src?) (opand E?)]
  [Id (i valid-id?)]
  [Func-addr (i valid-id?)])

(define-type K
  [App 
   (function-pointer E?) 
   (inputs (listof E?))]
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
      (x86:mov x86:eax 0)
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
    [(list 'app func-ptr params ...)
     (App (parse-e func-ptr) (map parse-e params))]
    [(list 'if0 test trueb falsb)
     (If0 (parse-e test) (parse-k trueb) (parse-k falsb))]
    [expr (Return (parse-e expr))]))

(define parse-e
  (match-lambda
    [(list 'func-addr (? valid-id? i))
     (Func-addr i)]
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
    [else (error)]))
(define (Def-name d)
  (type-case D d [Def (naming vars body) naming]))
(define (Def-vars d)
  (type-case D d [Def (naming vars body) vars]))

(define end-label (x86:make-label 'program-end))
(define stack-offset (make-parameter 0))

(define to-asm (λ (pp) (p->asm pp (hash))))
(define (p->asm pp gamma)
  (type-case P pp
    [Program
     (defs body)
     (let ([g
            (for/fold ([gm gamma])
              ([def defs])
              (hash-set gm
                        (Id-name (Def-name def))
                        (x86:make-label
                         (Id-name (Def-name def)))))])
       (x86:seqn
        (k->asm body g empty)
        (apply x86:seqn 
               (for/list ([def defs])
                 (x86:seqn
                  (x86:label-mark (hash-ref g (Id-name (Def-naming def))))
                  (d->asm def g))))
        (x86:label-mark end-label)))]
    [P-K (k) (x86:seqn (k->asm k gamma empty) (x86:label-mark end-label))]))

(define (d->asm pp gamma)
  (type-case D pp
    [Def
     (naming vars body)
     (k->asm body
             (for/fold ([g gamma])
               ([v vars]
                [n (in-naturals)])
               (hash-set g (Id-name v) (cons 'stack-pos n)))
             vars)]))

(define (k->asm pp gamma old-args)
  (type-case K pp
    [If0 
     (test trueb falsb)
     (let ([falsb-start (x86:make-label 'falsb-start)])
       (x86:seqn
        (e->asm test gamma)
        (x86:cmp x86:eax 0)
        (x86:jne falsb-start)
        (k->asm trueb gamma old-args)
        (x86:label-mark falsb-start)
        (k->asm falsb gamma old-args)))]
    [App
     (function-pointer inputs)
     (x86:seqn
      ;; calculate and place on the stack the value of each input
      (apply x86:seqn
             (for/list ([i (reverse inputs)]
                        [so (in-naturals)])
               (x86:seqn
                (parameterize ([stack-offset so])
                  (e->asm i gamma))
                (x86:push x86:eax))))
      (if (> (length old-args) 0)
          (x86:seqn
           (x86:comment "Trim the old function variables from the stack")
           (apply x86:seqn
                  (for/list ([i (in-range (sub1 (length inputs)) -1 -1)])
                    (x86:seqn (x86:mov x86:eax (x86:esp+ (* 4 i)))
                              (x86:mov (x86:esp+ 
                                        (* 4 (+ i (length old-args)))) 
                                       x86:eax))))
             (x86:add x86:esp 
                      (* 4 (length old-args))));sub if stack grows other way
            (x86:seqn))
      (e->asm function-pointer gamma)
      (x86:jmp x86:eax))]
    [Return (e) (x86:seqn (e->asm e gamma) (x86:jmp end-label))]))

(define (e->asm pp gamma)
  (type-case E pp
    [Func-addr
     (ident)
     (x86:seqn
      (x86:mov x86:eax (x86:addr-of-label
                        (hash-ref gamma ident))))]
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
               (match (hash-ref gamma i)
                 [(cons 'stack-pos sp)
                  (x86:esp+ (* 4 (+ sp (stack-offset))))]
                 [lbl (x86:addr-of-label lbl)])))]))

(define (interp pp) (interp-p pp (hash)))
(define (interp-p pp gamma)
  (type-case P pp
    [Program
     (defs body)
     (interp-k body
               (for/fold ([g gamma])
                 ([def defs])
                 (hash-set g (Id-name (Def-naming def)) def)))]
    [P-K (k) (interp-k k gamma)]))

(define (interp-k pp gamma)
  (type-case K pp
    [If0 
     (test trueb falsb)
     (if (= 0 (interp-e test gamma))
         (interp-k trueb gamma)
         (interp-k falsb gamma))]
    [App
     (function-addr inputs)
     (let ([fd 
            (hash-ref gamma 
                      (num->symbol (interp-e function-addr gamma))
                      (λ () (error "Not a valid function: " function-addr)))])
       (interp-k (Def-body fd)
                 (for/fold ([g gamma])
                   ([var (map Id-name (Def-variables fd))]
                    [val (map (λ (i) (interp-e i gamma)) inputs)])
                   (hash-set g var val))))]
    [Return (e) (interp-e e gamma)]))

(define (symbol->num s)
  (for/fold ([a 0])
      ([b (reverse (bytes->list (string->bytes/utf-8 (symbol->string s))))]
       [i (in-naturals)])
    (+ a (* (expt 256 i) b))))

(define (num->symbol n)
  (let-values 
      ([(z lobys)
        (for/fold ([n-r n]
                   [lobys empty])
          ([i (in-range (ceiling (/ (log (max n 1)) (log 256))))])
          (values (quotient n-r 256) (cons (modulo n-r 256) lobys)))]) 
  (string->symbol (bytes->string/utf-8 (list->bytes lobys)))))

(define (interp-e pp gamma)
  (type-case E pp
    [Func-addr
     (ident)
     (symbol->num ident)]
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
              (cond 
                [(E? v) (interp-e v gamma)]
                [(D? v) (symbol->num i)]
                [else v]))]))

(provide
 (contract-out
  [rename parse-p parse (-> any/c P?)]
  [to-asm (-> P? x86:asm?)]
  [interp (-> P? any/c)]))