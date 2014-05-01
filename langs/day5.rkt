#lang racket/base
(require racket/contract
         racket/match
         racket/set
         (prefix-in x86: "asm.rkt"))

;; define app

(struct e () #:prefab)
(struct num e (n) #:prefab)

(struct binop e (opor lhs rhs) #:prefab)
(struct unaop e (opor opand) #:prefab)

(struct mult e (lhs rhs) #:prefab)
(struct quot e (lhs rhs) #:prefab)
(struct rem e (lhs rhs) #:prefab)

(struct boolop e (opor lhs rhs) #:prefab)

(struct if0 e (test trueb falsb) #:prefab)

(struct definition-seq e (defines body) #:prefab)
(struct definition e (naming variables body) #:prefab) ; limit variables to 4?
(struct application e (function inputs) #:prefab)
(struct id e (name) #:prefab)

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

(define boolops
  (set '= '< '<= '> '>=))

(define reserved-words
  (set-union (set '* 'quotient 'remainder 'if0 'define 'app)
             (list->set boolops)
             (list->set (hash-keys binops))
             (list->set(hash-keys unaops))))


(define (valid-id? pid) ; enforce which naming convention? [a-zA-Z]?
  (and (symbol? pid) 
       (not (set-member? reserved-words pid))))

(define parse
  (match-lambda
    [(list (? (λ (i) #t) (list (? (λ (s) (equal? s 'define))) _ _) defs) ... app-body)
     (definition-seq (map parse defs) (parse app-body))]
    [(list (? (λ (s) (equal? s 'define)))
           (list (? valid-id? the-id) (? valid-id? ids)...)
           function-body)
     (definition (id the-id) (map parse ids) (parse function-body))]
    [(list (? (λ (s) (equal? s 'app)))
           (? valid-id? fid)
           params ...)
     (application (id fid) (map parse params))]
    [(list (? (λ (s) (equal? s 'if0))) test tb fb)
     (if0 (parse test) (parse tb) (parse fb))]
    [(list (? (λ (s) (set-member? boolops s)) op) lhs rhs)
     (boolop op (parse lhs) (parse rhs))]
    [(list (? (λ (s) (equal? s '*))) lhs rhs)
     (mult (parse lhs) (parse rhs))]
    [(list (? (λ (s) (equal? s 'quotient))) lhs rhs)
     (quot (parse lhs) (parse rhs))]
    [(list (? (λ (s) (equal? s 'remainder))) lhs rhs)
     (rem (parse lhs) (parse rhs))]
    [(list (? (λ (s) (and (symbol? s) (hash-has-key? binops s))) 
              operator)
           lhs
           rhs)
     (binop (hash-ref binops operator) (parse lhs) (parse rhs))]
    [(list (? (λ (s) (and (symbol? s) (hash-has-key? unaops s)))
              operator)
           operand)
     (unaop (hash-ref unaops operator) (parse operand))]
    [(? valid-id? i)
     (id i)]
    [(? byte? b)
     (num b)]))

;; eax is default return value
(define to-asm
  (parameterize ([gamma (hash)])
    (match-lambda
      [(if0 test trueb falsb) ; optimize away? not as default
       (let ([trueb-end (x86:make-label)]
             [falsb-end (x86:make-label)])
         (x86:seqn
          (to-asm test)
          (x86:cmp x86:eax 0)
          (x86:jne trueb-end)
          (to-asm trueb)
          (x86:jmp falsb-end)
          (x86:label-mark trueb-end)
          (to-asm falsb)
          (x86:label-mark falsb-end)))]
      [(boolop op l r)
       (x86:seqn
        (to-asm l)
        (x86:push x86:eax)
        (to-asm r)
        (x86:mov x86:ebx x86:eax)
        (x86:pop x86:eax)
        (x86:cmp x86:eax x86:ebx)
        #;(x86:mov x86:eax 0)
        ((match op
           ['= x86:sete]
           ['< x86:setl]
           ['<= x86:setle]
           ['> x86:setg]
           ['>= x86:setge])
         x86:al))]
      [(mult l r)
       (x86:seqn
        (to-asm l)
        (x86:mov x86:ebx x86:eax)
        (to-asm r)
        (x86:imul x86:ebx))]
      [(quot l r)
       (x86:seqn
        (to-asm r)
        (x86:mov x86:ebx x86:eax)
        (to-asm l)
        #;(x86:cdq)
        (x86:idiv x86:ebx))]
      [(rem l r)
       (x86:seqn
        (to-asm r)
        (x86:mov x86:ebx x86:eax)
        (to-asm l)
        #;(x86:cdq)
        (x86:idiv x86:ebx)
        (x86:mov x86:eax x86:edx))]
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
        (x86:mov x86:eax b))])))

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

(define gamma (make-parameter (hash)))

(define interp
  (match-lambda
    [(definition-seq defines body) ;; make definition-seq work like let*?
     (parameterize 
         ([gamma (for/fold ([g (gamma)])
                   ([def defines])
                   (hash-set g (definition-naming def) def))])
       (interp body))]
    [(definition naming vars body)
     (error)]
    [(application function values) ;; remove self recursion?
     (let ([fd (hash-ref (gamma) function)])
       (parameterize 
           ([gamma (for/fold ([g (gamma)])
                     ([var (definition-variables fd)]
                      [val values])
                     (hash-set g var val))])
         (interp (definition-body fd))))]
    [(if0 test trueb falsb) ;; should if0 be short-circuiting?
     (if (= 0 (interp test))
         (interp trueb)
         (interp falsb))]
    [(boolop op lhs rhs)
     (if ((match op
            ['= =]
            ['< <]
            ['<= <=]
            ['> >]
            ['>= >=]) (interp lhs) (interp rhs))
         1
         0)]
    [(mult lhs rhs)
     (* (interp lhs)
        (interp rhs))]
    [(quot lhs rhs)
     (quotient
      (interp lhs)
      (interp rhs))]
    [(rem lhs rhs)
     (remainder
      (interp lhs)
      (interp rhs))]
    [(binop opor lhs rhs)
     ((x86-op->racket-op opor)
      (interp lhs)
      (interp rhs))]
    [(unaop opor opand)
     ((x86-op->racket-op opor)
      (interp opand))]
    [(id i)
     (interp (hash-ref (gamma) (id i)))]
    [(num b)
     b]))

(provide
 (contract-out
  [struct e ()]
  [struct (num e) ([n byte?])]
  [parse (-> any/c e?)]
  [to-asm (-> e? x86:asm?)]
  [interp (-> e? any/c)]))