#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse))

(define-syntax (define-subst-cipher stx)
  (syntax-parse stx
    [(_ name:id [lang:id ...] entry ...)
     (with-syntax
         ([name? (format-id #'name "~a?" #'name)]
          [((name-lang0? entry0 ...) ...)
           (for/list ([lang0 (in-list (syntax->list #'(lang ...)))]
                      [i (in-naturals)])
             (cons (format-id #'name "~a-~a?" #'name lang0)
                   (for/list ([entry (map syntax->list (syntax->list #'(entry ...)))]
                              #:when ((length entry) . > . i))
                     (list-ref entry i))))]
          [((name-lang1->lang2 (entry1 entry2) ...) ...)
           (append*
            (for/list ([lang1 (in-list (syntax->list #'(lang ...)))]
                       [i (in-naturals)])
              (for/list ([lang2 (in-list (syntax->list #'(lang ...)))]
                         [j (in-naturals)])
                (cons (format-id #'name "~a-~a->~a" #'name lang1 lang2)
                      (for/list
                          ([entry (map syntax->list (syntax->list #'(entry ...)))]
                           #:when ((length entry) . > . i)
                           #:when ((length entry) . > . j))
                        (list (list-ref entry i)
                              (list-ref entry j)))))))])
       (syntax/loc stx
         (begin
           (define (name? s)
             (or (name-lang0? s) ...))
           (define (name-lang0? s)
             (cond
               [(eq? entry0 s) #t]
               ...
               [else #f]))
           ...
           (define (name-lang1->lang2 s)
             (cond
               [(eq? entry1 s) entry2]
               ...
               [else #f]))
           ...)))]))

(module+ test
  (require rackunit)
  
  (define-subst-cipher
    unaop
    [src          asm      rkt]
    ['add1        'x86:inc add1]
    ['sub1        'x86:dec sub1]
    ['bitwise-not 'x86:not bitwise-not]
    ['garbage     'x86:seqn])
  
  ;; Tests
  (check-true (andmap unaop? (list 'add1 'sub1 'bitwise-not
                                   'x86:inc 'x86:dec 'x86:not
                                   add1 sub1 bitwise-not
                                   'garbage 'x86:seqn)))
  (check-false (unaop? 'random-thing))
  
  (check-true (and
               (andmap unaop-src? '(add1 sub1 bitwise-not garbage))
               (andmap unaop-asm? '(x86:inc x86:dec x86:not x86:seqn))
               (andmap unaop-rkt? (list add1 sub1 bitwise-not))))
  (check-equal? (unaop-src->asm 'add1) 'x86:inc)
  (check-equal? (unaop-src->asm 'sub1) 'x86:dec)
  (check-equal? (unaop-src->asm 'bitwise-not) 'x86:not)
  (check-equal? (unaop-asm->src 'x86:inc) 'add1)
  (check-equal? (unaop-asm->src 'x86:dec) 'sub1)
  (check-equal? (unaop-asm->src 'x86:not) 'bitwise-not)
  (check-equal? (unaop-rkt->asm add1) 'x86:inc)
  (check-equal? (unaop-rkt->asm sub1) 'x86:dec)
  (check-equal? (unaop-rkt->asm bitwise-not) 'x86:not)
  (check-equal? (unaop-asm->rkt 'x86:inc) add1)
  (check-equal? (unaop-asm->rkt 'x86:dec) sub1)
  (check-equal? (unaop-asm->rkt 'x86:not) bitwise-not)
  (check-equal? (unaop-src->rkt 'add1) add1)
  (check-equal? (unaop-src->rkt 'sub1) sub1)
  (check-equal? (unaop-src->rkt 'bitwise-not) bitwise-not)
  (check-equal? (unaop-rkt->src add1) 'add1)
  (check-equal? (unaop-rkt->src sub1) 'sub1)
  (check-equal? (unaop-rkt->src bitwise-not) 'bitwise-not)
  (check-equal? (unaop-src->asm 'garbage) 'x86:seqn)
  (check-equal? (unaop-asm->src 'x86:seqn) 'garbage))

(provide define-subst-cipher)
