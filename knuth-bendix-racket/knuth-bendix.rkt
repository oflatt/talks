#lang racket

(module+ test (require rackunit))
(provide (all-defined-out))

(define global-var-counter 5)
(define (reset-var-counter)
  (set! global-var-counter 0))


(define axioms
  (list
   (list `(+ 0 v0) `v0)
   (list `(+ (- v2) v2) `0)
   (list `(+ (+ v3 v4) v5) `(+ v3 (+ v4 v5)))))

(define (more-complex? exp1 exp2)
  (cond
    [(and (list? exp1) (list? exp2))
     (list-more-complex? (rest exp1) (rest exp2))]
    [(list? exp1)
     true]
    [(list? exp2)
     false]
    [else
     false]))

(define (same-complexity? exp1 exp2)
  (and (not (more-complex? exp1 exp2)) (not (more-complex? exp2 exp1))))

(define (list-more-complex? list1 list2)
  (cond
    [(and (empty? list1) (empty? list2))
     false]
    [(empty? list1)
     false]
    [(empty? list2)
     true]
    [(same-complexity? (first list1) (first list2))
     (list-more-complex? (rest list1) (rest list2))]
    [(more-complex? (first list1) (first list2))
     true]
    [else
     false]))

(define (identity? rule)
  (and (symbol? (first rule)) (symbol? (second rule))))

;; ################### Knuth-bendix #########################

(define (orient-axiom axiom)
  (cond
    [(more-complex? (first axiom) (second axiom))
     axiom]
    [(more-complex? (second axiom) (first axiom))
     (list (second axiom) (first axiom))]
    [else
     false]))


;; selection strategy
(define (find-smallest axioms)
  (cond
    [(empty? (rest axioms))
     (first axioms)]
    [else
     (let ([r (find-smallest (rest axioms))])
       (if
        (> (length (flatten (first axioms))) (length (flatten r)))
        r
        (first axioms)))]))

(define (knuth-bendix axioms [rules empty])
  (cond
    [(empty? axioms)
     rules]
    [else
     (define axiom-selected
       (find-smallest axioms))
     (define axioms-updated
       (remove axiom-selected axioms))
     
     ;; normalize selected axiom
     (define axiom-normalized
       (list (rewrite-converge (first axiom-selected) rules)
             (rewrite-converge (second axiom-selected) rules)))
     (define axiom-rewritten
       (rename-vars (list `dummy (first axiom-normalized) (second axiom-normalized)) (make-hash)))

     
     (define axiom
       (orient-axiom
        (list (second axiom-rewritten) (third axiom-rewritten))))

     (cond
       ;; failed to orient
       [(equivalent? (first axiom-normalized) (second axiom-normalized))
        (knuth-bendix axioms-updated rules)]
       [(not axiom)
        (println "Failed to orient axiom: ")
        (println axiom)
        (println axiom-rewritten)
        false]
       [else
        (displayln (format "Adding axiom: ~a" axiom))
        (define new-rules
          (cons axiom rules))
        (define new-axioms
          (add-superpositions axioms-updated axiom new-rules))
        ;(println "New axioms: ")
        ;(for ([r new-axioms])
          ;(println r))
        (knuth-bendix new-axioms
                      new-rules)])]))

;; superimposes the new rule with all the rules to get new axioms
;; for each rule you must superimpose with all subexpressions
(define (add-superpositions axioms new-rule rules)
  (cond
    [(empty? rules)
     axioms]
    [else
     (define critical-terms
       (superimpose (first new-rule) (first (first rules))))

     
     (define critical-pairs
       (if
        (equivalent? (first rules) new-rule)
        (for/list ([critical-term critical-terms])
          (let ([possibilities (rewrite-possibilities critical-term new-rule)])
            (list (first possibilities)
                  (if (< (length possibilities) 2) (first possibilities)
                      (second possibilities)))))
        (for/list ([critical-term critical-terms])
          (list (first (rewrite-possibilities critical-term new-rule))
                (first (rewrite-possibilities critical-term (first rules)))))))

     (add-superpositions
      (append
       critical-pairs
       axioms)
      new-rule
      (rest rules))]))



; ######################## Superimpose ########################

;; like unify but unifies each expression with all subexpressions of the other expression
(define (superimpose exp1 exp2-unrenamed)
  (define exp2 (rename-vars exp2-unrenamed (make-hash)))
  (define subs-exp1
    (for/list ([expr1-subs
                (superimpose-subexpressions exp1 exp2)])
      (rename-vars
       (substitute exp1 expr1-subs) (make-hash))))
  (append
   subs-exp1
   (for/list ([expr2-subs
               (superimpose-subexpressions exp2 exp1)])
     (rename-vars
      (substitute exp2 expr2-subs) (make-hash)))))

;; unify subexpressions of exp1 with exp2 and return substitutions
(define (superimpose-subexpressions exp1 exp2)
  (define subs (make-hash))
  (define unification (unify-helper exp1 exp2 subs))
  
  (define with-unification-subs
    (cond
      [(list? exp1)
       (cons subs
             (foldr
              append
              (list)
              (for/list ([e (rest exp1)])
                (superimpose-subexpressions e exp2))))]
      [else
       (list subs)]))
  (if (or (equal? unification false) (symbol? exp1) (symbol? exp2))
      (rest with-unification-subs)
      with-unification-subs))

;; ################### Rewrite ############################

(define (rewrite-converge expr rules)
  (define new-expr (rewrite expr rules))
  (if (equal? new-expr expr)
      expr
      (rewrite-converge new-expr rules)))

(define (rewrite expr rules)
  (cond
    [(empty? rules)
     expr]
    [else
     (rewrite (rewrite-one expr (first rules)) (rest rules))]))
      

(define (rewrite-one expr rule)
  (cond
    [(list? expr)
     (match-rewrite
      (cons (first expr)
            (for/list ([e (rest expr)])
              (rewrite-one e rule)))
      rule)]
    [else
     (match-rewrite expr rule)]))

(define (replace-with expr list-of-replacements)
  (for/list ([replaced (replace-list (rest expr) list-of-replacements)])
    (cons (first expr) replaced)))

(define (replace-list list1 list-of-replacements)
  (define (recur)
    (for/list ([l (replace-list (rest list1) (rest list-of-replacements))])
      (cons (first list1) l)))
  (cond
    [(empty? list1)
     empty]
    [else
     (append
      (for/list ([e (first list-of-replacements)])
        (cons e (rest list1)))
      (recur))]))

(define (rewrite-possibilities expr rule)
  (define subs (make-hash))
  (define rewrite
    (if
     (match-expr? expr (first rule) subs)
     (substitute (second rule) subs)
     false))
  (define possibilities
    (cond
      [(list? expr)
       (cons rewrite
             (replace-with expr
                           (for/list ([e (rest expr)])
                             (rewrite-possibilities e rule))))]
      [else
       (list rewrite)]))
  (if
   (equal? rewrite false)
   (rest possibilities)
   possibilities))

(define (match-rewrite expr rule)
  (define subs (make-hash))
  (if
   (match-expr? expr (first rule) subs)
   (substitute (second rule) subs)
   expr))

;; returns true if there is a match, and subs will be populated with variable substitutions
(define (match-expr? expr pattern subs)
  (cond
    [(and (list? expr) (list? pattern))
     (cond
       [(equal? (first expr) (first pattern))
        (for/and ([e (rest expr)] [p (rest pattern)])
          (match-expr? e p subs))]
       [else
        false])]
    [(symbol? pattern)
     (if
      (hash-has-key? subs pattern)
      (equal? (hash-ref subs pattern) expr)
      (begin
        (hash-set! subs pattern expr)
        true))]
    [else
     (equal? pattern expr)]))


;; ################## UNIFY ##################################

(define (rename-vars expr rename-dict)
  (cond
    [(list? expr)
     (cons
      (first expr)
      (for/list ([e (rest expr)])
        (rename-vars e rename-dict)))]
    [(symbol? expr)
     (if
      (hash-has-key? rename-dict expr)
      (hash-ref rename-dict expr)
      (begin
        (set! global-var-counter (+ global-var-counter 1))
        (let ([new-var (string->symbol
                        (string-append "v" (number->string global-var-counter)))])
          (hash-set! rename-dict expr new-var)
          new-var)))]
    [else
     expr]))

(define (equivalent? exp1 exp2)
  (define global-var-count-before global-var-counter)
  (define firste (rename-vars exp1 (make-hash)))
  (set! global-var-counter global-var-count-before)
  (define seconde (rename-vars exp2 (make-hash)))
  (define result
    (equal? firste seconde))
  (set! global-var-counter global-var-count-before)
  result)

(define (substitute expr substitutions)
  (cond
    [(list? expr)
     (for/list ([e expr])
       (substitute e substitutions))]
    [(hash-has-key? substitutions expr)
     (hash-ref substitutions expr)]
    [else
     expr]))

(define (contains-term? exp2 exp1)
  (cond
    [(list? exp2)
     (ormap (lambda (x) (contains-term? x exp1)) exp2)]
    [else
     (equal? exp2 exp1)]))

(define (substitute-new expr for-var substitutions)
  ;; add new substitution
  (hash-set! substitutions for-var expr)
  ;; fix old substitutions
  (for ([(key val) substitutions])
    (hash-set! substitutions key (substitute val substitutions))))

(define (unify exp1 exp2)
  (define subs (make-hash))
  (if
   (unify-helper exp1 exp2 subs)
   (rename-vars (substitute exp1 subs) (make-hash))
   false))

(define (unify-recur exp1 exp2 subs)
  (unify-helper (substitute exp1 subs) (substitute exp2 subs) subs))

(define (unify-helper exp1 exp2 substitutions)
  (cond
    [(equal? exp1 exp2)
     true]
    [(symbol? exp1)
     (if
      (contains-term? exp2 exp1)
      false
      (begin
        (substitute-new exp2 exp1 substitutions)
        true))]
    [(and (list? exp1) (list? exp2))
     (if (and (equal? (length exp1) (length exp2)) (> (length exp1) 0)
              (equal? (first exp1) (first exp2)))
         (for/and ([a (rest exp1)] [b (rest exp2)])
           (unify-recur a b substitutions))
         false)
     ]
    [(and (not (symbol? exp1)) (not (symbol? exp2)))
     false]
    [else (unify-helper exp2 exp1 substitutions)]))



;; ##################################### TESTS #######################
(module+ test
  (define subs (make-hash))
  (substitute-new `(+ a 3) `x subs)

  (define (check-hash l table)
    (check-equal? (length l) (hash-count table))
    (for ([pair l])
      (check-true (hash-has-key? table (first pair)))
      (check-equal? (second pair) (hash-ref table (first pair)))))

  (check-hash (list (list `x `(+ a 3))) subs)

  (substitute-new `b `a subs)
  (check-hash (list (list `x `(+ b 3)) (list `a `b)) subs)

  ;; Unify tests
  (reset-var-counter)
  (check-equal? (unify `(+ x y) `z) `(+ v1 v2))
  (reset-var-counter)
  (check-equal? (unify `(p 11) `(p x)) `(p 11))

  (reset-var-counter)
  (check-equal? (unify `(+ x x) `(+ y z)) `(+ v1 v1))
  (reset-var-counter)
  (check-equal? (unify `(+ x y) `(+ (- a) b)) `(+ (- v1) v2))
  (reset-var-counter)
  (check-equal? (unify `(+ x x) `(+ (- a) b)) `(+ (- v1) (- v1)))
  (reset-var-counter)
  (check-equal? (unify `0 `(+ 0 x)) false)
  
  


  ;; complexity measure tests
  (check-true (more-complex? `(+ (+ x y) z) `(+ z (+ x y))))
  (check-true (more-complex? `(+ 0 1) `0))
  (check-false (more-complex? `(+ a b) `(- c d)))
  (check-true (same-complexity? `(+ 0 1) `(- 2 3)))
  (check-true (same-complexity? `(+ (- 3 4) 1) `(+ (+ 2 3) 0)))


  ;; rewrite tests
  (check-equal? (rewrite-one `(+ 0 x) (list `(+ 0 y) `y)) `x)
  (check-equal? (rewrite-one `0 (list `a `10)) `10)
  (check-equal? (rewrite-one `(+ 2 3) (list `a `10)) `10)
  (check-equal? (rewrite-one `(+ x 3) (list `(+ a a) `10)) `(+ x 3))
  (check-equal? (rewrite-one `(+ x x) (list `(+ a a) `10)) `10)
  (check-equal? (rewrite-one `(+ 3 3) (list `(+ a a) `10)) `10)
  (check-equal? (rewrite-one `(- (- 5 6) 8) (list `(- x y) `(- y x))) `(- 8 (- 6 5)))

  ;; superimpose tests
  (reset-var-counter)
  (check-equal? (superimpose `0 `(+ x y)) empty)
  (reset-var-counter)
  (check-equal? (superimpose `0 `(+ 0 y)) `((+ 0 v2)))
  (reset-var-counter)
  (check-equal? (superimpose `0 `1) empty)
  (reset-var-counter)
  (check-equal? (remove-duplicates (superimpose `(+ 0 x) `(+ y 0))) `((+ 0 0)))
  (reset-var-counter)
  (check-equal? (superimpose `(+ (+ a b) c) `(+ (- x) x))
                `((+ (+ (- v2) v2) v3)))

  (reset-var-counter)
  (check-equal? (superimpose `(+ (+ x y) x) `(+ 0 c))
                `((+ (+ 0 v2) 0)))

  (reset-var-counter)
  (check-equal? (orient-axiom (list `(+ 0 x) `a)) (list `(+ 0 x) `a))
  (check-true (equivalent? `(+ (+ a b) c) `(+ (+ c d) e)))
  (check-true (equivalent? `(+ (+ 0 v0) v16) `(+ (+ 0 v12) v17)))
  (check-false (equivalent? `(+ (+ a b) c) `(+ (+ c c) e)))

  (define R4 `(+ (- x) (+ x y)))
  (define R2 `(+ (- z) z))
  (define R1 `(+ 0 z))
  
  ;; Critical term for R5
  (reset-var-counter)
  (check-equal? (superimpose R4 R1)
                `((+ (- 0) (+ 0 v2))))

  
  ;; Critical term for R6
  (define test-subs (make-hash))
  (check-equal? (unify-helper R4 R2 test-subs) #f)
  
  (reset-var-counter)
  (check-equal? (superimpose R4 R2)
                `((+ (- (- v2)) (+ (- v2) v2))))

  ;(println (rewrite-possibilities `(+ (- (- v2)) (+ (- v2) v2)) (list R4 `y)))
  ;(println (rewrite-possibilities `(+ (- (- v2)) (+ (- v2) v2)) (list R2 `0)))

  (reset-var-counter)
  (check-equal? (last (superimpose R4 R4))
                `(+ (- (- v9)) (+ (- v9) (+ v9 v10))))

  (check-equal? (rewrite-possibilities `(+ (+ (+ 2 a) b) c) (list `(+ (+ x y) z) `(+ x (+ y z))))
                `((+ (+ 2 a) (+ b c)) (+ (+ 2 (+ a b)) c))) 

  (check-equal? (more-complex? `(- v414) `(+ v415 (- (+ v414 v415))))
                false)
  (check-equal? (more-complex?  `(+ v415 (- (+ v414 v415))) `(- v414))
                true)
  (check-equal? (orient-axiom `((- v414) (+ v415 (- (+ v414 v415)))))
                `((+ v415 (- (+ v414 v415))) (- v414)))
  

  )
