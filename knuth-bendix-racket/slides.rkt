#lang slideshow

(require slideshow/repl)
(require slideshow/text)
(require pict/color)
(define NAMESPACE (make-base-namespace))
(define (namespace) NAMESPACE)


(current-para-width client-w)

(define (knuth-repl content [repl-h (* client-h 1/2)])
  (repl-area #:width (* client-w 1) #:height repl-h #:font-size 14
             #:make-namespace namespace
             content))

(define-syntax-rule (make-title title)
  (parameterize ([current-font-size (exact-floor (* 1.5 (current-font-size)))])
    (t title)))

(define-syntax-rule (make-title-with text)
  (parameterize ([current-font-size (exact-floor (* 1.5 (current-font-size)))])
    text))

(define (slides-list title items)
  (slides-list-show title (reverse items)))

(define (slides-list-show title items)
  (cond
    [(empty? items)
     (void)]
    [else
     (slides-list-show title (rest items))
     (apply slide (reverse items)
      #:layout 'top
      #:title title)]))
   

(slide
 (make-title "An Introduction to Knuth-Bendix Completion")
 (t "AJJ Dick"))


(slide
 #:layout 'top
 #:title (make-title "Goal")
 (para "Input: ")
 (subitem (bt "axioms") "- a set of " (colorize (t "orientable") (dark "green")) " equations")
 (subitem (bt "ordering relation") "- a " (colorize (t "comparator") (dark "green")) " for complexity of expressions")
 (para (t "Output: "))
 (subitem (ht-append (bt "rewrite rules") (t "- a ") (blue (t "confluent")) (t " set of rules"))))

(define (two-to-one-tree text1 text2 text3)
  (define a-var
    (t text1))
  (define b-var
    (t text2))
  (define c-var
    (t text3))
  (define rewrite-combined
    (vc-append
     (ht-append a-var (blank 20 1) b-var)
     (blank 1 40)
     c-var))
  (define rewrite-arrow-1
    (pin-arrow-line 10 rewrite-combined
                    a-var cb-find
                    c-var lt-find
                    #:line-width 3))
  (pin-arrow-line 10 rewrite-arrow-1
                  b-var cb-find
                  c-var rt-find
                  #:line-width 3))

(define (one-to-two-tree text1 text2 text3)
  (define a-var
    (t text1))
  (define b-var
    (t text2))
  (define c-var
    (t text3))
  (define rewrite-combined
    (vl-append
     (ht-append (blank (pict-width b-var) 1) a-var)
     (blank 1 80)
     (ht-append b-var (blank 40 1) c-var))) 
  (define rewrite-arrow-1
    (pin-arrow-line 10 rewrite-combined
                    a-var cb-find
                    b-var ct-find
                    #:line-width 3))
  (pin-arrow-line 10 rewrite-arrow-1
                  a-var cb-find
                  c-var ct-find
                  #:line-width 3))

(define rewrite (two-to-one-tree "e1" "e2" "e3"))

(slide
 #:layout 'top
 #:title (make-title "Goal")
 (para "Input: ")
 (subitem (bt "axioms") "- a set of " (colorize (t "orientable") (dark "green")) " equations")
 (subitem (bt "ordering relation") "- a " (colorize (t "comparator") (dark "green")) " for complexity of expressions")
 (para (t "Output: "))
 (subitem (ht-append (bt "rewrite rules") (t "- a ") (blue (t "confluent")) (t " set of rules")))
 (blank-line)
 (ht-append rewrite (blank (pict-width rewrite) 1)
            (parameterize ([current-para-width (exact-floor (* client-w 0.75))])
              (para (blue (t "Confluent-")) " any proof based upon these rules has a simple rewrite proof."))))


(slide
 #:layout 'top
 #:title (make-title "Goal")
 (para (ht-append (t "Input: ") (bt "axioms") (t ", ") (bt "ordering relation")))
 (para (ht-append (t "Output: ") (bt "rewrite rules")))
 (knuth-repl "(require \"knuth-bendix.rkt\")"))

(current-para-width (exact-floor (* client-w 0.75)))
(parameterize ([current-gap-size (* (current-gap-size) 2)])
  (slides-list
   (make-title "Superimpose, Critical pairs, and New rules")
   (list
    (para "For each rule, " (bt "superimpose") " with all other rules")
    (para "Add " (bt "critical pairs") " to axioms")
    (para "Use these axioms to form " (bt "new rules") " and repeat"))))

(current-para-width client-w)

(slide
 #:layout 'top
 #:title (make-title-with  (ht-append (bt "Superimpose") (t ", Critical pairs, and New rules")))
 (para "For each rule, " (bt "superimpose") " with all other rules")
 (knuth-repl "(unify `(+ y y) `(+ (- 2) b))" (* client-h 1/8))
 (knuth-repl "(unify `(+ (+ x y) z) `(+ (- a) a))" (* client-h 1/8))
 (knuth-repl "(superimpose `(+ (+ x y) z) `(+ (- a) a))"))


(slide
 #:layout 'top
 #:title (make-title-with  (ht-append (t "Superimpose, ") (bt "Critical pairs") (t ", and New rules")))
 (para (bt "Critical pairs") "- formed by using two rules to reduce their superposition")
 (one-to-two-tree "(+ (+ (- a) a) c)" "(+ 0 c)" "(+ (- a) (+ a c))")
 (knuth-repl "(rewrite-one `(+ (+ (- a) a) c) `((+ (- x) x) 0))" (* client-h 1/8))
 (knuth-repl "(rewrite-one `(+ (+ (- a) a) c) `((+ (+ x y) z) (+ x (+ y z))))" (* client-h 1/8)))


(slide
 #:layout 'top
 #:title (make-title-with  (ht-append (t "Superimpose, Critical pairs, and ") (bt "New rules")))
 (para "The new critical pair sometimes results in a new rewrite rule.")
 (one-to-two-tree "(+ (+ (- a) a) c)" "(+ 0 c)" "(+ (- a) (+ a c))")
 (hc-append (t "(+ (- a) (+ a c))") (blank 30 1) (arrow 30 0) (blank 30 0) (t "(+ 0 c)"))
 (knuth-repl "(orient-axiom (list `(+ 0 c) `(+ (- a) (+ a c))))" (* client-h 4/8)))

(slide
 #:layout 'top
 #:title (make-title "When does Knuth-Bendix fail?")
 (item "It may never terminate with some rules- (no example)")
 (item "It may fail if axioms do not always reduce complexity")
 (item "Also, use " (bt "selection strategy") " or it may never converge")
 (knuth-repl "(orient-axiom (list `(+ a b) `(+ b a)))" (* client-h 1/8)))
