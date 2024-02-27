#lang slideshow/widescreen

(require "slidehelpers.rkt")
(require pict pict/conditional slideshow/play slideshow/text pict/code)

(current-main-font "Verdana")

(define (item-no-bullet . content)
  (apply item #:bullet (blank 0 0) content))

(set-spotlight-style!	#:size 10 #:color "yellow")

(define (autoarrow-at-old iter current-iter time contents node1 node2 #:arrow-size [arrow-size 25] #:style [style #f] #:line-width [line-width (* 2 BORDERWIDTH)] #:label [label (blank)])
  (autoarrow-at iter current-iter time node1 node2 contents #:arrow-size arrow-size #:style style #:line-width line-width #:label label))


;; Don't use this function- instead use make-dynamic-slide from slidehelpers.rkt
;; some of the slides in this file still use this crappy thing
(define (make-slides-with-items to-add #:layout [layout 'top] #:title [title #f] #:start [start -1] #:end [end #f] #:animation-superimpose [animation-superimpose lt-superimpose])
  (when (equal? start -1)
    (slide #:layout layout #:title title))
  (define end-bound (if end (+ 1 end) (length to-add)))
  (for ([i (in-range  (max 0 start) end-bound)])
    (define current (list-ref to-add i))
    (define with-empties
      (for/list ([item to-add] [j (in-range (length to-add))])
        (define item2 (if (list? item) (last item) item))
        (if (>= j i)
            (blank (pict-width item2) (pict-height item2))
            item2)))
    (define current-list
      (if (list? current)
          current
          (list current)))
    (define lasti (blank (pict-width (first current-list)) (pict-height (first current-list))))
    (for ([item current-list])
      (play-n #:title title #:skip-first? #t #:layout layout
              (lambda (t)
                (apply (curry vl-append (current-gap-size))
                       (list-set with-empties i (fade-pict #:combine animation-superimpose t lasti item)))))
      (set! lasti item))))


(define (mod1 a)
  (- a (floor a)))

(define FPS 60.0)
(define SLIDEDELAY (/ 1.0 FPS))

(define (interp point1 point2 time)
  (for/list ([x point1] [x2 point2])
    (+ x (* time (- x2 x)))))


(define (example-2 highlight-ac anim-bc highlight-fa-a [highlight-fa-fc #f] [anim-fa-fc #f] [anim-fa-fc-proof #f] #:new-edge [new-edge #f] #:ac-edge [ac-edge #f] #:show-input [show-input #t] #:fast-proof [fast-proof #f] #:no-longer [no-longer #f] #:useful [useful #f])
  (define inputslabel (if show-input (t "Inputs:") (cellophane (t "Inputs:") 0)))
  (define inputlist (list "f(c)" "f(a) = f(b)" "a = b" "b = c" "a = c"))
  (define inputs
    (lambda (bold-index)
      (vl-append 20 inputslabel
                 (apply (curry vl-append (/ (current-gap-size) 2))
                        (for/list ([input inputlist] [i (in-range (length inputlist))])
                          (indent
                           (if (equal? i bold-index)
                               (text input (cons 'bold null) (current-font-size))
                                (text input null (current-font-size)))))))))

  (define fa (make-node "f" (or highlight-fa-a highlight-fa-fc)))
  (define a (make-node "a" (or highlight-fa-a highlight-ac)))
  (define fb
    (make-node "f"))
  (define b (make-node "b" #f))
  (define fc
    (make-node "f" highlight-fa-fc))
  (define c (make-node "c" highlight-ac))

  (define faarrowed (vc-append (blank 0 100) (parent-child (vc-append 75 fa a) fa a)))

  (define fbarrowed (parent-child (vc-append 100 fb b) fb b))

  (define fcarrowed (vc-append (blank 0 100) (parent-child (vc-append 75 fc c) fc c)))

  (define combined (ht-append 220 faarrowed fbarrowed fcarrowed))

  (define eq1 (equality-horiz combined fa fb))
  (define eq2 (equality-horiz eq1 a b))
  (define eq3-before (equality-horiz eq2 b c))
  (define eq3 (if ac-edge (equality-horiz eq3-before a c) eq3-before))
  (define eq4 (parameterize ([current-font-size 30]) (equality-horiz eq3 fb fc "congr(b, c)" #:x-adjust-label 25 #:y-adjust-label -20)))
  (define eq5
    (parameterize ([current-font-size 30])
      (if new-edge
          (equality-horiz eq4 fa fc "congr(a, c)" #:x-adjust-label 85 #:y-adjust-label -5)
          eq4)))

  (define spacing (+ 150 (pict-width (inputs -1))))
  (define inputsclean (inputs -1))
  (define final
    (vl-append
    (pin-on
     115
     265
     inputsclean
     (make-bubble #:color (if useful green "red") #:padding 2 (text (if useful "useful" "unnecessary") null (current-font-size)))
     (superimpose inputsclean eq5 spacing 0))
    (blank 0 (current-gap-size))))
  (define steps 1000)
  (cond
    [anim-bc
     (play-n #:title "A Bigger E-Graph Example"
             #:steps steps #:skip-first? #t
             #:layout 'top
             #:delay SLIDEDELAY
             (lambda (time)
               (make-dot-animation-slide
                time
                (vl-append (current-gap-size)
                           final
                           (item-no-bullet "Are a and c equal?" (cellophane (colorize (t "Yes!") blue) (min 1 (* 100 time))))
                           (cellophane (make-bubble (hbl-append (bold (t "Key idea: ")) (t "equality edges form equivalence classes of terms"))) 0)
                           (cellophane (make-bubble (hbl-append (bold (t "Key idea: ")) (t "equality edges form equivalence classes of terms"))) 0))
                (list a b c))))]
    [anim-fa-fc
     (play-n #:title "A Bigger E-Graph Example"
             #:steps steps #:skip-first? #t
             #:layout 'top
             #:delay SLIDEDELAY
             (lambda (time)
               (make-dot-animation-slide
                time
                (vl-append (current-gap-size)
                           final
                           (item-no-bullet (t "Are a and c equal?") (colorize (t "Yes!") blue))
                           (item-no-bullet (t "Are f(a) and a?") (colorize (t "No!") red))
                           (item-no-bullet (t "Are f(a) and f(c)?") (cellophane (colorize (t "Yes!") blue) (min 1 (* 100 time))))
                           (cellophane (make-bubble (hbl-append (bold (t "Key idea: ")) (t "equality edges form equivalence classes of terms"))) 0))
                (list fa fb fc))))
     ]
    [anim-fa-fc-proof
     (play-n
      #:steps steps #:skip-first? #t
      #:layout 'top
      #:delay SLIDEDELAY
      (lambda (time)
        (make-dot-animation-slide
         time
         (vl-append (current-gap-size)
                    final
                    (item-no-bullet (t "Prove f(a) and f(c) are equal:"))
                    (subitem-no-bullet "f(a) = f(b)")
                    (subitem-no-bullet "Prove f(b) = f(c) by congruence:")
                    (ht-append (blank (* 2 (current-gap-size)) 0) (subitem-no-bullet "b = c"))
                    (subitem-no-bullet "done!"))
         (list fa fb b c fc))))
     ]
    [fast-proof
     (play-n #:title "Leveraging Additional Equalities"
             #:steps steps #:skip-first? #t
             #:layout 'top
             #:delay SLIDEDELAY
             (lambda (time)
               (make-dot-animation-slide
                time
                (vl-append (current-gap-size)
                           final
                           (item-no-bullet (t "Prove f(a) and f(c) are equal:"))
                           (subitem-no-bullet (t "Prove f(a) = f(c) by congruence:"))
                           (subsubitem (t "a = c"))
                           (subitem-no-bullet (t "done!"))
                           (hbl-append (blank (* 2 (current-gap-size)) 0) (t " Proof size: ") (colorize (t "1  ") blue) (scale robot-happy 0.1)))
                (list fa a c fc)
                )))]
    [no-longer
     (define keyidea
      (make-bubble (bold (t "Key idea:"))
                     (t "Try alternate path")))
     (define message
       (superimpose
        keyidea
        (make-flash (t "new!") #:color green)
        (- (pict-width keyidea) 75)
        (- (pict-height keyidea) 30))
       )
     (define invisible (blank 0 0))
     (make-slides-with-items
      #:start 5 #:title "Leveraging Additional Equalities"
      (list
       (pin-arrow-line 15
                       (pin-over
                        (pin-over final
                                  700
                                  300
                                  message)
                        700 130
                        invisible)
                       message ct-find
                       invisible ct-find
                       #:color blue
                       #:line-width BORDERWIDTH)

       (item-no-bullet (t "Prove f(a) and f(c) are equal:"))
       (subitem-no-bullet (t "Prove f(a) = f(c) by congruence:"))
       (subsubitem (t "a = c"))
       (subitem-no-bullet (t "done!"))
       (hbl-append (blank (* 2 (current-gap-size)) 0)(t " Proof size: ") (colorize (t "1  ") blue) (scale robot-happy 0.1))
       ))]
    [else
     (list (superimpose inputsclean combined spacing 0)
           (superimpose (inputs 1) eq1 spacing 0)
           (superimpose (inputs 2) eq2 spacing 0)
           (superimpose (inputs 3) eq3 spacing 0)
           (superimpose (inputs -1) eq5 spacing 0)
           (superimpose (inputs 4) eq5 spacing 0)
           final
           )]))

(define (make-dot-animation-slide time-in contents nodes)
  (define segments (- (length nodes) 1))

  (define steps 1000)
  (define step-value (/ 1.0 steps))
  (define period-duration (* segments 1.0));; seconds
  (define num-steps (/ period-duration SLIDEDELAY))
  (define period (* num-steps step-value))
  (define time (mod1 (/ time-in period)))

  (define node1-index (exact-floor (* time segments)))
  (define localtime (mod1 (* time segments)))

  (define node1 (list-ref nodes node1-index))
  (define node2 (list-ref nodes (+ node1-index 1)))
  (define-values (a b) (cc-find contents node1))
  (define-values (c d) (cc-find contents node2))
  (match-define (list find1 find2)
    (cond
      [(and (equal? c a) (> d b)) ;; underneath
       (list cb-find ct-find)]
      [(and (equal? c a) (< d b)) ;; above
       (list ct-find cb-find)]
      [(> c a) ;; right
       (list rc-find lc-find)]
      [else
       (list lc-find rc-find)])) ;; left
  (define-values (x1 y1) (find1 contents node1))
  (define-values (x2 y2) (find2 contents node2))
  (match-define (list xres yres) (interp (list x1 y1) (list x2 y2) localtime))
  (pin-over contents (- xres 10) (- yres 10)
            (if (equal? time-in 1.0)
                (blank)
                (filled-ellipse 20 20 #:color light-blue
                                #:border-color blue #:border-width BORDERWIDTH))))

(define uu-logo (bitmap "uulogo.png"))


(slide
 (tt "Small Proofs from E-Graphs")
 (t "Oliver Flatt, Samuel Coward, Max Willsey,")
 (t " Zachary Tatlock, Pavel Panchekha")
 (blank 0 100)
 (ht-append 800 (scale (bitmap "uwlogo.png") 0.35) (scale uu-logo 0.1))
 )

(define bubw 600)
(define bubh 250)

(define smallbw 300)
(define smallbh 150)

(make-dynamic-slide
 #:iters 8
 #:title "Solvers and Proofs"
 (lambda (iter time)
   (define prob (make-bubble #:width smallbw #:height smallbh
                             (vc-append (current-gap-size)
                                        (ht "Problem")
                                        (t "Is a+b = b+a?"))))
   (define solver (make-bubble #:width smallbw #:height smallbh
                               (vc-append (current-gap-size) (ht "Solver")
                                          (scale (bitmap "gear.png") 0.2))))
   (define solution (make-bubble #:width smallbw #:height smallbh
                                 (vc-append (current-gap-size)
                                            (ht "Solution")
                                            (colorize (t "YES") green)
                                            )))
   (define proof (make-bubble #:width smallbw #:height smallbh
                              (vc-append 15
                                         (ht "Proof")
                                         (vc-append 5
                                                    (t "a+b = b+a")
                                                    (t "by commutativity"))
                                         )))
   (define checker (make-bubble #:width smallbw #:height smallbh (ht "Proof Checker")))
   (define content
     (superimpose-on
      175 120 checker
      (fade-iter 6 iter time (make-flash (t "Simple") #:color "green"))
      (superimpose-on
       175 120 solver
       (fade-iter 3 iter time (make-flash (t "Complex") #:color "red"))
       (vl-append
        (* 2 (current-gap-size))
        (hc-append (* 4 (current-gap-size))
                   (ht-append
                    (* 4 (current-gap-size))
                    (hc-append (* 4 (current-gap-size))
                               (fade-iter 1 iter time prob) solver))
                   (vl-append
                    (* 2 (current-gap-size))
                    (fade-iter 2 iter time solution)
                    (fade-iter 4 iter time proof)))

        (hc-append
         (blank (+ (* 8 (current-gap-size))
                   (* 2 smallbw)) 0) (hc-append (current-gap-size) (fade-iter 5 iter time checker)
                                                (fade-iter 7 iter time (scale (bitmap "check.png") 0.4))
                                                ))))))

   (autoarrow-at-old 5 iter time
                     (autoarrow-at-old 4 iter time
                                       (autoarrow-at-old
                                        2 iter time
                                        (autoarrow-at-old 1 iter time content prob solver)
                                        solver solution)
                                       solver proof)
                     proof checker)


   ))


(make-dynamic-slide
 #:iters 14
 #:title (lambda (iter)
           (cond
             [(> iter 7)
              "Proofs can be Long"]
             [else
              "Proofs are Useful"]))
 (lambda (iter time)
   (cb-superimpose

    (append-gap-size
     (happend-gap-size

      (fade-iter
       1 iter time
       (make-bubble #:width bubw #:height bubh
                    (vc-append (current-gap-size) (ht "Checking")
                               (combine-at 9 iter time #:combine cc-superimpose
                                           (fade-iter 2 iter time (t "Can we trust the solver?"))
                                           (make-bubble (t "Okay") #:width (- bubw 50) #:color "orange"
                                                        )))))

      (fade-iter
       3 iter time
       (make-bubble #:width bubw #:height bubh
                    (vc-append (current-gap-size) (ht "Debugging")
                               (combine-at 10 iter time #:combine cc-superimpose
                                           (fade-iter 4 iter time (t "How did we prove 0 = 1?"))
                                           (make-bubble (t "Confusing") #:width (- bubw 50) #:color "red"))
                               ))))


     (happend-gap-size
      (fade-iter
       5 iter time
       (make-bubble #:width bubw #:height bubh
                    (vc-append (current-gap-size) (ht "CDCL")
                               (combine-at 11 iter time #:combine cc-superimpose
                                           (fade-iter 6 iter time (t "What facts led to this result?"))
                                           (make-bubble (t "Too Specific")
                                                        #:width (- bubw 50) #:color "red"))

                               )))
      (fade-iter
       7 iter time
       (make-bubble #:width bubw #:height bubh
                    (vc-append (current-gap-size) (ht "...And More")
                               (combine-at 12 iter time #:combine cc-superimpose
                                           (vc-append (current-gap-size)
                                                      (fade-iter 7 iter time (t "Fuzzing"))
                                                      (fade-iter 7 iter time (t "Optimization")))
                                           (make-bubble (t "Slow")
                                                        #:width (- bubw 50) #:color "red")
                                           )))))
     )


    (fade-iter
     13 iter time
     (make-bubble #:color green #:width (+ (* bubw 2) (* 4 (current-gap-size))) #:height (+ (* 2 (current-gap-size)) (* bubh 2))
                  (vc-append (current-gap-size)
                             (bold (tt "This Talk:"))
                             (hbl-append (tt "Finding ") (bold (tt "smaller")) (tt " proofs from congruence closure")))))

    )))

(define herbie-logo (bitmap "herbie.png"))
(define incarnet-logo (scale (bitmap "incarnate.png") 0.1))
(define ruler-logo (scale (bitmap "ruler.png") 0.2))
(define egglogo (bitmap "egglogo.png"))

(make-dynamic-slide
 #:iters 6
 (lambda (iter time)
   (vc-append
    (* 2 (current-gap-size))
    (tt "Why E-Graphs?")
    (vl-append (* 2 (current-gap-size))

               (fade-iter 1 iter time
                          (hbl-append
                           (bold (ht "Congruence Closure"))
                           (ht " forms the basis of many solvers")))

               (fade-iter 2 iter time
                          (hbl-append
                           (ht "Generates all proofs of ") (bold (ht "equality"))))

               (fade-iter
                3 iter time
                (hc-append
                 (* 4 (current-gap-size))
                 (vl-append (* 2 (current-gap-size))
                            (hbl-append (ht "Enables ")
                                        (bold (ht "equality saturation")))
                            (indent
                             (ht "Optimization and synthesis"))
                            (fade-iter 4 iter time
                                       (indent (hbl-append (ht "Our library: egg ") egglogo))))
                 (fade-iter
                  5 iter time
                  (hc-append
                   (* 4 (current-gap-size))
                   (vl-append
                    (current-gap-size)
                    herbie-logo

                    incarnet-logo
                    )
                   ruler-logo))

                 ))))))


;; Talk overview


(define outline-headings
  (list "Motivation"
        "Congruence Closure"
        "Proofs from Congruence Closure"
        "Finding Small Proofs"))
(define outline-funcs
  (list #f #f #f
        (lambda (iter time)
          (fade-iter 1 iter time
                     (subitem-no-bullet
                      (make-flash #:color green #:padding 100
                                  (ht "27.2% smaller proofs!"))))
          )))


(my-make-outline outline-headings outline-funcs 1 #:iters 2 #:start 0)

;; Section 1 Congruence Closure
;; Way to store an equivalence relation over terms that preserves the congruence invariant
;; E-Graphs

(make-dynamic-slide
 #:iters 4
 #:title "Congruence Closure"
 (lambda (iter time)
   (define input (make-bubble
                  (append-gap-size
                   (t "Input: equalities between terms")
                   (st "    a = b")
                   (st "    f(a) = f(b)")
                   (st "    b = c"))))
   (vc-append (* 2 (current-gap-size))
              (ht-append (* 2 (current-gap-size))
                         (fade-iter 1 iter time
                                    input)
                         (fade-iter 2 iter time
                                    (make-bubble #:height (pict-height input)
                                                 (append-gap-size
                                                  (t "Output: equivalence relation")
                                                  (hbl-append (st "stored in an ") (bold (st "e-graph")) (st " data structure"))
                                                  (st "Ask: is a = c?")))))

              (fade-iter 3 iter time
                         (make-bubble
                          (hbl-append (t "The relation is also closed under ") (bold (t "congruence")))
                          (t "∀ x, y: x = y ⇒ f(x) = f(y)")))
              )))


(define ex1-inputs
  (vl-append (current-gap-size)
             (t "Inputs:")
             (indent (t "f(a)"))
             (indent (t "f(b)"))
             (indent (t "a = b"))
             ))

(define fa (make-node "f"))
(define a (make-node "a"))
(define fb
  (make-node "f"))
(define b (make-node "b"))

(make-dynamic-slide
 #:iters 8
 #:title "E-Graph Example"
 (lambda (iter time)
   (match-define
      (list inputsiter faiter fbiter type1 equality congr congrbub)
      (range 1 8))
   (vc-append
    (current-gap-size)
    (hbl-append (t "A graph with ") (bold (t "3")) (t " kinds of edges"))
   (ht-append
    (* (current-gap-size) 6)
    (fade-iter
      inputsiter iter time
      (vl-append (blank 0 125) ex1-inputs))
    (fade-iter
     faiter iter time
     (autoarrow-at-old
      #:arrow-size 0 #:style 'long-dash #:line-width BORDERWIDTH #:label (st "congr(a, b)")
      congr iter time
      (autoarrow-at-old
       #:arrow-size 0 #:style 'long-dash #:line-width BORDERWIDTH
       equality iter time
       (vl-append
        (current-gap-size)
        (fade-iter congrbub iter time (make-bubble
                                (st "equality edge from congruence")))
        (hc-append (current-gap-size)
                   (autoarrow #:line-width BORDERWIDTH #:arrow-size 15
                              fa
                              a
                              (vc-append 150
                                         fa
                                         a))
                   (blank 150 0)
                   (fade-iter
                    fbiter iter time
                    (autoarrow #:line-width BORDERWIDTH #:arrow-size 15
                               fb b
                               (vc-append 150
                                          fb
                                          b)))
                   (fade-iter type1 iter time (make-bubble (st "parent-child edge"))))
        (hc-append (blank (pict-width fa) 0)
        (fade-iter equality iter time (make-bubble (st "equality edge"))))
        )
       a b

       )


      fa fb)))
   )))

;; Introduce our example:
;; Process one equality at a time

;; We want to support 3 operations:
;; Assert equality between two grounded terms
;; Ask for equality between two grounded terms
;; Ask for a proof of this equality


(make-slides-with-items #:title "A Bigger E-Graph Example" #:end 0
                        (list (example-2 #f #f #f)
                              (item-no-bullet (t "Are a and c equal?"))
                              (item-no-bullet (t "How can we") (bold (t "debug")) "this?")
                              (item-no-bullet (t "How can we") (bold (t "verify")) "that two terms are equal?")
                              (make-bubble (hbl-append (bold (t "Key idea: ")) (t "equality edges form equivalence classes of terms")))))

(make-slides-with-items #:title "A Bigger E-Graph Example" #:start 3
                        (list (last (example-2 #f #f #f))
                              (item-no-bullet (t "Are a and c equal?") (colorize (t "Yes!") blue))
                              (item-no-bullet (t "Are f(a) and a?") (colorize (t "No!") red))
                              (item-no-bullet (t "Are f(a) and f(c)?") (colorize (t "Yes!") blue))
                              (make-bubble (hbl-append (bold (t "Key idea: ")) (t "equality edges form equivalence classes of terms")))))

(make-slides-with-items #:start 0
                        (list (ht-append (t "Example: ") (last (example-2 #f #f #f #:show-input #f)))
                              (ht-append (t "Reality: ") (scale (bitmap "crazyegraph.png") 0.2))))


(make-slides-with-items #:start 1
                        (list (ht-append (t "Example: ") (last (example-2 #f #f #f #:show-input #f)))
                              (pin-over (ht-append (t "Reality: ") (scale (bitmap "crazyegraph.png") 0.2)) 700 100 (scale (bitmap "confusedeggtransparent.png") 0.5))))


(my-make-outline outline-headings outline-funcs 2 #:iters 3 #:start 2)

(make-slides-with-items #:title "Congruence Proofs"
                        (list (item-no-bullet "Answer the question \"how are these two terms equal?\"")
                              (last (example-2 #t #f #f))
                              (item-no-bullet (t "Prove a and c are equal:"))
                              (subitem-no-bullet "a = b")
                              (subitem-no-bullet "b = c")
                              (subitem-no-bullet "done!")
                              ))


(make-slides-with-items #:start 1
                        (list (last (example-2 #f #f #f #t))
                              (item-no-bullet (t "Prove f(a) and f(c) are equal:"))
                              (subitem-no-bullet "f(a) = f(b)")
                              (subitem-no-bullet "Prove f(b) = f(c) by congruence:")
                              (subsubitem "b = c")
                              (subitem-no-bullet "done!")))

(example-2 #f #f #f #t #f #t)


(define fafb (t "f(a) = f(b)"))

(define bc (t "b = c"))
(define too-big (make-bubble #:color "red"
                             (hbl-append (t "This proof: size 2 ") small-robot-sad)))

#;(define-values (prove0 prove1) (lt-find ...))

(define 2proof (append-gap-size
                (t "Prove f(a) and f(c) are equal:")
                (indent fafb)
                (indent (t "Prove f(b) = f(c) by congruence:"))
                (indent2 bc)
                (indent (t "done!"))))

(make-dynamic-slide
 #:iters 4
 (lambda (iter time)
   (match-define
     (list psize thisproof better)
     (range 1 4))


   (autoarrow-at
    thisproof iter time too-big fafb
    (autoarrow-at
     thisproof iter time too-big bc
     (append-gap-size
      (vc-append
       (* 2 (current-gap-size))
       #;(fade-iter 1 iter time
                   (ht-append (make-bubble (ht "Is this a good proof?"))
                              (blank 600 0))
                   )
        #;(fade-iter 2 iter time (make-bubble
                                (hbl-append (t "It is valid ") small-robot-happy)))
        (fade-iter
         psize iter time
         (make-bubble
          (vc-append (current-gap-size)
                     (para #:align 'center
                           (t "We define")
                           (bold (t "proof size"))
                           (t "as")
                           (t "the number of")
                           (bold (t "unique"))
                           (t "equalities in the proof")))))
        (hbl-append
         (* 4 (current-gap-size))
         (append-gap-size
          (fade-iter thisproof iter time too-big)

          (fade-iter better iter time
                     (make-bubble #:color green (t "We can do better!"))))
         2proof)
        )
       )))))


(make-dynamic-slide #:iters 9 #:title "Leveraging Additional Equalities"
                    (lambda (iter time)
                      (append-gap-size
                       (swap-at 3 iter time
                                (swap-at 2 iter time
                                         (swap-at 1 iter time
                                                  (last (example-2 #f #f #f #t #:new-edge #f))
                                                  (last (example-2 #f #f #f #t #:useful #t)))
                                         (last (example-2 #f #f #f #t #:useful #t #:ac-edge #t)))
                                (last (example-2 #f #f #f #t #:useful #t #:ac-edge #t #:new-edge #t)))
                       (fade-iter 4 iter time (item-no-bullet (t "Prove f(a) and f(c) are equal:")))
                       (fade-iter 5 iter time (subitem-no-bullet (t "Prove f(a) = f(c) by congruence:")))
                       (fade-iter 6 iter time (subsubitem (t "a = c")))
                       (fade-iter 7 iter time (subitem-no-bullet (t "done!")))
                       (fade-iter 8 iter time
                                  (hbl-append (blank (* 2 (current-gap-size)) 0)  (t " Proof size: ") (colorize (t "1  ") blue) (scale robot-happy 0.1)))
                       )))


(example-2 #f #f #f #:fast-proof #t #:new-edge #t #:ac-edge #t #:useful #t)


(example-2 #f #f #f #:new-edge #t #:no-longer #t #:ac-edge #t #:useful #t)



(define eggbit
  (scale (bitmap "confusedeggtransparent.png") 0.5))
(make-dynamic-slide
 #:iters 10 #:title "The Crux of The Problem"
 (lambda (iter time)
   (define gap (* 8 (current-gap-size)))
   (define (eq i node1 node2 content)
     (autoarrow-at #:arrow-size 0 #:style 'long-dash #:line-width BORDERWIDTH i iter time
                   node1 node2 content))
   (define (parent i node1 node2 content)
     (autoarrow-at #:arrow-size 15 #:line-width BORDERWIDTH
                   #:finder1 bc-find #:finder2 tc-find
                   i iter time
                   node1 node2 content))
   (match-define (list a b c d e f g h)
     (list (make-node "a") (make-node "b")
           (make-node "c") (make-node "d")
           (make-node "e") (make-node "f")
           (make-node "f") (make-node "f")))
   (match-define (list pa pb pc)
     (list (t "Proof re-use is free")
           (t "Subproofs can be complex")
           (t "Exponential paths in graph")))
   (define NP (colorize (ht "NP-Complete Problem (Fellner Et al.)") "red"))
   (define content
     (vl-append (current-gap-size)
                (hc-append 0
                           (vl-append (* 2 (current-gap-size))
                                      (t "Prove a = e")
                                      (fade-iter 3 iter time (t "Bottom: size 4"))
                                      (swap-at
                                       5 iter time
                                       (fade-iter 4 iter time (t "Top: ???"))
                                       (t "Top: size 4")))
                           (blank (current-gap-size) 0)
                           a
                           (blank gap 0)
                           (vc-append gap
                                      f b)
                           (blank gap 0)
                           (vc-append gap
                                      g c)
                           (blank gap 0)
                           (vc-append gap
                                      h d)
                           (blank gap 0)
                           e)
                (superimpose-on
                 50 0 NP
                 (fade-iter 10 iter time eggbit)
                 (fade-iter 4 iter time
                            (hc-append gap
                                       (vl-append (current-gap-size)
                                                  (fade-iter 6 iter time pa)
                                                  (fade-iter 7 iter time pb)
                                                  (fade-iter 8 iter time pc))
                                       (fade-iter 9 iter time NP))))
                ))

   (autoarrow-at
    #:finder2 tl-find 8 iter time
    pa NP
    (autoarrow-at
     8 iter time
     pb NP
     (autoarrow-at
      #:finder2 bl-find 8 iter time
      pc NP
      (autoarrow-at
       #:arrow-size 0 #:style 'long-dash #:line-width BORDERWIDTH
       #:label (st "congr(c, d)") 2 iter time
       f g
       (autoarrow-at
        #:arrow-size 0 #:style 'long-dash #:line-width BORDERWIDTH
        #:label (st "congr(d, c)") 2 iter time
        g h
        (eq
         2 h e
         (eq
          2 a f
          (eq
           1 a b
           (eq
            1 b c
            (eq
             1 c d
             (eq
              1 d e
              (parent 0 f c
                      (parent 0 g d
                              (parent 0 h b
                                      content))))))))))))))))




(my-make-outline outline-headings outline-funcs 3 #:iters 3 #:start 2)

(define proof (make-bubble
               (vl-append (/ (current-gap-size) 2)
                          (t "Proof of f(a) = f(c):")
                          (st "f(a) = f(b)")
                          (st "a = b")
                          (st "a = c"))))

(define proof1 (make-bubble
                (vl-append (/ (current-gap-size) 2)
                           (t "Omit One Equality")
                           (cross-out (st "f(a) = f(b)"))
                           (st "a = b")
                           (st "a = c"))))

(define congr (make-bubble #:height (pict-height proof1)
                           (vc-append (current-gap-size)
                                      (t "Congruence Closure")
                                      (st "Complexity: O(n log(n))"))))

(define proofreduced  (make-bubble
                       (vl-append (/ (current-gap-size) 2)
                                  (t "Proof of f(a) = f(c):")
                                  (st "a = c"))))

(define proof2 (make-bubble
                (vl-append (/ (current-gap-size) 2)
                           (t "Omit One Equality")
                           (cross-out (st "a = c")))))

(define dotdotdot (tt "..."))

#;(make-dynamic-slide
 #:title "Brute Force Algorithm (Z3 style)" #:iters 10
 (lambda (iter time)
   (pin-on
    300 -50
    proof
    (fade-iter 7 iter time
               (make-bubble #:color "red"
                            (hc-append (st "Depends on initial proof ") small-robot-sad)))
    (pin-on
     300 -50
     proof2
     (fade-iter 8 iter time
                (make-bubble #:color "red"
                             (hc-append (st "Depends on order ") small-robot-sad)))
     (autoarrow-at
      5 iter time proofreduced proof2
      (autoarrow-at
       6 iter time proof2 dotdotdot
       (autoarrow-at
        2 iter time proof proof1
        (autoarrow-at
         3 iter time proof1 congr
         (autoarrow-at
          4 iter time #:start-angle (- (/ (* 3 pi) 4)) #:end-angle (* pi -3/4) #:finder1 cb-find
          #:finder2 ct-find
          congr proofreduced
          (vc-append
           (current-gap-size)
           (vl-append
            (* (current-gap-size) 2)
            (hc-append (* 4 (current-gap-size))
                       (fade-iter 1 iter time proof)
                       (fade-iter 2 iter time proof1)
                       (fade-iter 3 iter time congr))

            (hc-append (* 4 (current-gap-size))
                       (fade-iter 4 iter time proofreduced)
                       (fade-iter 5 iter time proof2)
                       (fade-iter 6 iter time dotdotdot)))
           (fade-iter 9 iter time
                      (make-bubble #:color "red"
                                   (hc-append (st "Complexity O(n^2 log(n)) where n is proof size ") small-robot-sad))
                      )))))))))))

(make-dynamic-slide
 #:title "Idea: Shortest Path?" #:iters 3
 (lambda (iter time)
   (vc-append (current-gap-size)
              (superimpose
               (last (example-2 #f #f #f #:new-edge #t #:useful #t #:ac-edge #t))
               (fade-iter 1 iter time
                          (make-bubble (hc-append small-robot-idea (t " shortest path?"))))
               700 200)
              (fade-iter 2 iter time
                         (make-bubble #:color "red"
                                      (t "Problem: how big are congruence edges?")
                                      )))))


(define (ex3-inputs iter time)
  (vl-append (current-gap-size)
             (t "Inputs:")
             (swap-temporary 1 iter time (t "a = b") (bold (t "a = b")))
             (swap-temporary 3 iter time (t "g(b) = c") (bold (t "g(b) = c")))
             ))

(define 1bubble (make-bubble #:color green #:padding 2
                             (t "1")))
(define 2bubble (make-bubble #:color green #:padding 2
                             (t "2")))
(define bubblepos
  (- (* 4 (current-gap-size)) (/ 2 (pict-width 1bubble))))
(define bubble2pos
  (- (* 8 (current-gap-size)) (/ 2 (pict-width 1bubble))))
(define fgaouter (make-node "f"))
(define fcouter (make-node "f"))
(define ga (make-node "g"))
(define gb (make-node "g"))
(define c (make-node "c"))

(define (example-3 iter time add-inputs)
  (match-define
    (list eq1 congr1 eq2 congr2 keyidea bubbles1 bubblecongr bubbles2  bubcongr2 bubbles3 highlight-end)
    (range 1 12))
  (define fga (swap-temporary highlight-end iter time fgaouter (make-node "f" #t)))
  (define fc (swap-temporary highlight-end iter time fcouter (make-node "f" #t)))

  (superimpose-on
   #:finder tr-find bubblepos -60  a
   (fade-iter bubbles1 iter time
              1bubble)
   (superimpose-on
    #:finder tr-find bubblepos -60  gb
    (fade-iter bubbles1 iter time
               1bubble)
    (superimpose-on
     #:finder tr-find bubblepos -60 ga
     (fade-iter
      bubbles2 iter time
      1bubble)
     (superimpose-on
      #:finder tr-find bubble2pos -60 fga
      (fade-iter bubbles3 iter time
                 2bubble)
      (autoarrow
       fga ga #:line-width BORDERWIDTH #:arrow-size 15
       (autoarrow
        ga a #:line-width BORDERWIDTH #:arrow-size 15
        (autoarrow
         gb b #:line-width BORDERWIDTH #:arrow-size 15
         (autoarrow
          fc c #:line-width BORDERWIDTH #:arrow-size 15

          (autoarrow-at
           #:arrow-size 0 #:style 'long-dash #:line-width BORDERWIDTH
           eq1 iter time a b
           (autoarrow-at
            #:arrow-size 0 #:style 'long-dash #:line-width BORDERWIDTH #:label (swap-temporary bubblecongr iter time (t "congr(a, b)") (colorize (t "congr(a, b)") blue))
            congr1 iter time ga gb
            (autoarrow-at
             #:arrow-size 0 #:style 'long-dash #:line-width BORDERWIDTH
             eq2 iter time gb c
             (autoarrow-at
              #:arrow-size 0 #:style 'long-dash #:line-width BORDERWIDTH
              #:label (swap-temporary bubcongr2 iter time (t "congr(g(a), c)") (colorize (t "congr(g(a), c)") blue))
              #:color (if (equal? iter highlight-end) blue #f)
              congr2 iter time fga fc

              (ht-append
               (if add-inputs (ex3-inputs iter time) (blank))
               (if add-inputs (blank (* (current-gap-size) 6) 0) (blank))
               (vl-append
                (current-gap-size)
                (vl-append
                 (* 3 (current-gap-size))
                 (hc-append (+ (pict-width gb) (* 16 (current-gap-size)))
                            fga fc)
                 (hc-append (* 8 (current-gap-size)) ga gb c)
                 (hc-append (* 8 (current-gap-size)) a b)
                 )
                (fade-iter keyidea iter time
                           (if add-inputs
                               (make-bubble
                                (hbl-append (bold (ht "Key idea: ")) (ht "compute estimates bottom-up")))
                               (blank)))
                )))))))))))))))


(make-dynamic-slide
 #:title "Proof Size Estimation" #:iters 12
 (lambda (iter time)
   (example-3 iter time #t)))



(define putting-together
  (lambda (iter time)
    (match-define
      (list step1 step2 step3 email)
      (range 1 5))
    (vc-append
     (current-gap-size)
     (hc-append
      (current-gap-size)

      (vl-append
       (* 3 (current-gap-size))
       (fade-iter step1 iter time
                  (ht "1. Compute size estimates"))
       (fade-iter step2 iter time
                  (ht "2. Find shortest path"))
       (fade-iter step3 iter time
                  (ht "3. Output extracted proof"))
       ;; proof turned off
       #;(indent
          (fade-iter prove1 iter time
                     (st "f(g(a)) = f(g(c)) by congruence:"))
          (indent
           (vl-append
            (current-gap-size)
            (fade-iter prove2 iter time
                       (st "g(a) = g(b) by congruence:"))
            (fade-iter prove3 iter time
                       (indent (st "a = b")))
            (fade-iter prove4 iter time
                       (st "g(b) = g(c)"))
            ))))
      (example-3 20 0 #f))

     (fade-iter email iter time
                (t "oflatt@cs.washington.edu"))

     )))

(make-dynamic-slide
 #:title "Putting it All Together" #:iters 4
 putting-together)

#;(play-n #:title "Putting it All Together"
        #:steps 1000 #:skip-first? #f
        #:layout 'top
        #:delay SLIDEDELAY
        (lambda (time)
          (make-dot-animation-slide
           time
           (putting-together 10 0)
           (list fgaouter ga a b gb c fcouter))))


(make-dynamic-slide
 #:title "Results" #:iters 3
 (lambda (iter time)
   (match-define (list arrowi real)
     (range 1 3))
     (superimpose
   (swap-at
    real iter time
    
     (scale (bitmap "dag-size-blank.png") 0.7)
     
     
    (scale (bitmap "dag-size-cdf.png") 0.7))
    
    
    (fade-iter
      arrowi iter time
      (colorize
       (hbl-append (/ (current-gap-size) 2)
        (arrow 40 (* pi 3/4))
        (ht "better"))
       blue))
     125 50)  
       ))

(make-dynamic-slide
 #:title "Intel Case Study" #:iters 3
 (lambda (iter time)
   (vc-append
    (current-gap-size)
    (make-bubble
     (vc-append (current-gap-size)
                (hbl-append (t "Multi-operation circuit optimization and translation validation with egg ")
                            (bitmap "egglogo.png"))
                (fade-iter 1 iter time (item-no-bullet "4.7 hours -> 2.3 hours"))))
    (fade-iter
     2 iter time
     (hc-append
      (t "2 inputs")
      (scale (bitmap "egg-optimized.png") 0.6)
      (t "5 outputs"))))))

(slide
 #:title "Team and Acknowledgments"
 (parameterize ([current-font-size 30])
   (vc-append (current-gap-size)
              (ht-append
               50
               (vc-append (clip-to-fit (inset (bitmap "oliver.png") -800 0 0 0) 150 225)
                          (t "Oliver Flatt"))
               (vc-append (clip-to-fit (bitmap "sam.jpeg") 150 225)
                          (t "Samuel Coward"))
               (vc-append (clip-to-fit (inset (bitmap "max.jpeg") 0 -70 0 0) 150 225)
                          (t "Max Willsey"))
               (vc-append (clip-to-fit (bitmap "zach.jpeg") 150 225)
                          (t "Zachary Tatlock"))
               (vc-append (clip-to-fit (bitmap "pavel.jpeg") 150 225)
                          (t "Pavel Panchekha"))
               )
              (vl-append (current-gap-size)
                         (ht "Special thanks to:")
                         (indent (indent (ht "Theo Drane (Intel)")))
                         (indent (indent (ht "George A. Constantinides (Imperial College)")))
                         (indent (indent (ht "Leonardo de Moura (Microsoft)")))))))
;;, Max Willsey, Zachary Tatlock, Pavel Panchekha

(make-dynamic-slide
  #:title "What about egglog?" #:iters 11
  (lambda (iter time)
    (match-define
      (list probs one onea second seconda third thirda firstsol secondsol thirdsol)
      (range 1 11))
    (vl-append
      (current-gap-size)
      (fade-iter probs iter time (t "Three new problems:"))
      (fade-iter one iter time (item-no-bullet "1. Multi-patterns"))
      (fade-iter onea iter time (subitem-no-bullet
          (hc-append (current-gap-size)
            (scale (codeblock-pict
              "(rule ((= a (Neg a)))
      ((union a 0)))") 2)
            (fade-iter firstsol iter time
              (make-bubble
              (t "Multiple Justifications"))))))
      (fade-iter second iter time (item-no-bullet "2. Provenance"))
      (fade-iter seconda iter time (subitem-no-bullet
          (hc-append (current-gap-size)
            (scale (codeblock-pict
            "
(rule ((= lhs (Abs a))
       (> (lower-bound a) 0))
      ((union lhs a)))") 2)
             (fade-iter secondsol iter time
              (make-bubble (t "Justify Terms"))))))
      (fade-iter third iter time (item-no-bullet "3. Enginnering challenges"))
      (fade-iter thirda iter time (subitem-no-bullet
        (hc-append (current-gap-size)
         (vl-append
          (t "Egglog -> Desugaring -> CoreEgglog ->")
          (t "Query Plan -> Generic Join -> Database"))
         (fade-iter thirdsol iter time
           (make-bubble (t "Hard") #:color red)))))
          )))


(make-dynamic-slide
 #:title "Questions?" #:start 4 #:iters 5
 putting-together)

