#lang slideshow/widescreen

(require "slidehelpers.rkt")
(require pict pict/color pict/conditional slideshow/play slideshow/text)

(current-main-font "Verdana")

(define (item-no-bullet . content)
  (apply item #:bullet (blank 0 0) content))


(define green (dark "green"))

(define (autoarrow-at-old iter current-iter time contents node1 node2 #:arrow-size [arrow-size 25] #:style [style #f] #:line-width [line-width (* 2 BORDERWIDTH)] #:label [label (blank)])
  (autoarrow-at iter current-iter time node1 node2 contents #:arrow-size arrow-size #:style style #:line-width line-width #:label label))

;; Don't use this fuction- instead use make-dynamic-slide from slidehelpers.rkt
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
  (define inputlist (list "f(a) = f(b)" "a = b" "b = c" "a = c"))
  (define inputs
    (lambda (bold-index)
      (vl-append 20 inputslabel
                 (apply vl-append
                        (for/list ([input inputlist] [i (in-range (length inputlist))])
                          (if (equal? i bold-index)
                              (text input (cons 'bold null) (current-font-size))
                              (text input null (current-font-size))))))))

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
    (pin-on
     85
     175
     inputsclean
     (make-bubble #:color (if useful (dark "green") "red") #:padding 2 (text (if useful "useful" "unnecessary") null (current-font-size)))
     (superimpose inputsclean eq5 spacing 0)))
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
                           (item-no-bullet "Are a and c equal?" (cellophane (blue (t "Yes!")) (min 1 (* 100 time))))
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
                           (item-no-bullet (t "Are a and c equal?") (blue (t "Yes!")))
                           (item-no-bullet (t "Are f(a) and a?") (red (t "No!")))
                           (item-no-bullet (t "Are f(a) and f(c)?") (cellophane (blue (t "Yes!")) (min 1 (* 100 time))))
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
                           (hbl-append (blank (* 2 (current-gap-size)) 0) (t " Proof size: ") (blue (t "1  ")) (scale robot-happy 0.1)))
                (list fa a c fc)
                )))]
    [no-longer
     (define message (make-bubble (bold (t "Key idea:"))
                                  (t "Try different paths")))
     (define invisible (blank 0 0))
     (make-slides-with-items #:start 5 #:title "Leveraging Additional Equalities"
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
                                              #:color "blue"
                                              #:line-width BORDERWIDTH)

                              (item-no-bullet (t "Prove f(a) and f(c) are equal:"))
                              (subitem-no-bullet (t "Prove f(a) = f(c) by congruence:"))
                              (subsubitem (t "a = c"))
                              (subitem-no-bullet (t "done!"))
                              (hbl-append (blank (* 2 (current-gap-size)) 0)(t " Proof size: ") (blue (t "1  ")) (scale robot-happy 0.1))
                              ))]
    [else
     (list (superimpose inputsclean combined spacing 0)
           (superimpose (inputs 0) eq1 spacing 0)
           (superimpose (inputs 1) eq2 spacing 0)
           (superimpose (inputs 2) eq3 spacing 0)
           (superimpose (inputs -1) eq5 spacing 0)
           (superimpose (inputs 3) eq5 spacing 0)
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
                (filled-ellipse 20 20 #:color (light "blue") #:border-color "blue" #:border-width BORDERWIDTH))))



(slide
 (tt "Small Proofs from E-Graphs")
 (t "Oliver Flatt, Samuel Coward, Max Willsey,")
 (t " Zachary Tatlock, Pavel Panchekha")
 (blank 0 100)
 (ht-append 800 (scale (bitmap "uwlogo.png") 0.35) (scale (bitmap "uulogo.png") 0.1))
 )

(define bubw 600)
(define bubh 250)

(define smallbw 300)
(define smallbh 150)

(make-dynamic-slide
 #:iters 9
 #:title "Egg"
 (lambda (iter time)
   (define prob (make-bubble
                 #:width smallbw #:height smallbh
                 (vc-append (current-gap-size)
                            (ht "Program")
                            (t "a + b - b"))))
   (define rules
     (make-bubble
      #:width smallbw #:height smallbh
      (vc-append (current-gap-size)
                 (ht "Rewrites")
                 (t "∀ x, x - x = 0"))))
   (define solver (make-bubble
                   #:width smallbw #:height smallbh
                   (vc-append (current-gap-size) (ht "egg")
                              (scale (bitmap "egglogo.png") 1))))
   (define solution (make-bubble #:width smallbw #:height smallbh
                                 (vc-append (current-gap-size)
                                            (ht "Optimized")
                                            (t "a")
                                            )))
   (define proof (make-bubble
                  #:width smallbw #:height smallbh
                  (vc-append
                   15
                   (ht "Proof")
                   (vl-append 5
                              (t "because b - b = 0"))
                   )))
   (define checker (make-bubble #:width smallbw #:height smallbh (ht "Proof Checker")))
   (define content
     (superimpose-on
      175 120 checker
      (fade-iter 8 iter time (make-flash (t "Simple") #:color "green"))
      (superimpose-on
       175 120 solver
       (fade-iter 4 iter time (make-flash (t "Complex") #:color "red"))
       (vl-append
        (* 2 (current-gap-size))
        (hc-append
         (* 4 (current-gap-size))
         (ht-append
          (* 4 (current-gap-size))
          (hc-append (* 4 (current-gap-size))

                     (vl-append
                      (* 2 (current-gap-size))
                      (fade-iter 1 iter time prob)
                      (fade-iter 2 iter time rules))

                     solver))
         (vl-append (* 2 (current-gap-size))
                    (fade-iter 3 iter time solution)
                    (fade-iter 5 iter time proof)))

        (hc-append
         (blank (+ (* 8 (current-gap-size))
                   (* 2 smallbw)) 0) (hc-append (current-gap-size) (fade-iter 6 iter time checker)
                                                (fade-iter 7 iter time (scale (bitmap "check.png") 0.4))
                                                ))))))

   (autoarrow-at
    2 iter time
    rules solver
    (autoarrow-at-old
     6 iter time
     (autoarrow-at-old
      5 iter time
      (autoarrow-at-old
       3 iter time
       (autoarrow-at-old 1 iter time content prob solver)
       solver solution)
      solver proof)
     proof checker)


    )))

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

      (fade-iter 1 iter time
                 (make-bubble #:width bubw #:height bubh
                              (vc-append (current-gap-size) (ht "Checking")
                                         (combine-at 9 iter time #:combine cc-superimpose
                                                     (fade-iter 2 iter time (t "Can we trust the solver?"))
                                                     (make-bubble (t "Okay") #:width (- bubw 50) #:color "orange"
                                                                  )))))

      (fade-iter 3 iter time
                 (make-bubble #:width bubw #:height bubh
                              (vc-append (current-gap-size) (ht "Debugging")
                                         (combine-at 10 iter time #:combine cc-superimpose
                                                     (fade-iter 4 iter time (t "How did we prove 0 = 1?"))
                                                     (make-bubble (t "Confusing") #:width (- bubw 50) #:color "red"))
                                         ))))


     (happend-gap-size
      (fade-iter 5 iter time
                 (make-bubble #:width bubw #:height bubh
                              (vc-append (current-gap-size) (ht "Backtracking")
                                         (combine-at 11 iter time #:combine cc-superimpose
                                                     (fade-iter 6 iter time (t "What facts led to this result?"))
                                                     (make-bubble (t "Too Specific")
                                                                  #:width (- bubw 50) #:color "red"))

                                         )))
      (fade-iter 7 iter time
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
     (make-bubble
      #:color (dark "green") #:width (+ (* bubw 2) (* 4 (current-gap-size))) #:height (+ (* 2 (current-gap-size)) (* bubh 2))
      (vc-append (current-gap-size)
                 (bold (tt "Our contribution:"))
                 (hbl-append (tt "Finding ") (bold (tt "smaller")) (tt " proofs from e-graphs")))))

    )))


#;(make-dynamic-slide #:iters 4
                      (lambda (iter time)
                        (vl-append (* (current-gap-size) 2)
                                   (fade-iter 1 iter time (make-bubble (tt "Proofs are important")))
                                   (fade-iter 2 iter time
                                              (make-bubble #:color "red" (tt "Proofs can be long")))
                                   (fade-iter
                                    3 iter time
                                    (make-bubble #:color green (tt "This talk: finding smaller proofs")
                                                 (hbl-append
                                                  (ht "Specifically: from ")
                                                  (bold (ht "congruence closure"))))))))



(make-dynamic-slide
 #:iters 5
 #:title "Finding Smaller Proofs"
 (lambda (iter time)
   (define gap (* 8 (current-gap-size)))
   (match-define (list a b c d e f)
     (list (make-node "a")
           (make-node "b")
           (make-node "c")
           (make-node "d")
           (make-node "e")
           (make-node "f")))
   (define np-hard (make-bubble #:color "red" (t "Minimal Proof is NP-hard!")))
   (define subproof (colorize (t "subproof") "red"))
   (autoarrow-at #:color "red"
                 3 iter time
                 np-hard subproof
                 (autoarrow-at
                  #:arrow-size 0 #:style 'long-dash #:line-width BORDERWIDTH #:color "blue"
                  #:label (fade-iter 2 iter time subproof)
                  1 iter time
                  b e
                  (autoarrow
                   #:arrow-size 0 #:style 'long-dash #:line-width BORDERWIDTH
                   a c
                   (autoarrow
                    #:arrow-size 0 #:style 'long-dash #:line-width BORDERWIDTH
                    b c
                    (autoarrow
                     #:arrow-size 0 #:style 'long-dash #:line-width BORDERWIDTH
                     #:label (fade-iter 2 iter time (colorize (t "subproof") "red"))
                     #:x-adjust-label 100
                     #:y-adjust-label 25
                     c d
                     (autoarrow
                      #:arrow-size 0 #:style 'long-dash #:line-width BORDERWIDTH
                      c e

                      (autoarrow
                       #:arrow-size 0 #:style 'long-dash #:line-width BORDERWIDTH
                       e f
                       (vc-append
                        (current-gap-size)
                        (hc-append
                         gap
                         (vc-append
                          gap
                          a
                          b)
                         c
                         (vc-append
                          gap
                          d
                          e)
                         (vc-append
                          gap
                          (blank (pict-width f) (pict-height f))
                          f))


                        (fade-iter 3 iter time np-hard)
                        (fade-iter 4 iter time (make-bubble (hc-append small-robot-idea (t " instead, greedily estimate subproof sizes"))))
                        ))))))))))


(make-dynamic-slide #:title "Results" #:iters 1
                    (lambda (iter time)
                      (scale (bitmap "dag-size-cdf.png") 0.7)))

(make-dynamic-slide
 #:title "Intel Case Study" #:iters 3
 (lambda (iter time)
   (vc-append (current-gap-size)
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

#;(slide #:title "Team and Acknowledgments"
       (parameterize ([current-font-size 30])
         (ht-append 50
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
                    )))
;;, Max Willsey, Zachary Tatlock, Pavel Panchekha



