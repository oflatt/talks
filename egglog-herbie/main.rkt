#lang slideshow/widescreen

(require "../../slide-helpers/main.rkt")
(require pict pict/conditional slideshow/play slideshow/text)
(require (except-in racket/gui))
(require slideshow/repl racket/runtime-path)
(require racket/random)

(current-main-font "Verdana")


(set-spotlight-style!	#:size 10 #:color "yellow")

(slide
  (tt "egglog In Practice")
  (t "Oliver Flatt, Yihong Zhang")
  (blank 0 30)
  (t "With thanks to the rest of the egglog team!")
  (t "Max Willsey, Eli Rosenthal, Philip Zucker, Ryan Tjoa, Zhiyuan Yan, and Zachary Tatlock")
  (blank 0 30)
  (t "Special shout-out to Saul Shanabrook for the visualiziations shown today")
  (blank 0 30)
  (t "And thanks to Anjali Pal for the slide feedback"))

(dynamic-slide
  "Why egglog?"
  (header lang lang-ex analysis analysis-ex  multi multi-ex incr incr-ex)
  (define lib (fade-iter lang (t "library")))
  (define analysis-text (fade-iter analysis (colorize (t "e-class analysis") black)))
  (define multipatterns (fade-iter multi (colorize (t "slow multi-patterns") black)))
  (define lang-text (fade-iter lang (t "language")))
  (define comp-text (fade-iter analysis (colorize (t "composable analysis") black)))
  (define multi-text (fade-iter multi (colorize (t "fast database joins") black)))
  (fade-iter header 

  (superimpose-center 0 0
    (fade-iter incr-ex
      (make-bubble #:color "black" #:inner-color "white"
        (scale-to-width 600 (cached-bitmap "incremental-graph.png"))))
  (ht-append 20
    (stack 0 60
      (t "egg")
      (hline 500 10)
      lib
      (show-at-iter lang-ex (scale-to-width 500 (cached-bitmap "egg-simple.png"))
                    #:alt analysis-text)
      (show-at-iter analysis-ex (scale-to-height 400 (cached-bitmap "egg-analysis.png")) #:alt multipatterns)
      (show-at-iter multi-ex (scale-to-width 500 (cached-bitmap "egg-multi.png"))
        #:alt (fade-iter incr (t "slow e-matching")))
      )
    (vline 5 400)
    (stack 0 60
      (t "egglog")
      (hline 500 10)
      lang-text
      (show-at-iter lang-ex (scale-to-width 500 (cached-bitmap "egglog-simple.png")) #:alt comp-text)
      (show-at-iter analysis-ex (scale-to-width 500 (cached-bitmap "egglog-analysis.png")) #:alt multi-text)
      (show-at-iter multi-ex
        (hc-append (blank 50 0) 
            (scale-to-height 200 (cached-bitmap "egglog-multi.png")))
        #:alt (fade-iter incr (t "incremental e-matching")))
      )))))

(define code-sexps
  (read-sexps (open-input-file "herbie-tutorial.egg")))


(define code-strings
  (break-up-code (port->string (open-input-file "herbie-tutorial.egg"))))

(define-runtime-path temp "temp")

(define repl-w
  (* client-w 6/10))
(define repl-h
  (* client-h 4/5))
(define graph-w
  (- client-w repl-w))
(define graph-h
  (/ client-h 2.0))
(define datatypes-w graph-w)
(define datatypes-h graph-h)

#;(make-repls-for-code code-strings
    #:repl-fun
    (lambda (content #:pre-content pre-content
                     #:eval-callback eval-callback)
            (make-egglog-repl
              content
              repl-w
              repl-h
              #:pre-content pre-content
              #:eval-callback eval-callback))
    #:callback
    (lambda (repl namespace callback-box)
      (define half-blank
        (blank graph-w graph-h))
      (slide
        #:layout 'top
        (ht-append 0
          repl
          (interactive half-blank
            (lambda (win)
              (define (get-graph)
                (define id (number->string (random 4294967087)))
                (define path
                  (path->string (build-path temp (format "~a.png" id))))
                (parameterize
                  ([current-namespace namespace]
                   [current-output-port (open-output-nowhere)])
                  (eval `(#%top-interaction visualize ,path)))
                (when (not (file-exists? path))
                  (error "Failed to save SVG file"))
                path)
              
              (define canvas
                (new canvas%
                  [parent win]
                  [paint-callback
                   (lambda (canvas dc)
                     (define-values (w h) (send canvas get-size))
                     (define orig-bitmap (bitmap (get-graph)))
                     (define res  (size-in-pixels (my-scale-to-fit w h orig-bitmap)))
                     (send dc clear)
                     (when (> (pict-width orig-bitmap) 100)
                      (send dc draw-bitmap (pict->bitmap res)
                           1 1)))]))
              (set-box! callback-box
                        (lambda ()
                          (send canvas on-paint)))
              (lambda ()
                void
                )))))))


;;(define repl2 (egglog-repl
#;(dynamic-slide
  "Constants"
  ()
  repl2)
