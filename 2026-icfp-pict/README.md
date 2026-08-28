# Animated Pictures for Slide Presentations — ICFP 2026 talk

Slides for "Animated Pictures for Slide Presentations" (Oliver Flatt, Robert
Bruce Findler, Matthew Flatt), written in Rhombus using `rhombus-slideshow`.

Copied from the paper repo `anipict-paper` at commit `4125678` ("last minute
changes! moved a slide back, change to green fish") on 2026-08-28.

## Running

    cd talk
    racket talk.rhm

`talk.rhm` accepts `--skip N` (handled by `argstrip.rhm` before slideshow's own
argument parser) to render only the slides after the first N — useful for
jumping into the middle while rehearsing.

To regenerate the static backup PDF (`talk/talk-backup.pdf`, already included):

    cd talk
    sh make-backup-pdf.sh

The talk alt-tabs to a live GUI demo near the end (cued by
`whattouse.what_next_slide()`). The demo files are copied here too:

  - `demo0.rhm`, `demo1.rhm` — the two working demo programs; `racket demo1.rhm`
  - `demo.rhm` — a later scratch version, copied verbatim. **It does not parse**
    ("demo.rhm:32:6: missing comma before new group") — it was an uncommitted
    work-in-progress in the paper repo at copy time, and is preserved as-is
    rather than guessed at. Use `demo1.rhm` unless you mean to finish this one.

## Layout

The talk lives in `talk/` and reaches out to `../config.rhm` and `../stepper/`,
so that two-level structure has to be preserved:

    config.rhm          font setup shared with the paper (requires the fonts below)
    stepper/            evaluation-stepper library used by the explanation slides
    talk/               talk.rhm plus its supporting modules, logos/, backup PDF

## Fonts

`config.rhm` errors out at load time if these are not installed:

  - Linux Libertine
  - Inconsolata (the fixed-width version, not the variable-width one)

The talk also uses Comic Sans MS for the deliberately-bad-slide joke.

## Versions used to give this talk

### Racket

    version: 9.3.0.2-2026-08-13-4b9d0d3f63 (cs)
    repo:    https://github.com/racket/racket
    commit:  4b9d0d3f63ca82ca29bd28f4bbf4326f404d510c
    date:    2026-08-11 08:28:24 -0600
    subject: upgrade native-lib build

### Rhombus

Installed as a clone of the `rhombus` repo (this single checkout supplies
`rhombus-lib`, `rhombus-pict`, `rhombus-slideshow`, `rhombus-gui`, `shrubbery`,
and the linked `rhombus-logo-lib` that `talk.rhm` imports as `rhombus/logo`).

    repo:    https://github.com/racket/rhombus
    commit:  4ab181ca4546be24183899e754f1d7ed3eed1014
    date:    2026-08-12 11:38:42 -0600
    subject: update distibution for existing catalog and improved pkg makefile

### Other relevant packages (from the Racket catalog)

    pict / pict-lib   f96ef6a7c26d
    slideshow         235519e704e0

Both the Racket and Rhombus checkouts were clean (no uncommitted changes) when
this talk was given.
