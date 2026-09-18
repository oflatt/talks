# The talk, and a PDF of it to fall back on if the show crashes live.
#
#   make               build talk.pdf: slideshow's own printer, one page per press
#   make show          run the talk
#   make show SKIP=20  run it from slide 21, without building the twenty before
#   make frames        render every epoch to frames/ as PNGs, for checking a change
#   make check         list every press's duration and flag presses that change nothing
#   make clean         drop the PDF and the compiled code
#
# HELPERS.md says how the slides are built.

RACKET ?= racket
SOURCES := $(wildcard *.rhm) $(wildcard media/*)

.PHONY: all show frames check clean

all: talk.pdf

# `-c` condenses each animation to the picture it lands on; `-e` prints at the
# screen's size rather than on paper. The page in the corner carries the same
# number the live show does, which is the one to find after a crash.
talk.pdf: $(SOURCES)
	raco slideshow --widescreen --pdf -c -e -o $@ talk.rhm

show:
	$(RACKET) talk.rhm $(if $(SKIP),--skip $(SKIP))

frames:
	$(RACKET) snapshot.rhm frames

check:
	$(RACKET) verify.rhm

clean:
	rm -f talk.pdf
	rm -rf compiled
