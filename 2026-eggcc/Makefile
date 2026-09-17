# The talk, and a PDF of it to fall back on if the show crashes live.
#
#   make            build talk.pdf (one page per press, the picture each press lands on)
#   make show       run the talk
#   make frames     render every epoch to frames/ as PNGs, for checking a change
#   make clean      drop the PDF and the compiled code

RACKET ?= racket
SOURCES := $(wildcard *.rhm) $(wildcard media/*)

.PHONY: all show frames clean

all: talk.pdf

talk.pdf: $(SOURCES)
	$(RACKET) -l racket/base -e '(require (submod (file "talk.rhm") pdf))'

show:
	$(RACKET) talk.rhm

frames:
	$(RACKET) snapshot.rhm frames

clean:
	rm -f talk.pdf
	rm -rf compiled
