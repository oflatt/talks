# The talk, and a PDF of it to fall back on if the show crashes live.
#
#   make               build talk.pdf: slideshow's own printer, one page per press
#   make show          run the talk
#   make show SKIP=20  run it from slide 21, without building the twenty before
#   make frames        render every epoch to frames/ as PNGs, for checking a change
#   make check         list every press's duration and flag presses that change nothing
#   make check-deps    confirm rhombus-glide is at the commit this deck is written against
#   make deps          on a fresh machine, fetch rhombus-glide at that commit and link it in
#   make clean         drop the PDF and the compiled code
#
# HELPERS.md says how the slides are built.

RACKET ?= racket
SOURCES := $(wildcard *.rhm) $(wildcard media/*)

# The commit of rhombus-glide this deck is written against. `glide-pptx` is
# installed as a link to a checkout of that repository, so whatever the checkout is
# at is what runs the talk: `make check-deps` says whether it is at this commit, and
# building or showing the talk warns if it is not. On a machine without the
# checkout, `make deps` clones it at this commit and links it in.
GLIDE_REPO   ?= https://github.com/oflatt-claude/rhombus-glide
GLIDE_SRC    ?= $(HOME)/rhombus-glide
GLIDE_COMMIT := 8534a00184f6ad447faa252406df9548baf2fa14

.PHONY: all show frames check clean deps check-deps deps-warn

all: deps-warn talk.pdf

# `-c` condenses each animation to the picture it lands on; `-e` prints at the
# screen's size rather than on paper. The page in the corner carries the same
# number the live show does, which is the one to find after a crash.
talk.pdf: $(SOURCES)
	raco slideshow --widescreen --pdf -c -e -o $@ talk.rhm

show: deps-warn
	$(RACKET) talk.rhm $(if $(SKIP),--skip $(SKIP))

frames:
	$(RACKET) snapshot.rhm frames

check:
	$(RACKET) verify.rhm

clean:
	rm -f talk.pdf
	rm -rf compiled

# the rhombus-glide checkout, at the pinned commit, linked in as `glide-pptx`
deps:
	@[ -d $(GLIDE_SRC)/.git ] || git clone $(GLIDE_REPO) $(GLIDE_SRC)
	git -C $(GLIDE_SRC) cat-file -e $(GLIDE_COMMIT) 2>/dev/null || git -C $(GLIDE_SRC) fetch --quiet --all
	git -C $(GLIDE_SRC) checkout --quiet $(GLIDE_COMMIT)
	raco pkg show glide-pptx | grep -q glide-pptx \
	  || raco pkg install --auto --link $(GLIDE_SRC)/glide-pptx

# is the checkout that runs the talk at the pinned commit?
check-deps:
	@have=$$(git -C $(GLIDE_SRC) rev-parse HEAD 2>/dev/null) ; \
	if [ "$$have" = "$(GLIDE_COMMIT)" ] ; then \
	  echo "rhombus-glide is at the pinned commit $(GLIDE_COMMIT)" ; \
	else \
	  echo "rhombus-glide at $(GLIDE_SRC) is at $${have:-no checkout}, not the pinned $(GLIDE_COMMIT)" ; \
	  exit 1 ; \
	fi

# the same, as a warning that does not stop a build
deps-warn:
	@have=$$(git -C $(GLIDE_SRC) rev-parse HEAD 2>/dev/null) ; \
	[ "$$have" = "$(GLIDE_COMMIT)" ] \
	  || echo "WARNING: rhombus-glide checkout is not at the pinned commit; see \`make check-deps\`"
