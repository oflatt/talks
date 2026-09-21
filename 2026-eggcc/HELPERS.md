# Building a talk with these helpers

The deck is a Rhombus program: every slide is a `pict` that animates, shown by
glide-pptx's `show_slides`. This is the vocabulary the slides are written in, and
the few facts about the show that the vocabulary rests on. Files: `theme.rhm` (the
page, faces, prose, sections), `slidehelpers.rhm` (bubbles, arrows, outline),
`write.rhm` + `ink.rhm` (how things arrive), `magicmove.rhm` (presses over keyed
scenes), `egraph.rhm` (nodes, classes, edges).

## 1. Presses, pages and the show

A pict's **epochs are the presses**. Epoch 0 is what the slide opens on; each later
epoch plays on a press and its end is where the slide rests. `pc.switch(a, b, c)`
concatenates epochs; `pc.switch(..., ~join: #'splice)` merges the epochs it joins
into one press (extents add) -- how four small moves become one gesture.

Things the show (glide `staged.rhm`) does for you, so do not code around them:

- **A still followed by an animation that starts on it is one page.** Slideshow
  would page both; glide splices the still away. Open a slide on `Frame()` or a
  bare canvas and write everything on presses -- the common pattern.
- **A slide whose last epoch animates is held** for one more press, so its last
  picture is seen at rest. Do not add your own `sustain`.
- **Hand-overs are cuts.** If a slide comes to rest on the picture the next slide
  opens with, glide drops the hold, cuts to the next slide, and the closing
  animation is the press that moves on. The dividers and the workflow → big-space
  handoff work this way; nothing is declared. "The same picture" is a pixel
  compare (`looks_same` in glide's `staged.rhm`, colour bytes of every eighth
  pixel, a budget of 24), so the next slide's first frame must draw the shared
  ink *once* -- a copy laid over it, or a mark, is a difference and costs the cut.
- **Pans** otherwise: `set_transitions(#'down)` in `talk.rhm`; a slide asks for its
  own with `p.epoch_set_metadata(0, p.epoch_metadata(0) ++ { #'transition: #'right })`
  (`#'left`, `#'up`, `#'down`, `#'none`).
- `racket talk.rhm --skip 20` starts at slide 21 without building the twenty
  before; `make`, `make show SKIP=20`, `make frames`, `make check` (below).

## 2. A slide

```rhombus
fun slide_thing():
  def page = titled("Case Study: eggcc", ~tag: "Eval setup title")   // white page + title
  def bubble = put(blank_slide, make_bubble(prose([["Questions?", #'bold]])), 1280.0, 130.0)
  fade_in(page, bubble, ~horiz: #'left, ~vert: #'top)                // one press: it writes
```

- Coordinates are the slide's, 1920 × 1080. `put(base, p, x, y)` places a pict by
  its top-left; `blank_slide` is a transparent layer that size; `page_canvas()` a
  white one; `title_text(s)`, `title_x`, `title_y` are the title on its own and
  where `titled` puts it.
- `fade_in(base, extra)` adds one press that brings `extra` on the way it wants to
  arrive (section 3). `staged(canvas, [layer, ...])` chains those, a press a layer.
  Several things in one press, one after another and overlapping -- a grid of logos
  -- is one layer with all of them on it: `write` staggers a layer's parts in the
  order they were put down (slide_2 in `opening.rhm`).
- Anything from glide's runtime (`slide_canvas`, `image_pict`, `textbox`) is a Racket
  pict; `as_pict` crosses it over. `pc.animate(~extent: secs, fun (t): ...)` is a
  hand-rolled press; keep `~bend: fun (x): x` when you time things inside it.
- A camera move is such a press: draw the picture scaled by `s` about a point, a
  scene point `q` landing at `(q - c) * s + c'`, and put anything that travels through
  the same transform (`slide_40`'s way into the e-graph in `workflow.rhm`). Keep the
  frame's box the slide's -- `put(...).refocus(page)` after each placement, `.clip()`
  at the end -- since a thing placed part way off the slide grows the box.

## 3. How things arrive

Every piece of text or shape carries an *arrival*, and `write(p)` plays a pict's
parts in tree order, each its own way, overlapping. Three arrivals:

| arrival | for | how |
|---|---|---|
| `#'write` | sentences in bubbles, equations, e-node labels, rules | traced letter by letter, shells outlined then filled |
| `#'pop` | big short labels (captions over a picture, stage names, code lines) | grows in from below with a small overshoot |
| `#'fade` | titles, the box round a block of code | fades where it stands |

- `prose_text(s, ~size, ~bold, ~italic, ~color, ~arrive)`, `code_text(...)`,
  `math_text(...)`: all deck text goes through these. `prose([["bold", #'bold], [" plain", #'plain]])`
  and `prose_stack(rows)` set a bubble's sentence. `frame_box(w, h, ~arrive: #'fade)`.
- Any pict can be tagged: `popped(p)`, `faded(p)`, `already(p)` (a copy laid over
  ink that is already there -- it is simply present at the end of the press, not
  redrawn), `remains(p)` (what a moving copy leaves behind -- present from the
  press's first instant; launder it if it is built from the picts that are moving,
  or the edges' `Find` sees them twice). Shapes: `written_shape(path, ~width, ~height, ~fill, ~line, ...)`,
  `written_stroke(path, ~pen)`; `rounded_path(w, h, r)`. Outlines are traced at a
  hand's pace -- quick along straights, slow round corners -- letters at a constant one.
- Pace lives in `write.rhm`: `write_pace` (0.5 s per unit of weight), `unwrite_pace`
  (an unwrite takes half a write), `write_span` (past it a picture's parts start
  closer together instead of taking longer, so nothing writes for more than ~2 s).
  A press that writes takes what its writing asks for; `extent_for(writer(p).total)`
  is that number if you need it for your own `animate`.
- `unwrite(p)`, `rewrite_between(a, b)` (unwrite then write when the words differ),
  `writer(p).frame(n)` for driving a write by hand -- as the update slide swaps two
  bubbles under a `magic_move`.

## 4. Bubbles and arrows

- `make_bubble(~color, ~padding, ~width, ~height, child, ...)`: pale fill,
  saturated border; written, the shell is traced and filled and then the words
  write. `make_arrow(...)` the same as a fat arrow; `make_gear(~size, ~teeth)`.
- `callout(~on, ~at: Find, ~spike: #'n/#'s/#'e/#'w, content)` pins a bubble to a
  point. `multi_callout(~on, ~at: [targets], ~place: marker, content)` reaches
  several targets from one body, each spike leaving the side that faces its target.
  `bubble_at(node, ~spot: [x, y], ~body: [bx, by], ~color, ~content)` is the
  slide-sized form for magic frames: the body's corner goes at `~body`, the spike
  finds the node at `~spot`.
- `arrow(~on, from, to, ~color, ~line_width, ~gap, ~shift, ~line_style, ~label, ~from_at, ~to_at)`
  joins two picts already placed on `~on`, border to border along the line of
  centres, aiming at rounded corners when the node carries `#'corner` metadata
  (`graph_node` does). `arrows(~on, [[a, b], ...])`. `arrow_morph(~on, ~t, ...)`
  draws one arrow becoming another -- a rule's pointer becoming the node's edge.
  An edge spec in a magic frame draws itself; an arrow that is a pict of its own
  (an annotation beside the picture) is `drawn_arrow(~on, from, to, ...)`, which
  `write` then draws from the tail with the head at the nib.
- Colours: `talk_blue`, `talk_light_blue`, `talk_green`, `talk_red`, `pale(c)`;
  the state parts of a graph are `keynote_blue`.

## 5. Presses over a scene: magic frames

For a picture of boxes and arrows, describe each press's *scene* and let
`magic_slide` work out the motion:

```rhombus
Frame(~picts: { key: [pict, x, y] or [pict, x, y, band] }, ~edges: [[kind, from_key, to_key], ...])
magic_slide(page, [Frame(), frame_1, frame_2, ...], ~wire: my_wire, ~edge_band: band_wires,
            ~extent: 1.0, ~extent_of: fun (i): if i in quick | 0.4 | 1.0)
```

- A key in both frames is *the same thing*: it moves from the one place to the
  other. Drawn differently in the two, it cross-fades in place (a node turning
  green) -- unless its words changed, when it unwrites and rewrites, or it is a
  bubble of the same kind, when the shell stays and the words are reworded. A key
  only the new frame has arrives its own way (section 3); a key only the old one
  has fades. `looks_same` stops a scene rebuilt per press from redrawing what did
  not change, but build a pict once and reuse it where you can.
- `band` orders drawing: `band_below` (class regions) < `band_nodes` (0) <
  `band_wires` < `band_point` < `band_chip` < `band_callout` < `band_flight`
  (from `theme.rhm`). Two things in one band have no order between them.
- Edges are specs, not picts. `~wire: fun (layer, specs, at): ...` is called every
  frame with ghosts of every pict where it is now and the picts by key, and draws
  the specs it is for -- `chain_wire(layer, at, [[from, to]], ~color, ~line_width)`
  is the usual body. Shared edges are solid; an edge only one frame has draws
  itself (new) or fades (gone); `[#'morph, gone, new, t]` is a pointer becoming an
  edge (`~pairs`). `~under_wire`/`~under_band` is a second edge layer under the
  picture; `~stage: fun (blank, a, b, t)` a layer that changes shape as the press runs.
- `chain_press(page, a, b)` is one press on its own, for a slide that opens some
  other way and carries on with frames; splice presses with `pc.switch(..., ~join: #'splice)`.

## 6. E-graphs

- `graph_node(~width, ~height, ~adj, ~line, ~line_width, ~fill, ~label: node_label("v", ~sub: "1"))`;
  fills `node_white`, `node_green` (picked), `node_fork` (picked twice), `node_match`
  (gold, just matched); `stateful_hex` for the blue border of a stateful node.
- `eclass_region(from_mid, to_mid, ~dash: value_class | state_class)` returns
  `[pict, x, y]` for a frame key: a grey tube with a dashed equality down it.
  `eclass_tube` is the tube itself, `eq_line` just the dashed line (what a rule's
  `→` turns into). `node_middle(node, x, y)`.
- `walk_arrow()` is the pointer an extraction walk moves down a graph;
  `gold_chip(p)` / `chip_at(...)` mark a rule variable as bound.

## 7. Sections

`sections` in `theme.rhm` names them. `divider(k, ~shots: section_shots, ~next: title)`
is the outline slide: the list writes itself the first time and flies down out of
the header after that; its second press puts the list away into the header **and
writes the next slide's title** (pass that slide's exported title constant). Wrap
each content slide in `in_section(k, slide)` for the header bar. `shots.rhm` cuts a
keepsake out of one slide per section with `crop_pict`.

## 8. Charts and data

Draw with `pc.dc(fun (dc, dx, dy): ...)` and a knob per series (`perf_chart(~ours: t)`),
then `pc.animate(~extent: 1.2, fun (t): scene(t))` per press. Numbers come from
`plotdata.rhm`, which reads `profile.json` verbatim -- a copy of eggcc's
`eggcc-submission-combined.json`; re-running the benchmarks is a file copy.

## 9. Packaging

These modules are the talk's own copy and stay that way, so the talk never breaks
under a later version of them. Once the talk is given, the general parts --
`write.rhm`, `ink.rhm`, `magicmove.rhm`, the bubbles, arrows and outline of
`slidehelpers.rhm` -- are to be lifted into a Rhombus package for the next talk;
`theme.rhm`, `egraph.rhm` and the slide files are this deck's.

## 10. Checking a change

- `make frames` writes every epoch at rest and mid-press to `frames/`; diff two
  runs to see exactly which pictures a change touched.
- `make check` (`verify.rhm`) lists each slide's press durations and reports any
  press whose end looks like its start.
- Build one slide in a script: `import: file("talk.rhm") open` gives `all_slides`
  and every `slide_*`; slideshow owns the command line, so pass choices through the
  environment, not arguments.
