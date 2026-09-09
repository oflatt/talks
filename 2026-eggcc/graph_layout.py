#!/usr/bin/env python3
"""Where the e-graph nodes go on the extraction slides.

The graph on those slides changed three times while the talk was being written,
and every time the arrows had to be re-aimed by hand. So the placement is solved
instead, against the constraints a reader of the picture actually cares about:

  1. data flows down    -- a node sits strictly below every node it is built
                           from, and the members of one e-class share a row,
                           since a class is one thing.
  2. few crossings      -- the order within each row is chosen to minimise them,
                           counting the long edges as they pass each row.
  3. nothing behind a box -- an edge may not pass through a node it does not end at.
  4. steep edges        -- a long shallow run reads as a horizontal line rather
                           than as data flowing down, so it is penalised; so is a
                           pencil of edges arriving at one node from nearly the
                           same angle.
  5. classes read as one -- nothing sits between the members of a class, so the
                           region drawn around them contains only them.
  6. arguments in order -- a node's children are drawn left to right in the order
                           the program passes them, since `update(a, x, w)` is read
                           off the picture by that order.

Two modes:

    python3 graph_layout.py            # check the table in talk.rhm and report
    python3 graph_layout.py --solve    # search, and print a table to paste in

The table in talk.rhm is the one the deck draws: solve, paste, then nudge by eye
and run the check again -- a nudge that buys a nicer picture is fine, a nudge
that puts an edge through a box is not, and the check is what tells them apart.

Everything it needs is read out of talk.rhm: the graph itself (`chain_graph`),
how wide each box is (`chain_box`), and the placement (`chain_rows`, `chain_pos`).
"""

import math
import random
import re
import sys
from itertools import combinations, permutations, product

TALK = "talk.rhm"
LEFT, RIGHT = 1090.0, 1812.0      # the graph's own column, right of the code
GAP = 44.0                        # least space between two boxes in a row: two
                                  # boxes closer than this leave their arrowheads
                                  # crowding each other


# ---------------------------------------------------------------------------
# reading the deck

def block(src, name):
    """the text of `def <name>:` -- the rest of that line and the lines indented
    under it -- with the comments taken out"""
    m = re.search(r"^def %s(?::|\s*=)([^\n]*\n(?:[ \t].*\n|\n)*)"
                  % re.escape(name), src, re.M)
    if not m:
        raise SystemExit("graph_layout: no `def %s:` in %s" % (name, TALK))
    return re.sub(r"//[^\n]*", "", m.group(1))


def read_talk(path=TALK, placement=True):
    src = open(path).read()
    g = block(src, "chain_graph")
    nodes, cls, args = [], {}, {}
    for key, c, arglist in re.findall(r"\[#'(\w+),\s*#'(\w+),\s*\[([^\]]*)\]\]", g):
        nodes.append(key)
        cls[key] = c
        args[key] = re.findall(r"#'(\w+)", arglist)
    box = {k: float(v) for k, v in
           re.findall(r"#'(\w+):\s*([\d.]+)", block(src, "chain_box"))}
    states = re.findall(r"#'(\w+)", block(src, "chain_states"))
    # the order the deck reveals the nodes in, which is also how it decides which
    # member of a class an edge is aimed at: the one the program itself built
    reveals = [re.findall(r"#'(\w+)", line)
               for line in block(src, "chain_reveals").split("\n") if "#'" in line]
    if not placement:                     # --solve does not need one yet
        return Graph(nodes, cls, args, box, reveals, states), [], {}
    rows = [float(v) for v in re.findall(r"[\d.]+", block(src, "chain_rows"))]
    pos = {k: (float(x), int(r)) for k, x, r in
           re.findall(r"#'(\w+):\s*\[([\d.]+),\s*(\d+)\]", block(src, "chain_pos"))}
    return Graph(nodes, cls, args, box, reveals, states), rows, pos


class Graph:
    def __init__(self, nodes, cls, args, box, reveals=(), states=()):
        self.nodes, self.cls, self.args, self.box = nodes, cls, args, box
        self.members = {}
        for k in nodes:
            self.members.setdefault(cls[k], []).append(k)
        # the edges the picture draws: one per argument class, aimed at the member
        # the program itself built, which is the first one the deck reveals
        order = [k for group in reveals for k in group]
        rank = {k: (order.index(k) if k in order else len(order)) for k in nodes}
        first = {c: min(ms, key=lambda k: rank[k]) for c, ms in self.members.items()}
        self.edges = [(k, first[a]) for k in nodes for a in args[k]]
        # what each node's children are, left to right as the program writes them:
        # `update(a, x, w)` is read off the picture by that order, so the picture
        # has to keep it or it says something the program does not.
        #
        # By *class*, not by node: which member of a class an edge is aimed at
        # changes as an extraction picks one, and the order has to hold for every
        # answer the deck draws, so a whole class goes to the left of the next one.
        self.kids = {k: [self.members[a] for a in args[k]] for k in nodes}
        # a memory passes along a blue edge and a value along a black one, and two
        # edges of different colour crossing is far less confusing than two of one
        self.states = list(states)
        self.mates = [ms for ms in self.members.values() if len(ms) > 1]

    def kind(self, e):
        return 's' if (e[0] in self.states or e[1] in self.states) else 'v'

    def w(self, k):
        return self.box[k]

    def h(self, k):
        return 64.0 if self.box[k] > 100 else 63.486

    def prepare(self, lay=None):
        """the rows, and the one-row steps a long edge is broken into"""
        self.lay = lay if lay else self.layers()
        self.dummy, self.steps = dummies(self.lay, self.edges)
        self.all_lay = dict(self.lay)
        self.all_lay.update(self.dummy)
        return self.lay

    def layers(self):
        """longest path over classes: constraint 1"""
        lay = {c: 0 for c in self.members}
        for _ in range(len(lay) + 2):
            for k in self.nodes:
                for a in self.args[k]:
                    if lay[self.cls[k]] <= lay[a]:
                        lay[self.cls[k]] = lay[a] + 1
        return {k: lay[self.cls[k]] for k in self.nodes}


# ---------------------------------------------------------------------------
# geometry: boxes, and the straight border-to-border edges the deck draws

class Geo:
    def __init__(self, g, x, lay, rows):
        self.g, self.x, self.lay, self.rows = g, x, lay, rows

    def rect(self, k):
        y = self.rows[self.lay[k]]
        return (self.x[k], y, self.x[k] + self.g.w(k), y + self.g.h(k))

    def mid(self, k):
        r = self.rect(k)
        return ((r[0] + r[2]) / 2, (r[1] + r[3]) / 2)

    def seg(self, e):
        a, b = e
        ca, cb = self.mid(a), self.mid(b)

        def border(k, tx, ty):
            cx, cy = self.mid(k)
            dx, dy = tx - cx, ty - cy
            hw, hh = self.g.w(k) / 2, self.g.h(k) / 2
            t = min(hw / abs(dx) if dx else 1e18, hh / abs(dy) if dy else 1e18)
            return (cx + dx * t, cy + dy * t)
        return border(a, *cb), border(b, *ca)


def hits_box(p, q, r, pad=4.0):
    x0, y0, x1, y1 = r[0] - pad, r[1] - pad, r[2] + pad, r[3] + pad
    px, py = p
    dx, dy = q[0] - px, q[1] - py
    t0, t1 = 0.0, 1.0
    for num, den in ((-dx, px - x0), (dx, x1 - px), (-dy, py - y0), (dy, y1 - py)):
        if num == 0:
            if den < 0:
                return False
        else:
            t = den / num
            if num < 0:
                if t > t1:
                    return False
                t0 = max(t0, t)
            else:
                if t < t0:
                    return False
                t1 = min(t1, t)
    return t0 <= t1


def crosses(p, q, r, s):
    def side(a, b, c):
        v = (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0])
        return 0 if abs(v) < 1e-9 else (1 if v > 0 else -1)
    return (side(p, q, r) * side(p, q, s) < 0
            and side(r, s, p) * side(r, s, q) < 0)


def seg_gap(p, q, r, s):
    def point_seg(a, b0, b1):
        bx, by = b1[0] - b0[0], b1[1] - b0[1]
        n = bx * bx + by * by
        if n == 0:
            return math.hypot(a[0] - b0[0], a[1] - b0[1])
        t = max(0.0, min(1.0, ((a[0] - b0[0]) * bx + (a[1] - b0[1]) * by) / n))
        return math.hypot(a[0] - b0[0] - t * bx, a[1] - b0[1] - t * by)
    return min(point_seg(p, r, s), point_seg(q, r, s),
               point_seg(r, p, q), point_seg(s, p, q))


# What each constraint is worth. Anything an audience would notice as a mistake
# -- an edge behind a box, two edges crossing -- outweighs everything that is
# merely a matter of taste.
COST = dict(behind_box=500, crossing=260, mixed_crossing=70, crowded=160, close=45,
            shallow=40, flat=240, overlap=80, inside_class=260, out_of_order=500,
            length=0.004, preferred=0.4)
SHALLOW = 1.2                    # |dx| / |dy| a plain edge may reach
FLAT = 2.5                       # and past this it reads as a horizontal line
CLOSE = 16.0                     # two edges nearer than this crowd each other
HEAD, TAIL = 0.30, 0.22          # radians between two edges at one node


def violations(g, x, lay, rows):
    """every constraint the placement breaks, as (weight, wording) pairs.

    This is both the check and the score: solving minimises the total weight,
    and `--check` prints the pairs. Only the rows an edge actually passes are
    looked at, which is what makes it quick enough to anneal against.
    """
    geo = Geo(g, x, lay, rows)
    out = []
    for r in range(len(rows)):
        ks = sorted([k for k in g.nodes if lay[k] == r], key=lambda k: x[k])
        for a, b in zip(ks, ks[1:]):
            over = (x[a] + g.w(a) + GAP) - x[b]
            if over > 0:
                out.append((COST['overlap'] * over,
                            "%s and %s are %.0fpt too close" % (a, b, over)))
        for k in ks:
            if x[k] < LEFT - 0.5 or x[k] + g.w(k) > RIGHT + 0.5:
                out.append((COST['overlap'] * 10.0, "%s is outside the column" % k))
    segs = {e: geo.seg(e) for e in g.edges}
    span = {}
    for e, (p, q) in segs.items():
        span[e] = (min(p[1], q[1]), max(p[1], q[1]))
    boxes = {k: geo.rect(k) for k in g.nodes}
    for e, (p, q) in segs.items():
        lo, hi = span[e]
        for k in g.nodes:
            if k in e:
                continue
            r = boxes[k]
            if r[3] < lo - 4 or r[1] > hi + 4:      # not in a row this edge passes
                continue
            if hits_box(p, q, r):
                out.append((COST['behind_box'],
                            "%s->%s passes behind %s" % (e[0], e[1], k)))
    for e, f in combinations(g.edges, 2):
        p, q = segs[e]
        r, s = segs[f]
        if set(e) & set(f):
            a1 = math.atan2(q[1] - p[1], q[0] - p[0])
            a2 = math.atan2(s[1] - r[1], s[0] - r[0])
            d = abs((a1 - a2 + math.pi) % (2 * math.pi) - math.pi)
            limit = HEAD if e[1] == f[1] else (TAIL if e[0] == f[0] else 0.0)
            if d < limit:
                out.append((COST['crowded'] * (limit - d) / limit,
                            "%s->%s and %s->%s meet at %d degrees"
                            % (e[0], e[1], f[0], f[1], math.degrees(d))))
            continue
        if span[e][0] > span[f][1] or span[f][0] > span[e][1]:
            continue                                 # nowhere near each other
        if crosses(p, q, r, s):
            same = g.kind(e) == g.kind(f)
            # two edges that cross at a wide angle read as two edges; two that
            # graze each other read as a mess, so the angle is part of the price
            a1 = math.atan2(q[1] - p[1], q[0] - p[0])
            a2 = math.atan2(s[1] - r[1], s[0] - r[0])
            steep = abs(math.sin(a1 - a2))
            price = (COST['crossing'] if same else COST['mixed_crossing'])
            out.append((price * (0.6 + 0.8 * (1.0 - steep)),
                        "%s->%s crosses %s->%s at %d degrees%s"
                        % (e[0], e[1], f[0], f[1], math.degrees(math.asin(steep)),
                           "" if same else ", blue over black")))
        else:
            d = seg_gap(p, q, r, s)
            if d < CLOSE:
                out.append((COST['close'] * (CLOSE - d) / CLOSE,
                            "%s->%s runs %.0fpt from %s->%s"
                            % (e[0], e[1], d, f[0], f[1])))
    for e, (p, q) in segs.items():
        dx, dy = abs(q[0] - p[0]), abs(q[1] - p[1])
        ratio = dx / max(dy, 1.0)
        if ratio > SHALLOW:
            out.append((COST['shallow'] * (ratio - SHALLOW)
                        + (COST['flat'] if ratio > FLAT else 0.0),
                        "%s->%s is %s (%.1f across for 1 down)"
                        % (e[0], e[1], "flat" if ratio > FLAT else "shallow", ratio)))
    for ms in g.mates:
        lo = min(x[k] for k in ms)
        hi = max(x[k] + g.w(k) for k in ms)
        for k in g.nodes:
            if k not in ms and lay[k] == lay[ms[0]] and lo < x[k] + g.w(k) / 2 < hi:
                out.append((COST['inside_class'],
                            "%s sits inside the class of %s" % (k, "/".join(ms))))
    for k, kids in g.kids.items():
        for before, after in zip(kids, kids[1:]):
            right = max(x[m] + g.w(m) / 2 for m in before)
            left = min(x[m] + g.w(m) / 2 for m in after)
            if right > left - 8.0:           # the next argument must be clear right
                out.append((COST['out_of_order'] + (right - left),
                            "%s's arguments are out of order: %s is not left of %s"
                            % (k, "/".join(before), "/".join(after))))
    return out


def cost(g, x, lay, rows, prefer=()):
    c = sum(w for w, _ in violations(g, x, lay, rows))
    geo = Geo(g, x, lay, rows)
    for e in g.edges:
        p, q = geo.seg(e)
        c += COST['length'] * math.hypot(q[0] - p[0], q[1] - p[1])
    for l, r in prefer:                       # left-to-right reading order
        d = (x[l] + g.w(l) / 2) - (x[r] + g.w(r) / 2)
        if d > 0:
            c += COST['preferred'] * d
    return c


# ---------------------------------------------------------------------------
# solving: rows from the classes, then an order per row, then the coordinates

def dummies(lay, edges):
    """split each edge into one-row steps, so an edge that spans three rows is
    counted where it passes the rows in between rather than only at its ends"""
    extra, steps = {}, []
    for i, (u, v) in enumerate(edges):
        prev = u
        for r in range(lay[u] - 1, lay[v], -1):
            d = ("edge%d" % i, r)
            extra[d] = r
            steps.append((prev, d))
            prev = d
        steps.append((prev, v))
    return extra, steps


def count_crossings(steps, pos, lay):
    n = 0
    for (a, b), (c, d) in combinations(steps, 2):
        if lay[a] != lay[c] or lay[b] != lay[d] or a == c or b == d:
            continue
        if (pos[a] - pos[c]) * (pos[b] - pos[d]) < 0:
            n += 1
    return n


def order_rows(lay, steps, rows, seed=0, sweeps=12):
    """constraint 2, on the ordering alone: median sweeps, then adjacent swaps"""
    rnd = random.Random(seed)
    order = {r: [k for k in lay if lay[k] == r] for r in rows}
    for r in rows:
        rnd.shuffle(order[r])
    above, below = {}, {}
    for u, v in steps:
        above.setdefault(u, []).append(v)
        below.setdefault(v, []).append(u)

    def positions():
        return {k: i for r in rows for i, k in enumerate(order[r])}

    def median(k, nbrs, pos):
        vs = sorted(pos[n] for n in nbrs.get(k, []))
        if not vs:
            return None
        m = len(vs) // 2
        return vs[m] if len(vs) % 2 else (vs[m - 1] + vs[m]) / 2.0

    best = count_crossings(steps, positions(), lay)
    kept = {r: list(order[r]) for r in rows}
    for s in range(sweeps):
        nbrs = above if s % 2 == 0 else below
        for r in (rows if s % 2 == 0 else rows[::-1]):
            pos = positions()
            keyed = []
            for i, k in enumerate(order[r]):
                m = median(k, nbrs, pos)
                keyed.append((m if m is not None else pos[k], i, k))
            order[r] = [k for _, _, k in sorted(keyed, key=lambda t: (t[0], t[1]))]
        improving = True
        while improving:
            improving = False
            now = count_crossings(steps, positions(), lay)
            for r in rows:
                for i in range(len(order[r]) - 1):
                    order[r][i], order[r][i + 1] = order[r][i + 1], order[r][i]
                    swapped = count_crossings(steps, positions(), lay)
                    if swapped < now:
                        now, improving = swapped, True
                    else:
                        order[r][i], order[r][i + 1] = order[r][i + 1], order[r][i]
        now = count_crossings(steps, positions(), lay)
        if now < best:
            best, kept = now, {r: list(order[r]) for r in rows}
    return best, kept


def spread(g, order, rows):
    """a starting placement: each row's nodes in order, centred in the column"""
    x = {}
    for r in rows:
        ks = [k for k in order[r] if k in g.box]
        total = sum(g.w(k) for k in ks) + GAP * (len(ks) - 1)
        at = LEFT + ((RIGHT - LEFT) - total) / 2
        for k in ks:
            x[k] = at
            at += g.w(k) + GAP
    return x


def fit_row(targets, gaps, lo, hi):
    """the row's centres, as near their targets as the order allows.

    Least squares under `x[i] + gaps[i] <= x[i+1]`, which is isotonic regression
    once the gaps are subtracted out, so pool-adjacent-violators solves it exactly
    -- no sweeping a node at a time and hoping the row settles.
    """
    n = len(targets)
    if n == 0:
        return []
    offs = [0.0] * n
    for i in range(1, n):
        offs[i] = offs[i - 1] + gaps[i - 1]
    blocks = []                              # [sum, count] of pooled targets
    for t, off in zip(targets, offs):
        blocks.append([t - off, 1])
        while (len(blocks) > 1
               and blocks[-2][0] / blocks[-2][1] > blocks[-1][0] / blocks[-1][1]):
            total, count = blocks.pop()
            blocks[-1][0] += total
            blocks[-1][1] += count
    flat = []
    for total, count in blocks:
        flat += [total / count] * count
    x = [v + off for v, off in zip(flat, offs)]
    if x[0] < lo:                            # and inside the column it has to fit
        x = [v + (lo - x[0]) for v in x]
    if x[-1] > hi:
        x = [max(lo, v - (x[-1] - hi)) for v in x]
    return x


def priority_x(g, order, rows, sweeps=10, dummy_w=20.0):
    """a placement: each node as near the middle of its neighbours as the order
    allows. The dummies stand in the rows a long edge passes, so the edge is given
    a lane with no box in it."""
    width = dict(g.box)
    for r in order:
        for k in order[r]:
            width.setdefault(k, dummy_w)
    above, below = {}, {}
    for u, v in g.steps:
        above.setdefault(u, []).append(v)
        below.setdefault(v, []).append(u)
    mid = {}
    for r in order:
        total = sum(width[k] for k in order[r]) + GAP * (len(order[r]) - 1)
        at = LEFT + ((RIGHT - LEFT) - total) / 2
        for k in order[r]:
            mid[k] = at + width[k] / 2
            at += width[k] + GAP
    for pass_no in range(sweeps):
        nbrs = above if pass_no % 2 == 0 else below
        rs = sorted(order) if pass_no % 2 == 0 else sorted(order, reverse=True)
        for r in rs:
            ks = order[r]
            targets = []
            for k in ks:
                ns = nbrs.get(k, [])
                targets.append(sum(mid[n] for n in ns) / len(ns) if ns else mid[k])
            gaps = [width[a] / 2 + GAP + width[b] / 2 for a, b in zip(ks, ks[1:])]
            placed = fit_row(targets, gaps, LEFT + width[ks[0]] / 2,
                             RIGHT - width[ks[-1]] / 2)
            for k, v in zip(ks, placed):
                mid[k] = v
    return {k: mid[k] - g.box[k] / 2 for k in g.nodes}


def orderings(g, rows):
    """every order of every row, cheapest first by what it actually draws.

    The rows hold two to four nodes each, so there are a few thousand orders in
    all and no need to be clever about which to look at.
    """
    per_row = []
    for r in range(len(rows)):
        ks = [k for k in g.nodes if g.lay[k] == r]
        ds = [d for d in g.dummy if g.dummy[d] == r]
        per_row.append((r, ks, ds))
    def keeps_classes_together(ks):
        for ms in g.mates:
            spots = [i for i, k in enumerate(ks) if k in ms]
            if spots and spots[-1] - spots[0] != len(spots) - 1:
                return False                 # something sits inside the class
        return True
    out = []
    for choice in product(*[permutations(ks) for _, ks, _ in per_row]):
        if not all(keeps_classes_together(ks) for ks in choice):
            continue
        order = {}
        for (r, _, ds), ks in zip(per_row, choice):
            # a dummy is put where its own edge wants it, which the sweeps sort out;
            # starting them at the middle of the row is enough
            order[r] = list(ks[:len(ks) // 2]) + list(ds) + list(ks[len(ks) // 2:])
        x = priority_x(g, order, rows)
        out.append((cost(g, x, g.lay, rows), order, x))
    out.sort(key=lambda t: t[0])
    return out


def keeps_order(g, x, lay, moved, order):
    """the left-to-right order of a row is the ordering step's answer, not the
    anneal's: it is what keeps each e-class contiguous, so a move that would
    reshuffle a row is refused"""
    if order is None:
        return True
    row = [k for k in order[lay[moved]] if k in g.box]
    return all(x[a] < x[b] for a, b in zip(row, row[1:]))


STEPS = (-96.0, -48.0, -24.0, -12.0, -6.0, -3.0, 3.0, 6.0, 12.0, 24.0, 48.0, 96.0)


def polish(g, x0, lay, rows, prefer, iters=12000, seed=0, order=None):
    """constraints 3-5, on the coordinates.

    Annealed, since the cost is what the picture is judged by and it is not smooth
    in the positions. The move is picked out of the violations rather than at
    random -- something the placement gets wrong, then a node it names -- because a
    node that is already fine is rarely the one worth moving.
    """
    rnd = random.Random(seed)
    x = dict(x0)
    now = best = cost(g, x, lay, rows, prefer)
    kept = dict(x)
    guilty = [text for _, text in violations(g, x, lay, rows)]
    for i in range(iters):
        heat = max(0.4, 30.0 * (1 - i / iters))
        k = None
        if guilty and rnd.random() < 0.75:
            named = [n for n in g.nodes if n in rnd.choice(guilty).split()]
            if named:
                k = rnd.choice(named)
        k = k or rnd.choice(g.nodes)
        was = x[k]
        x[k] = was + rnd.choice(STEPS)
        if (not (LEFT <= x[k] <= RIGHT - g.w(k))
                or not keeps_order(g, x, lay, k, order)):
            x[k] = was
            continue
        c = cost(g, x, lay, rows, prefer)
        if c < now or rnd.random() < math.exp((now - c) / heat):
            now = c
            guilty = [text for _, text in violations(g, x, lay, rows)]
        else:
            x[k] = was
        if now < best:
            best, kept = now, dict(x)
    return best, kept


def solve(g, rows, prefer, keep=12, seeds=(1, 2), iters=9000, lay=None):
    """rows from the classes, then the best order per row, then coordinates"""
    lay = g.prepare(lay)
    tries = orderings(g, rows)
    best = None
    for score, order, x in tries[:keep]:
        for seed in seeds:
            c, placed = polish(g, x, lay, rows, prefer, iters=iters, seed=seed,
                               order=order)
            if best is None or c < best[0]:
                best = (c, placed, order)
    return lay, best


# ---------------------------------------------------------------------------

# `x + 1` reads left to right, and the paper puts the memory after the value it
# comes with, so where the constraints leave a choice these break the tie.
PREFER = [("x", "one"), ("v1", "s1"), ("v2", "s2"), ("v3", "s3"), ("a", "s0")]


def rhombus(g, x, lay, rows):
    out = ["def chain_rows: [" + ", ".join("%.1f" % y for y in rows) + "]",
           "def chain_pos:"]
    for r in range(len(rows)):
        ks = sorted([k for k in g.nodes if lay[k] == r], key=lambda k: x[k])
        out.append("  " + ("{ " if r == 0 else "  ")
                   + ", ".join("#'%s: [%.1f, %d]" % (k, x[k], r) for k in ks)
                   + ("," if r < len(rows) - 1 else " }"))
    return "\n".join(out)


# A picture with a few crossings in it is still a good picture; a picture with an
# edge behind a box, two boxes touching, or an edge that reads as a horizontal line
# is not. Only the second kind fails the check.
SERIOUS = ("passes behind", "too close", "outside the column",
           "inside the class", "is flat", "out of order")


def serious(text):
    return any(mark in text for mark in SERIOUS)


def show(name, g, x, lay, rows):
    print("== %s" % name)
    for r in range(len(rows)):
        ks = sorted([k for k in g.nodes if lay[k] == r], key=lambda k: x[k])
        print("   row %d (y %.0f): " % (r, rows[r])
              + "   ".join("%s@%.0f" % (k, x[k]) for k in ks))
    bad = sorted(violations(g, x, lay, rows), key=lambda t: -t[0])
    if not bad:
        print("   every constraint met")
    for w, text in bad:
        print("   %s%s" % ("! " if serious(text) else "  ", text))
    return bad


TOP, ROW_GAP = 230.0, 92.0            # where the rows start, and how far apart


def main():
    solving = "--solve" in sys.argv
    g, rows, pos = read_talk(placement=not solving)
    if solving:
        lay = g.layers()
        rows = [TOP + ROW_GAP * i for i in range(max(lay.values()) + 1)]
        lay, (c, x, order) = solve(g, rows, PREFER)
        show("solved (cost %.0f)" % c, g, x, lay, rows)
        print()
        print(rhombus(g, x, lay, rows))
        return
    lay = {k: r for k, (_, r) in pos.items()}
    want = g.layers()
    for k in g.nodes:
        if k not in pos:
            raise SystemExit("graph_layout: chain_pos says nothing about %s" % k)
    x = {k: pos[k][0] for k in g.nodes}
    bad = show("talk.rhm", g, x, lay, rows)
    wrong = [text for _, text in bad if serious(text)]
    crossings = [text for _, text in bad if "crosses" in text]
    print("   -- %d serious, %d crossings (%d of them blue over black)"
          % (len(wrong), len(crossings),
             sum(1 for t in crossings if "blue over black" in t)))
    # constraint 1 is about the rows, and the rows are hand-written too
    for k in g.nodes:
        for a in g.args[k]:
            for m in g.members[a]:
                if lay[k] <= lay[m]:
                    print("   ! %s is not below %s, which it is built from" % (k, m))
    for ms in g.mates:
        if len({lay[m] for m in ms}) > 1:
            print("   ! the class %s is spread over more than one row" % "/".join(ms))
    sys.exit(1 if wrong else 0)


if __name__ == "__main__":
    main()
