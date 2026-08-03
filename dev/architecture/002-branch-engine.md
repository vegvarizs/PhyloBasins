# Architecture Note 002

## Title

The Branch Engine

---

Status: Accepted

Version: v0.4.0

Last updated: 2026-08-03

Authors:
    Zsolt Végvári
 

---

# Purpose

The Branch Engine is the computational core of the PhyloBasins package.

Its purpose is to transform a phylogenetic tree into a collection of
branch-based data structures that allow multiple phylogenetic diversity
metrics to be computed efficiently.

Rather than traversing the tree independently for every metric,
all expensive tree operations are performed once and cached for reuse.

This architecture substantially reduces computational cost and provides
a common foundation for all downstream analyses.

---

# Design principles

The Branch Engine follows five principles.

## 1. Compute once

Each expensive tree operation is performed only once.

Examples include

- descendant lists
- subtree lengths
- branch ranges

Subsequent metrics reuse the cached information.

---

## 2. Branch-centric representation

The engine treats branches—not nodes—as the primary computational unit.

Every branch has

- parent node
- child node
- branch length
- descendant tips
- descendant cache
- subtree length

This representation naturally supports Faith's PD, PE and future
branch-based metrics.

---

## 3. Metric independence

The Branch Engine itself computes no diversity metrics.

Its sole responsibility is to prepare reusable branch information.

Metrics such as

- PD
- PE
- RPE

operate entirely on the cached branch representation.

---

## 4. Separation of responsibilities

The Branch Engine does not

- read files,
- manipulate communities,
- create plots,
- export data.

Its only task is preparing reusable branch information.

---

## 5. Extensibility

New metrics should use the existing branch representation whenever
possible.

No changes to the Branch Engine should be required when implementing
new branch-based metrics.

---

# Workflow

The Branch Engine consists of four sequential stages.

```
prepare_tree()

        │

        ▼

build_branch_table()

        │

        ▼

build_site_branch_matrix()

        │

        ▼

compute_branch_ranges()
```

Each stage produces a reusable object.

---

# Stage 1

## prepare_tree()

Responsibilities

- validate tree
- reorder edges
- compute node indices
- initialise caches

Output

Prepared tree object.

---

# Stage 2

## build_branch_table()

Creates one record for every branch.

Each record stores

- branch identifier
- parent node
- child node
- branch length
- descendant tips
- subtree length

This table becomes the primary internal representation of the tree.

---

# Stage 3

## build_site_branch_matrix()

Constructs the sparse matrix linking

sites

↓

branches

Each matrix element indicates whether a branch contributes to the
phylogenetic diversity of a given site.

This matrix is reused by all branch-based metrics.

---

# Stage 4

## compute_branch_ranges()

Computes geographic branch ranges.

Current implementation provides

- proportional range
- inverse range
- normalised inverse range

These values are reused by PE and RPE.

---

# Computational complexity

Without caching, every metric would repeatedly traverse the tree.

The Branch Engine avoids repeated traversals by caching branch
information.

Consequently,

additional metrics require substantially less computation than the
initial preprocessing stage.

---

# Advantages

Compared with repeated tree traversals, the Branch Engine

- reduces computational cost,
- reduces duplicated code,
- enables caching,
- simplifies testing,
- supports future metrics.

---

# Future extensions

The Branch Engine has been designed to support

- Relative PD
- CANAPE
- evolutionary distinctiveness metrics
- branch rarity indices
- custom branch weights

without structural modifications.

---

# Alternatives considered

Alternative implementations included

- repeated recursive traversals,
- metric-specific tree processing,
- node-based representations.

These approaches were rejected because they duplicate computation and
make maintenance considerably more difficult.

---

# Stability

The public behaviour of the Branch Engine is considered stable from
version 0.4 onwards.

Future development should extend the engine without changing its
external behaviour.

---

# Related Architecture Notes

001 — Project object

003 — Pipeline

004 — Visualization
