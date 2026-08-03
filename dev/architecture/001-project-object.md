# Architecture Note 001

## Title

The `pb_project` object

---

Status: Accepted

Version: v0.4.0

Last updated: 2026-08-03

Authors:
    Zsolt Végvári
    

---

# Purpose

The `pb_project` object is the central data structure of the PhyloBasins
package.

Rather than passing multiple independent objects between functions
(tree, community matrix, branch table, metrics, etc.), every stage of the
analysis operates on a single project object.

This design guarantees consistency, reproducibility and extensibility
throughout the complete analysis pipeline.

---

# Design principles

The project object follows five principles.

## 1. Single source of truth

Every analysis step modifies the same project object.

There is never more than one authoritative representation of

- the phylogenetic tree,
- community data,
- branch information,
- computed metrics.

---

## 2. Immutable workflow

Functions never return independent partial results.

Instead they return an updated project.

Example

```r
pb <- read_tree(pb, tree)

pb <- read_community(pb, community)

pb <- run_pipeline(pb)
```

This prevents inconsistencies between intermediate objects.

---

## 3. Modular architecture

Each component is stored in its own section.

```
pb_project

├── tree
├── community
├── branches
├── site_branch_matrix
├── branch_ranges
├── metrics
└── metadata
```

Each module has clearly defined responsibilities.

---

## 4. Lazy computation

Expensive objects are computed only when required.

Examples include

- descendant cache
- branch ranges
- PE
- RPE

Previously computed objects are reused whenever possible.

---

## 5. Public API

Users interact only with high-level functions.

Typical workflow:

```r
pb <- pb_project()

pb <- read_tree(pb, tree)

pb <- read_community(pb, community)

pb <- run_pipeline(pb)
```

Users should never manipulate internal components directly.

---

# Internal structure

Current project structure (v0.4)

```
pb

├── tree
│
├── community
│
├── branches
│
├── site_branch_matrix
│
├── branch_ranges
│
├── metrics
│      ├── pd
│      ├── pe
│      └── rpe
│
└── metadata
```

Each module stores

- data
- validation state
- computation state

independently.

---

# Responsibilities

The project object is responsible for

- storing analysis data,
- tracking validation status,
- storing intermediate results,
- ensuring compatibility between modules,
- providing a stable interface for downstream analyses.

It is **not** responsible for

- plotting,
- exporting,
- reporting.

These functions operate on the project object but do not become part of it.

---

# Advantages

Compared with independent function-based workflows, the project object

- reduces user error,
- guarantees reproducibility,
- simplifies testing,
- enables caching,
- supports future extensions.

---

# Future extensions

The object has been designed to accommodate additional modules without
changing the public API.

Planned extensions include

- RPD
- CANAPE
- SES metrics
- visualization
- export
- reporting
- benchmarking

---

# Alternatives considered

Alternative designs included

- passing independent objects between functions;
- storing only trees and communities;
- fully functional (stateless) API.

These approaches were rejected because they increase the risk of
inconsistent analyses and make caching substantially more difficult.

---

# Stability

The public interface of `pb_project` is considered stable from version
0.4 onwards.

Future development should extend the object without breaking the public API.

---

# Related Architecture Notes

002 — Branch Engine

003 — Pipeline

004 — Visualization
