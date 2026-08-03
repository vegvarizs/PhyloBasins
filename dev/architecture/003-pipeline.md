# Architecture Note 003

## Title

Analysis Pipeline

---

Status: Accepted

Version: v0.4.0

Last updated: 2026-08-03

Authors:
    Zsolt Végvári

---

# Purpose

The Analysis Pipeline defines the standard workflow for all analyses
performed by the PhyloBasins package.

Rather than exposing numerous low-level functions to the user, the
pipeline provides a small number of high-level operations that execute
the complete analysis in a reproducible and consistent manner.

The pipeline guarantees that every required preprocessing step is
performed before diversity metrics are computed.

---

# Design principles

The pipeline follows five principles.

## 1. Sequential execution

Each stage depends on the successful completion of previous stages.

The user should never execute later stages before prerequisite data have
been prepared.

---

## 2. Automatic dependency handling

High-level functions automatically execute missing prerequisite stages
whenever possible.

For example,

- missing branch ranges are computed before PE,
- missing branch tables are built before branch ranges.

Users therefore do not need to manage dependencies manually.

---

## 3. Reproducibility

The same sequence of operations always produces identical results for
identical inputs.

Every processing stage updates the project object in a deterministic
manner.

---

## 4. Cached computation

Previously computed components are reused whenever possible.

The pipeline avoids unnecessary recomputation unless explicitly requested
by the user.

---

## 5. High-level API

Most users should only interact with the pipeline rather than with
individual computational modules.

Typical workflow:

```r
pb <- pb_project()

pb <- read_tree(
    pb,
    tree
)

pb <- read_community(
    pb,
    community
)

pb <- run_pipeline(pb)
```

---

# Pipeline overview

The standard analysis pipeline consists of the following stages.

```
read_tree()

        │

        ▼

prepare_tree()

        │

        ▼

read_community()

        │

        ▼

build_branch_table()

        │

        ▼

build_site_branch_matrix()

        │

        ▼

compute_branch_ranges()

        │

        ▼

compute_all_metrics()
```

The resulting project contains all currently implemented diversity
metrics.

---

# Stage descriptions

## Tree import

Reads a phylogenetic tree and stores it within the project.

Responsibilities

- import
- validation
- standardisation

---

## Tree preparation

Transforms the imported tree into an internal representation suitable for
branch-based computations.

Responsibilities

- node indexing
- descendant cache
- subtree information
- validation

---

## Community import

Reads the community matrix and validates species identities.

Responsibilities

- community validation
- species matching
- storage

---

## Branch Engine

Builds reusable branch-based data structures.

Responsibilities

- branch table
- site-branch matrix
- branch ranges

This stage is described in Architecture Note 002.

---

## Metric computation

Computes all currently implemented diversity metrics.

Current metrics

- Faith's PD
- Phylogenetic Endemism (PE)
- Relative Phylogenetic Endemism (RPE)

Future metrics will be added without changing the pipeline structure.

---

# Public API

The preferred user interface consists of only a few functions.

```r
pb <- pb_project()

pb <- read_tree(pb, tree)

pb <- read_community(pb, community)

pb <- run_pipeline(pb)
```

Advanced users may call lower-level functions directly, but this is not
required for standard analyses.

---

# Internal dependencies

```
Tree

↓

Prepared tree

↓

Branch table

↓

Site-branch matrix

↓

Branch ranges

↓

Metrics
```

Each stage consumes only outputs from previous stages.

This strict dependency structure reduces implementation complexity and
simplifies testing.

---

# Error handling

The pipeline validates every stage before continuing.

Typical checks include

- tree validity,
- community validity,
- branch availability,
- metric prerequisites.

Errors are reported immediately when prerequisite data are missing.

---

# Advantages

Compared with manually executing multiple functions, the pipeline

- guarantees correct execution order,
- reduces user error,
- simplifies reproducibility,
- enables caching,
- provides a stable public interface.

---

# Future extensions

The pipeline has been designed to accommodate future modules including

- visualization,
- export,
- reporting,
- benchmarking,
- additional diversity metrics.

These extensions will operate on the completed project without modifying
the pipeline itself.

pb_project
    ↓
run_pipeline()
    ↓
plot_metric()
    ↓
export_metrics()
    ↓
report()

---

# Alternatives considered

Alternative approaches included

- completely manual workflows,
- metric-specific workflows,
- independent computational functions.

These designs were rejected because they increase user complexity and
make reproducibility substantially more difficult.

---

# Stability

The pipeline is considered the primary public interface of the package
from version 0.4 onwards.

Future development should preserve the public workflow while allowing
internal implementation to evolve.

---

# Related Architecture Notes

001 — Project object

002 — Branch Engine

004 — Visualization
