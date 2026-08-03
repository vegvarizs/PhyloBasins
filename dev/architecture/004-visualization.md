# Architecture Note 004

## Title

Visualization Module

---

Status: Proposed

Version: v0.5.0

Last updated: 2026-08-03

Authors:
    Zsolt Végvári
    OpenAI ChatGPT

---

# Purpose

The Visualization Module provides a consistent graphical interface for
all diversity metrics computed by the PhyloBasins package.

Rather than implementing separate plotting functions for every metric,
the module introduces a single plotting engine that operates on the
standardised metric representation.

The objective is to produce publication-quality figures while maintaining
a simple public API.

---

# Motivation

Current phylogenetic software often provides metric computation but
leaves graphical visualisation entirely to the user.

PhyloBasins aims to integrate computation and visualisation into a single
workflow while keeping both components modular.

The plotting system should therefore

- operate on completed analyses,
- never modify project objects,
- remain independent from metric computation,
- support future diversity metrics without structural changes.

---

# Design principles

## 1. Read-only behaviour

Plotting functions never modify the project.

They simply convert project information into graphical objects.

---

## 2. Generic plotting engine

Only one plotting engine should exist.

```
plot_metric()
```

All metric-specific plotting functions become lightweight wrappers.

---

## 3. Standard metric interface

The plotting engine never accesses the internal metric structure
directly.

Instead it retrieves metric values using

```
metric_table()
```

This isolates the plotting layer from future internal changes.

---

## 4. ggplot compatibility

The plotting engine returns a standard ggplot object.

It never saves figures automatically.

Example

```r
p <- plot_pd(pb, shape)

print(p)

ggsave("PD.png", p)
```

---

## 5. Extensibility

New diversity metrics should become immediately plottable simply by
adding new wrappers.

No modifications to the plotting engine should be required.

---

# Planned architecture

```
pb_project

        │

        ▼

metric_table()

        │

        ▼

plot_metric()

        │

        ├── plot_pd()

        ├── plot_pe()

        ├── plot_rpe()

        ├── plot_rpd()

        ├── plot_canape()

        └── ...
```

---

# Responsibilities

The Visualization Module is responsible for

- joining metric values with spatial data,
- generating publication-quality maps,
- providing consistent graphical defaults,
- returning ggplot objects.

It is **not** responsible for

- computing metrics,
- exporting figures,
- reading shapefiles,
- modifying project objects.

---

# Public API

The planned public interface is

```r
plot_metric(
    pb,
    shape,
    metric = "pd"
)
```

Convenience wrappers include

```r
plot_pd()

plot_pe()

plot_rpe()
```

Future wrappers may include

```r
plot_rpd()

plot_canape()
```

without modifying the plotting engine.

---

# Default graphical style

The default appearance should prioritise publication-quality figures.

Current design goals include

- ggplot2
- geom_sf()
- viridis colour scales
- minimal theme
- no grid
- optional borders
- automatic legends

Users remain free to customise returned ggplot objects.

---

# Internal dependencies

The module depends on

- ggplot2
- sf
- metric_table()

It should not depend directly on

- compute_pd()
- compute_pe()
- compute_rpe()

---

# Future extensions

Potential future developments include

- interactive maps
- leaflet support
- tmap support
- faceted comparisons
- automatic hotspot maps
- uncertainty visualisation

These additions should require no changes to the public plotting API.

---

# Alternatives considered

Alternative designs included

- independent plotting functions,
- automatic figure export,
- storing plots inside pb_project.

These alternatives were rejected because they increase complexity,
duplicate code and reduce flexibility.

---

# Expected stability

The generic plotting interface is intended to remain stable throughout
future package development.

New metrics should be incorporated by extending wrappers rather than by
changing plot_metric().

---

# Related Architecture Notes

001 — Project object

002 — Branch Engine

003 — Pipeline
