# PhyloBasins

**PhyloBasins** is an R package for branch-based phylogenetic analyses of ecological communities. It provides a reproducible workflow for validating phylogenetic trees and community matrices, constructing branch incidence tables, and calculating sparse site × branch matrices that form the basis of phylogenetic diversity analyses.

The package has been developed primarily for freshwater biodiversity analyses at river-basin and catchment scales but is applicable to any presence–absence community dataset linked to a phylogenetic tree.

---

## Features

Current functionality includes:

- Validation of phylogenetic trees
- Validation of community matrices
- Construction of branch tables
- Efficient sparse site × branch matrix generation
- Extensive automated unit testing
- Modular architecture for future phylogenetic diversity metrics

---

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("vegvarizs/PhyloBasins")
```

---

## Workflow

```text
Phylogenetic tree
        │
        ▼
prepare_tree()

        │
        ▼
build_branch_table()

        │
Community matrix
        │
        ▼
prepare_community()

        │
        ▼
reference_branch_engine()

        │
        ▼
Site × branch matrix
```

---

## Example

```r
library(PhyloBasins)

pb <- pb_project()

pb <- prepare_tree(pb, tree)

pb <- build_branch_table(pb)

pb <- prepare_community(pb, community)

pb <- build_site_branch_matrix(pb)

head(pb$site_branch_matrix)
```

---

## Planned functionality

Future releases will include methods for

- Faith's Phylogenetic Diversity (PD)
- Phylogenetic Endemism (PE)
- Relative Phylogenetic Diversity
- Relative Phylogenetic Endemism
- Additional branch-based biodiversity metrics

---

## Development status

The package is under active development.

Contributions, bug reports and feature requests are welcome.

---

## Authors

Zsolt Végvári

Institute of Aquatic Ecology  
HUN-REN Centre for Ecological Research

---

## License

See the LICENSE file for details.
