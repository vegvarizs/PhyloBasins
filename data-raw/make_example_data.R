library(ape)

## Example tree
example_tree <- read.tree(text =
                            "((Species_A:1,Species_B:1):1,
    (Species_C:1,(Species_D:1,Species_E:1):0.5):0.5);")

## Example community matrix
example_community <- data.frame(
  Species_A = c(1,0,1),
  Species_B = c(1,1,0),
  Species_C = c(0,1,1),
  Species_D = c(0,0,1),
  Species_E = c(1,0,1),
  row.names = c("Basin_1","Basin_2","Basin_3")
)

usethis::use_data(
  example_tree,
  example_community,
  overwrite = TRUE
)
