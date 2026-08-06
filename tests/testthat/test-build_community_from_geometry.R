# tests/testthat/test-build_community_from_geometry.R

test_that("build_community_from_geometry() builds a community object", {

  pb <- pb_test_project(stage = "geometry")

  attrs <- data.frame(
    HYBAS_ID = c("S1", "S2", "S3"),
    SP_A = c(1, 0, 1),
    SP_B = c(0, 1, 1),
    SP_C = c(1, 1, 0),
    stringsAsFactors = FALSE
  )

  pb$geometry$data <- attrs

  pb <- build_community_from_geometry(
    pb,
    species_columns = c("SP_A", "SP_B", "SP_C"),
    verbose = FALSE
  )

  expect_s3_class(pb$community, "pb_community")

  expect_equal(

    rownames(pb$community$matrix),

    attrs$HYBAS_ID

  )

  expect_equal(

    colnames(pb$community$matrix),

    c("SP_A", "SP_B", "SP_C")

  )

  expect_equal(

    dim(pb$community$matrix),

    c(3, 3)

  )

})

test_that("species columns can be selected by first and last names", {

  pb <- pb_test_project(stage = "geometry")

  pb$geometry$data <- data.frame(

    HYBAS_ID = c("A", "B"),

    META = c(10, 20),

    SP_A = c(1, 0),

    SP_B = c(0, 1),

    SP_C = c(1, 1),

    OTHER = c(5, 6),

    stringsAsFactors = FALSE

  )

  pb <- build_community_from_geometry(

    pb,

    first_species = "SP_A",

    last_species = "SP_C",

    verbose = FALSE

  )

  expect_equal(

    colnames(pb$community$matrix),

    c("SP_A", "SP_B", "SP_C")

  )

})

test_that("unknown site identifier throws an error", {

  pb <- pb_test_project(stage = "geometry")

  pb$geometry$data <- data.frame(

    SITE = c(1, 2),

    SP_A = c(1, 0),

    stringsAsFactors = FALSE

  )

  expect_error(

    build_community_from_geometry(

      pb,

      species_columns = "SP_A",

      site_id = "HYBAS_ID",

      verbose = FALSE

    ),

    "Site identifier"

  )

})

test_that("unknown species columns throw an error", {

  pb <- pb_test_project(stage = "geometry")

  pb$geometry$data <- data.frame(

    HYBAS_ID = c(1, 2),

    SP_A = c(1, 0),

    stringsAsFactors = FALSE

  )

  expect_error(

    build_community_from_geometry(

      pb,

      species_columns = c("SP_A", "SP_X"),

      verbose = FALSE

    ),

    "Unknown species columns"

  )

})

test_that("non-binary species columns are rejected", {

  pb <- pb_test_project(stage = "geometry")

  pb$geometry$data <- data.frame(

    HYBAS_ID = c(1, 2),

    SP_A = c(0, 2),

    stringsAsFactors = FALSE

  )

  expect_error(

    build_community_from_geometry(

      pb,

      species_columns = "SP_A",

      verbose = FALSE

    ),

    "0/1"

  )

})

test_that("logical species columns are accepted", {

  pb <- pb_test_project(stage = "geometry")

  pb$geometry$data <- data.frame(

    HYBAS_ID = c(1, 2),

    SP_A = c(TRUE, FALSE),

    SP_B = c(FALSE, TRUE),

    stringsAsFactors = FALSE

  )

  pb <- build_community_from_geometry(

    pb,

    species_columns = c("SP_A", "SP_B"),

    verbose = FALSE

  )

  expect_equal(

    pb$community$matrix,

    matrix(

      c(1,0,
        0,1),

      nrow = 2,

      byrow = TRUE,

      dimnames = list(

        c("1","2"),

        c("SP_A","SP_B")

      )

    )

  )

})
