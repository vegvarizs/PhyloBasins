# =============================================================================
# Tests for pb_test_project()
# =============================================================================

test_that("empty stage returns a valid project", {

  pb <- pb_test_project("empty")

  expect_valid_pb_project(pb)

  expect_false(pb$tree$loaded)
  expect_false(pb$tree$prepared)
  expect_false(pb$branches$prepared)
  expect_false(pb$community$loaded)
  expect_false(pb$site_branch_matrix$built)
  expect_false(pb$branch_ranges$computed)

})

test_that("tree stage is valid", {

  pb <- pb_test_project("tree")

  expect_valid_pb_project(pb)
  expect_valid_tree(pb)

  expect_true(pb$tree$loaded)
  expect_false(pb$tree$prepared)

})

test_that("prepared_tree stage is valid", {

  pb <- pb_test_project("prepared_tree")

  expect_valid_pb_project(pb)
  expect_valid_tree(pb)

  expect_true(pb$tree$prepared)

})

test_that("branches stage is valid", {

  pb <- pb_test_project("branches")

  expect_valid_pb_project(pb)
  expect_valid_branches(pb)

  expect_true(pb$branches$prepared)

})

test_that("community stage is valid", {

  pb <- pb_test_project("community")

  expect_valid_pb_project(pb)
  expect_valid_community(pb)

  expect_true(pb$community$loaded)

})

test_that("site_branch_matrix stage is valid", {

  pb <- pb_test_project("site_branch_matrix")

  expect_valid_pb_project(pb)
  expect_valid_site_branch_matrix(pb)

  expect_true(pb$site_branch_matrix$built)

})

test_that("branch_ranges stage is valid", {

  pb <- pb_test_project("branch_ranges")

  expect_valid_pb_project(pb)
  expect_valid_branch_ranges(pb)

  expect_true(pb$branch_ranges$computed)

})

test_that("PD stage is valid", {

  pb <- pb_test_project("pd")

  expect_valid_tree(pb)
  expect_valid_branches(pb)
  expect_valid_site_branch_matrix(pb)
  expect_valid_branch_ranges(pb)
  expect_valid_metrics(pb)

  expect_true(
    is.data.frame(pb$metrics$pd$values)
  )

  expect_identical(
    names(pb$metrics$pd$values),
    c("HYBAS_ID", "pd")
  )

  expect_false(
    any(is.na(pb$metrics$pd$values$pd))
  )

  expect_equal(
    nrow(pb$metrics$pd$values),
    nrow(pb$community$matrix)
  )

})

test_that("PE stage is valid", {

  pb <- pb_test_project("pe")

  expect_valid_tree(pb)
  expect_valid_branches(pb)
  expect_valid_site_branch_matrix(pb)
  expect_valid_branch_ranges(pb)
  expect_valid_metrics(pb)

  expect_true(pb$metrics$pe$computed)

  expect_true(
    is.data.frame(pb$metrics$pe$values)
  )

  expect_identical(
    names(pb$metrics$pe$values),
    c("HYBAS_ID", "pe")
  )

  expect_false(
    any(is.na(pb$metrics$pe$values$pe))
  )

  expect_equal(
    nrow(pb$metrics$pe$values),
    nrow(pb$community$matrix)
  )

})

test_that("RPE stage is valid", {

  pb <- pb_test_project("rpe")

  expect_valid_tree(pb)
  expect_valid_branches(pb)
  expect_valid_site_branch_matrix(pb)
  expect_valid_branch_ranges(pb)
  expect_valid_metrics(pb)

  expect_true(pb$metrics$rpe$computed)

  expect_true(
    is.data.frame(pb$metrics$rpe$values)
  )

  expect_identical(
    names(pb$metrics$rpe$values),
    c("HYBAS_ID", "rpe")
  )

  expect_false(
    any(is.na(pb$metrics$rpe$values$rpe))
  )

  expect_equal(
    nrow(pb$metrics$rpe$values),
    nrow(pb$community$matrix)
  )
})

test_that("invalid stage fails", {

  expect_error(

    pb_test_project("foobar"),

    "arg"

  )

})
