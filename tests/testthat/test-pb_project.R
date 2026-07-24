test_that("pb_project creates a valid object", {

  pb <- pb_project()

  expect_s3_class(pb, "pb_project")

  expect_true(is.list(pb))

  expect_equal(
    names(pb),
    c(
      "config",
      "metadata",
      "history",
      "tree",
      "branches",
      "community",
      "site_branch_matrix",
      "metrics",
      "maps",
      "cache"
    )
  )

})
