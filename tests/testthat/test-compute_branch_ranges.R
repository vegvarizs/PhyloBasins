test_that("compute_branch_ranges creates a branch range object", {

  pb <- pb_project()

  pb$tree$prepared <- TRUE

  pb$branches$prepared <- TRUE

  pb$branches$table <- data.frame(

    branch_id = c("b1","b2","b3"),

    length = c(1,2,3),

    stringsAsFactors = FALSE

  )

  pb$site_branch_matrix$built <- TRUE

  pb$site_branch_matrix$matrix <- matrix(

    c(

      1,1,0,
      0,1,1,
      1,1,1

    ),

    byrow = TRUE,

    nrow = 3

  )

  pb$branch_ranges <- pb_branch_ranges()

  pb <- compute_branch_ranges(

    pb,

    verbose = FALSE

  )

  expect_true(

    inherits(

      pb$branch_ranges,

      "pb_branch_ranges"

    )

  )

  expect_true(

    pb$branch_ranges$computed

  )

})

test_that("branch occurrences are computed correctly", {

  pb <- pb_project()

  pb$tree$prepared <- TRUE

  pb$branches$prepared <- TRUE

  pb$branches$table <- data.frame(

    branch_id = c("b1","b2","b3"),

    length = c(1,2,3),

    stringsAsFactors = FALSE

  )

  pb$site_branch_matrix$built <- TRUE

  pb$site_branch_matrix$matrix <- matrix(

    c(

      1,1,0,
      0,1,1,
      1,1,1

    ),

    byrow = TRUE,

    nrow = 3

  )

  pb$branch_ranges <- pb_branch_ranges()

  pb <- compute_branch_ranges(

    pb,

    verbose = FALSE

  )

  expect_equal(

    pb$branch_ranges$table$n_sites,

    c(2L,3L,2L)

  )

})

test_that("branch proportions are computed correctly", {

  pb <- pb_project()

  pb$tree$prepared <- TRUE

  pb$branches$prepared <- TRUE

  pb$branches$table <- data.frame(

    branch_id = c("b1","b2","b3"),

    length = c(1,2,3),

    stringsAsFactors = FALSE

  )

  pb$site_branch_matrix$built <- TRUE

  pb$site_branch_matrix$matrix <- matrix(

    c(

      1,1,0,
      0,1,1,
      1,1,1

    ),

    byrow = TRUE,

    nrow = 3

  )

  pb$branch_ranges <- pb_branch_ranges()

  pb <- compute_branch_ranges(

    pb,

    verbose = FALSE

  )

  expect_equal(

    pb$branch_ranges$table$proportion,

    c(2/3,1,2/3),

    tolerance = 1e-12

  )

})

test_that("overwrite protection works", {

  pb <- pb_project()

  pb$tree$prepared <- TRUE

  pb$branches$prepared <- TRUE

  pb$site_branch_matrix$built <- TRUE

  pb$branches$table <- data.frame(

    branch_id = "b1",

    length = 1

  )

  pb$site_branch_matrix$matrix <- matrix(

    1,

    nrow = 1

  )

  pb$branch_ranges <- pb_branch_ranges()

  pb <- compute_branch_ranges(

    pb,

    verbose = FALSE

  )

  expect_error(

    compute_branch_ranges(

      pb,

      verbose = FALSE

    ),

    "already been computed"

  )

})
