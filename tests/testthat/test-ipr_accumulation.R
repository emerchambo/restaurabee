test_that("thresholds follow the existing selection rules", {

  priorities <- data.frame(
    plant_species = c("A", "B", "C", "D"),
    IPR = c(0.4, 0.3, 0.3, 0)
  )

  thresholds <- c(0.5, 0.8, 1)

  for (tie_rule in c("first", "include")) {

    result <- ipr_accumulation(
      priorities,
      thresholds = thresholds,
      ties = tie_rule
    )

    for (i in seq_along(thresholds)) {

      reference <- select_species(
        priorities,
        threshold = thresholds[i],
        ties = tie_rule
      )

      expect_equal(
        result$thresholds$k[i],
        nrow(reference)
      )

      expect_equal(
        result$thresholds$achieved_cumulative_IPR[i],
        tail(reference$cumulative_IPR, 1)
      )

      expect_equal(
        result$thresholds$boundary_IPR[i],
        tail(reference$IPR, 1)
      )
    }
  }
})


test_that("the curve retains all plants in decreasing score order", {

  priorities <- data.frame(
    plant_species = c("D", "C", "A", "B"),
    IPR = c(0, 0.3, 0.4, 0.3),
    information = c("d", "c", "a", "b")
  )

  result <- ipr_accumulation(priorities)

  expect_identical(
    result$curve$plant_species,
    c("A", "C", "B", "D")
  )

  expect_identical(
    result$curve$information,
    c("a", "c", "b", "d")
  )

  expect_equal(
    result$curve$k,
    1:4
  )

  expect_equal(
    result$curve$relative_IPR,
    c(0.4, 0.3, 0.3, 0)
  )

  expect_equal(
    result$curve$cumulative_IPR,
    c(0.4, 0.7, 1, 1)
  )

  expect_equal(
    result$settings$total_IPR,
    1
  )
})


test_that("boundary ties can be included", {

  priorities <- data.frame(
    plant_species = c("A", "B", "C", "D"),
    IPR = c(0.4, 0.3, 0.3, 0)
  )

  first <- ipr_accumulation(
    priorities,
    thresholds = 0.5,
    ties = "first"
  )

  included <- ipr_accumulation(
    priorities,
    thresholds = 0.5,
    ties = "include"
  )

  expect_equal(first$thresholds$k, 2)
  expect_equal(included$thresholds$k, 3)

  expect_equal(
    included$thresholds$achieved_cumulative_IPR,
    1
  )
})


test_that("a threshold of one retains every positive score", {

  priorities <- data.frame(
    plant_species = c("A", "B", "C"),
    IPR = c(1, 1e-18, 0)
  )

  result <- ipr_accumulation(
    priorities,
    thresholds = 1
  )

  expect_equal(result$thresholds$k, 2)

  expect_equal(
    result$thresholds$achieved_cumulative_IPR,
    1
  )

  expect_equal(nrow(result$curve), 3)
})


test_that("zero totals have undefined cumulative proportions", {

  priorities <- data.frame(
    plant_species = c("A", "B"),
    IPR = c(0, 0)
  )

  result <- ipr_accumulation(priorities)

  expect_equal(nrow(result$curve), 2)

  expect_true(
    all(is.na(result$curve$relative_IPR))
  )

  expect_true(
    all(is.na(result$curve$cumulative_IPR))
  )

  expect_true(
    all(is.na(result$thresholds$k))
  )

  expect_true(
    all(result$thresholds$status == "zero_total")
  )
})


test_that("invalid thresholds are rejected", {

  priorities <- data.frame(IPR = c(0.6, 0.4))

  invalid <- list(
    NULL,
    numeric(0),
    TRUE,
    NA_real_,
    NaN,
    Inf,
    -Inf,
    0,
    -0.1,
    1.1,
    c(0.5, 2),
    "0.8",
    matrix(0.8)
  )

  for (value in invalid) {

    expect_error(
      ipr_accumulation(
        priorities,
        thresholds = value
      ),
      "thresholds"
    )
  }

  expect_error(
    ipr_accumulation(priorities, ties = "unknown"),
    "arg"
  )
})


test_that("invalid prioritization tables are rejected", {

  expect_error(
    ipr_accumulation(NULL),
    "data frame"
  )

  expect_error(
    ipr_accumulation(data.frame(other = 1)),
    "Missing required columns"
  )

  expect_error(
    ipr_accumulation(data.frame(IPR = numeric())),
    "at least one row"
  )

  duplicated <- data.frame(
    first = 1,
    second = 2
  )

  names(duplicated) <- c("IPR", "IPR")

  expect_error(
    ipr_accumulation(duplicated),
    "unique column names"
  )

  invalid_scores <- list(
    "1",
    TRUE,
    -1,
    NA_real_,
    NaN,
    Inf,
    -Inf
  )

  for (value in invalid_scores) {

    expect_error(
      ipr_accumulation(data.frame(IPR = value)),
      "finite, non-negative numeric values"
    )
  }

  expect_error(
    ipr_accumulation(
      data.frame(IPR = c(1e308, 1e308))
    ),
    "sum of `IPR` must be finite",
    fixed = TRUE
  )
})


test_that("a single plant and repeated thresholds are supported", {

  priorities <- data.frame(
    plant_species = "A",
    IPR = 0.5
  )

  result <- ipr_accumulation(
    priorities,
    thresholds = c(0.8, 0.5, 0.8)
  )

  expect_equal(
    result$thresholds$threshold,
    c(0.8, 0.5, 0.8)
  )

  expect_equal(
    result$thresholds$k,
    c(1, 1, 1)
  )

  expect_equal(
    result$thresholds$achieved_cumulative_IPR,
    c(1, 1, 1)
  )
})
