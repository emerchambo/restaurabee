component_test_data <- function() {

  data.frame(
    plant_species = c("A", "B", "C", "D"),
    abundance_pad = c(1, 0.7, 0.2, 0),
    d_prime_pad = c(0.1, 0.8, 0.2, 1),
    connectivity_pad = c(0, 0.5, 1, 0.2),
    frequency_pad = c(1, 0.6, 0.1, 0)
  )
}


test_that("correlations match the direct Spearman calculation", {

  data <- component_test_data()

  columns <- c(
    "abundance_pad",
    "d_prime_pad",
    "connectivity_pad",
    "frequency_pad"
  )

  reference <- stats::cor(
    data[, columns],
    method = "spearman"
  )

  result <- component_correlations(data)

  expect_equal(
    unname(result$correlation_matrix),
    unname(reference)
  )

  expect_identical(
    rownames(result$correlation_matrix),
    c(
      "abundance",
      "specialization",
      "connectivity",
      "frequency"
    )
  )

  expect_identical(
    colnames(result$correlation_matrix),
    rownames(result$correlation_matrix)
  )

  expect_equal(nrow(result$pairwise), 6)

  expect_true(
    all(result$pairwise$status == "defined")
  )

  expect_length(result$constant_components, 0)

  expect_equal(result$n_plants, 4)

  for (i in seq_len(nrow(result$pairwise))) {

    expect_equal(
      result$pairwise$spearman[i],
      result$correlation_matrix[
        result$pairwise$component_1[i],
        result$pairwise$component_2[i]
      ]
    )
  }
})


test_that("tied values are handled using Spearman correlations", {

  data <- component_test_data()

  data$abundance_pad <- c(1, 1, 0, 0)
  data$frequency_pad <- c(0, 0.5, 0.5, 1)

  result <- component_correlations(data)

  reference <- stats::cor(
    data$abundance_pad,
    data$frequency_pad,
    method = "spearman"
  )

  expect_equal(
    result$correlation_matrix["abundance", "frequency"],
    reference
  )
})


test_that("constant components have undefined correlations", {

  data <- component_test_data()
  data$frequency_pad <- 0

  expect_warning(
    result <- component_correlations(data),
    NA
  )

  expect_identical(
    result$constant_components,
    "frequency"
  )

  expect_true(
    all(is.na(result$correlation_matrix["frequency", ]))
  )

  expect_true(
    all(is.na(result$correlation_matrix[, "frequency"]))
  )

  expect_equal(
    sum(result$pairwise$status == "constant_component"),
    3
  )

  expect_equal(
    sum(result$pairwise$status == "defined"),
    3
  )

  expect_true(
    is.finite(
      result$correlation_matrix["abundance", "specialization"]
    )
  )
})


test_that("a single varying component retains its diagonal correlation", {

  data <- component_test_data()

  data$d_prime_pad <- 0
  data$connectivity_pad <- 0
  data$frequency_pad <- 0

  result <- component_correlations(data)

  expect_equal(
    result$correlation_matrix["abundance", "abundance"],
    1
  )

  expect_equal(
    sum(is.finite(result$correlation_matrix)),
    1
  )

  expect_length(result$constant_components, 3)

  expect_true(
    all(result$pairwise$status == "constant_component")
  )
})


test_that("all constant components are reported without warnings", {

  data <- component_test_data()

  for (column in c(
    "abundance_pad",
    "d_prime_pad",
    "connectivity_pad",
    "frequency_pad"
  )) {
    data[[column]] <- 0
  }

  expect_warning(
    result <- component_correlations(data),
    NA
  )

  expect_true(
    all(is.na(result$correlation_matrix))
  )

  expect_length(result$constant_components, 4)

  expect_true(
    all(result$pairwise$status == "constant_component")
  )
})


test_that("one plant gives undefined correlations", {

  data <- component_test_data()[1, , drop = FALSE]

  expect_warning(
    result <- component_correlations(data),
    NA
  )

  expect_equal(result$n_plants, 1)

  expect_true(
    all(is.na(result$correlation_matrix))
  )

  expect_true(
    all(result$pairwise$status == "insufficient_plants")
  )
})


test_that("row order does not change correlations or modify the input", {

  data <- component_test_data()
  original <- data

  first <- component_correlations(data)

  reordered <- component_correlations(
    data[c(4, 2, 1, 3), , drop = FALSE]
  )

  expect_identical(data, original)

  expect_equal(
    reordered$correlation_matrix,
    first$correlation_matrix
  )

  expect_equal(
    reordered$pairwise,
    first$pairwise
  )
})


test_that("invalid input tables are rejected", {

  expect_error(
    component_correlations(NULL),
    "data frame"
  )

  expect_error(
    component_correlations(data.frame(IPR = 1)),
    "Missing required columns"
  )

  data <- component_test_data()

  expect_error(
    component_correlations(data[0, , drop = FALSE]),
    "at least one row"
  )

  duplicated <- data
  names(duplicated)[2] <- names(duplicated)[3]

  expect_error(
    component_correlations(duplicated),
    "unique column names"
  )
})


test_that("invalid component values are rejected", {

  invalid <- list(
    "a",
    TRUE,
    NA_real_,
    NaN,
    Inf,
    -Inf,
    -0.1
  )

  for (value in invalid) {

    data <- component_test_data()
    data$abundance_pad <- rep(value, nrow(data))

    expect_error(
      component_correlations(data),
      "finite, non-negative numeric values"
    )
  }

  data <- component_test_data()
  data$abundance_pad[1] <- 1.1

  expect_error(
    component_correlations(data),
    "between 0 and 1"
  )
})
