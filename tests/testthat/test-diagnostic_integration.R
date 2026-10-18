diagnostic_integration_data <- function() {

  data.frame(
    plant_species = c(
      "A", "A", "A", "B",
      "B", "C", "C", "C"
    ),
    bee_species = c(
      "b1", "b1", "b2", "b2",
      "b3", "b1", "b3", "b3"
    ),
    sample_unit = c(
      "S1", "S2", "S2", "S1",
      "S2", "S1", "S2", "S3"
    )
  )
}


test_that("the workflow returns the three diagnostic results", {

  result <- restaurabee(
    diagnostic_integration_data(),
    network_indices = NULL
  )

  expect_named(
    result$diagnostics,
    c(
      "ipr_accumulation",
      "component_correlations",
      "component_omission"
    )
  )

  expect_true(result$settings$diagnostics)

  expect_equal(
    result$diagnostics$ipr_accumulation,
    ipr_accumulation(
      result$prioritization,
      thresholds = c(0.70, 0.80, 0.90),
      ties = "first"
    )
  )

  expect_equal(
    result$diagnostics$component_correlations,
    component_correlations(result$prioritization)
  )

  expect_equal(
    result$diagnostics$component_omission,
    component_omission(
      result$prioritization,
      weights = result$settings$weights,
      threshold = result$settings$threshold,
      ties = result$settings$ties,
      tie_break = "input"
    )
  )
})


test_that("diagnostics do not change the established results", {

  data <- diagnostic_integration_data()

  enabled <- restaurabee(
    data,
    network_indices = NULL,
    diagnostics = TRUE
  )

  disabled <- restaurabee(
    data,
    network_indices = NULL,
    diagnostics = FALSE
  )

  fields <- c(
    "metadata",
    "interaction_matrix",
    "IIP",
    "d_prime",
    "plant_metrics",
    "prioritization",
    "selected_species",
    "sensitivity",
    "network_metrics"
  )

  for (field in fields) {

    expect_identical(
      enabled[[field]],
      disabled[[field]]
    )
  }

  expect_null(disabled$diagnostics)

  expect_false(disabled$settings$diagnostics)
})


test_that("custom settings reach the diagnostic functions", {

  weights <- c(
    abundance = 0.20,
    specialization = 0.40,
    connectivity = 0.20,
    frequency = 0.20
  )

  result <- restaurabee(
    diagnostic_integration_data(),
    network_indices = NULL,
    weights = weights,
    threshold = 0.60,
    ties = "include"
  )

  expect_equal(
    result$diagnostics$ipr_accumulation$
      thresholds$threshold,
    c(0.60, 0.70, 0.80, 0.90)
  )

  expect_identical(
    result$diagnostics$ipr_accumulation$settings$ties,
    "include"
  )

  expect_equal(
    result$diagnostics$component_omission,
    component_omission(
      result$prioritization,
      weights = weights,
      threshold = 0.60,
      ties = "include",
      tie_break = "input"
    )
  )

  expect_identical(
    result$diagnostics$component_omission$
      settings$weights,
    weights
  )

  reference <- result$diagnostics$
    component_omission$results_table

  reference <- reference[
    reference$scenario == "reference",
    ,
    drop = FALSE
  ]

  positions <- match(
    result$prioritization$plant_species,
    reference$plant_species
  )

  expect_equal(
    reference$IPR[positions],
    result$prioritization$IPR
  )

  expect_setequal(
    reference$plant_species[reference$selected],
    result$selected_species$plant_species
  )
})


test_that("disabled diagnostics are not called", {

  testthat::local_mocked_bindings(
    ipr_accumulation = function(...) {
      stop("ipr_accumulation should not be called")
    },
    component_correlations = function(...) {
      stop("component_correlations should not be called")
    },
    component_omission = function(...) {
      stop("component_omission should not be called")
    },
    .package = "restaurabee"
  )

  result <- restaurabee(
    diagnostic_integration_data(),
    network_indices = NULL,
    diagnostics = FALSE
  )

  expect_null(result$diagnostics)

  expect_true(
    is.data.frame(result$prioritization)
  )

  expect_true(
    is.data.frame(result$selected_species)
  )
})


test_that("constant components remain identifiable in the workflow", {

  result <- restaurabee(
    diagnostic_integration_data(),
    network_indices = NULL
  )

  associations <- result$diagnostics$
    component_correlations

  expect_identical(
    associations$constant_components,
    "connectivity"
  )

  expect_true(
    all(is.na(
      associations$correlation_matrix["connectivity", ]
    ))
  )

  expect_true(
    all(is.na(
      associations$correlation_matrix[, "connectivity"]
    ))
  )
})


test_that("omitting a sole positive weight does not stop the workflow", {

  result <- restaurabee(
    diagnostic_integration_data(),
    network_indices = NULL,
    weights = c(
      abundance = 1,
      specialization = 0,
      connectivity = 0,
      frequency = 0
    )
  )

  omission <- result$diagnostics$
    component_omission$summary

  row <- omission$scenario == "without_abundance"

  expect_identical(
    omission$status[row],
    "no_remaining_weight"
  )

  expect_true(
    is.na(omission$n_selected[row])
  )

  expect_true(
    is.data.frame(result$selected_species)
  )
})


test_that("diagnostics do not change the random-number state", {

  withr::local_preserve_seed()

  set.seed(99)

  state_before <- .Random.seed

  result <- restaurabee(
    diagnostic_integration_data(),
    network_indices = NULL,
    diagnostics = TRUE
  )

  expect_identical(
    .Random.seed,
    state_before
  )

  expect_true(
    is.list(result$diagnostics)
  )
})


test_that("invalid diagnostic options are rejected", {

  invalid <- list(
    1,
    0,
    NA,
    "TRUE",
    logical(0),
    c(TRUE, FALSE),
    matrix(TRUE),
    list(TRUE)
  )

  for (value in invalid) {

    expect_error(
      restaurabee(
        diagnostic_integration_data(),
        network_indices = NULL,
        diagnostics = value
      ),
      "`diagnostics` must be TRUE or FALSE.",
      fixed = TRUE
    )
  }
})
