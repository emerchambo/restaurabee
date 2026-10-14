bootstrap_test_data <- function() {

  one_unit <- data.frame(
    plant_species = c(
      "A", "A", "A", "A",
      "B", "B",
      "C", "C", "C"
    ),
    bee_species = c(
      "b1", "b1", "b1", "b2",
      "b2", "b3",
      "b1", "b3", "b3"
    ),
    stringsAsFactors = FALSE
  )

  units <- lapply(
    seq_len(4),
    function(i) {
      data <- one_unit
      data$sample_unit <- paste0("S", i)
      data$site <- if (i <= 2) "Site_1" else "Site_2"
      data
    }
  )

  result <- do.call(rbind, units)
  rownames(result) <- NULL
  result
}

test_that("bootstrap returns consistent counts and frequencies", {

  result <- bootstrap_priority(
    bootstrap_test_data(),
    n_boot = 10,
    seed = 123
  )

  expect_equal(result$diagnostics$n_requested, 10)
  expect_equal(result$diagnostics$n_successful, 10)
  expect_equal(result$diagnostics$n_failed, 0)
  expect_equal(result$diagnostics$n_metrics_available, 10)
  expect_equal(result$diagnostics$n_calculation_failed, 0)
  expect_equal(result$diagnostics$n_selection_undefined, 0)
  expect_equal(nrow(result$diagnostics$failures), 0)

  expect_setequal(
    result$summary$plant_species,
    c("A", "B", "C")
  )

  expect_equal(result$summary$n_present, rep(10, 3))
  expect_equal(result$summary$presence_frequency, rep(1, 3))

  expect_equal(
    result$summary$selection_frequency,
    result$summary$n_selected / 10
  )

  expect_equal(
    result$summary$selection_given_presence,
    result$summary$n_selected / result$summary$n_present
  )

  expect_true(all(
    result$summary$IPR_lower <= result$summary$IPR_upper
  ))

  expect_true(all(
    result$summary$rank_lower <= result$summary$rank_upper
  ))

  expect_equal(nrow(result$replicates), 30)
  expect_false(anyNA(result$replicates$selected))
})

test_that("a seed reproduces results and restores the RNG state", {

  had_seed <- exists(
    ".Random.seed",
    envir = globalenv(),
    inherits = FALSE
  )

  if (had_seed) {
    previous_seed <- get(".Random.seed", envir = globalenv())
  }

  withr::defer({
    if (had_seed) {
      assign(".Random.seed", previous_seed, envir = globalenv())
    } else if (exists(
      ".Random.seed",
      envir = globalenv(),
      inherits = FALSE
    )) {
      rm(".Random.seed", envir = globalenv())
    }
  })

  set.seed(99)
  state_before <- .Random.seed

  first <- bootstrap_priority(
    bootstrap_test_data(),
    n_boot = 5,
    seed = 123
  )

  expect_identical(.Random.seed, state_before)

  second <- bootstrap_priority(
    bootstrap_test_data(),
    n_boot = 5,
    seed = 123
  )

  expect_identical(first, second)
  expect_identical(.Random.seed, state_before)
})

test_that("resampling groups retain their sampling units and records", {

  observed <- list()
  original_IIP <- calculate_IIP

  testthat::local_mocked_bindings(
    calculate_IIP = function(interaction_data) {
      observed[[length(observed) + 1L]] <<- interaction_data
      original_IIP(interaction_data)
    },
    .package = "restaurabee"
  )

  result <- bootstrap_priority(
    bootstrap_test_data(),
    resample_unit = "site",
    n_boot = 5,
    seed = 123
  )

  expect_equal(result$settings$n_units, 2)
  expect_identical(result$settings$resample_unit, "site")
  expect_length(observed, 5)

  for (data in observed) {
    expect_equal(nrow(data), 36)
    expect_equal(length(unique(data$sample_unit)), 4)

    expect_equal(
      sort(as.integer(table(data$sample_unit))),
      rep(9L, 4)
    )

    draw_id <- sub("_unit_.*$", "", data$sample_unit)

    expect_equal(length(unique(draw_id)), 2)

    # Every drawn group comes from only one original site.
    sites_per_draw <- vapply(
      split(data$site, draw_id),
      function(x) length(unique(x)),
      integer(1)
    )

    expect_true(all(sites_per_draw == 1L))
  }
})

test_that("custom weights and selection settings reach the calculations", {

  received_weights <- list()
  received_selection <- list()

  original_IPR <- calculate_IPR
  original_selection <- select_species

  testthat::local_mocked_bindings(
    calculate_IPR = function(
    plant_metrics,
    weight_abundance,
    weight_specialization,
    weight_connectivity,
    weight_frequency
    ) {
      received_weights[[length(received_weights) + 1L]] <<- c(
        abundance = weight_abundance,
        specialization = weight_specialization,
        connectivity = weight_connectivity,
        frequency = weight_frequency
      )

      original_IPR(
        plant_metrics,
        weight_abundance = weight_abundance,
        weight_specialization = weight_specialization,
        weight_connectivity = weight_connectivity,
        weight_frequency = weight_frequency
      )
    },
    select_species = function(prioritization, threshold, ties) {
      received_selection[[length(received_selection) + 1L]] <<-
        list(threshold = threshold, ties = ties)

      original_selection(
        prioritization,
        threshold = threshold,
        ties = ties
      )
    },
    .package = "restaurabee"
  )

  weights <- c(
    abundance = 0.20,
    specialization = 0.40,
    connectivity = 0.20,
    frequency = 0.20
  )

  result <- bootstrap_priority(
    bootstrap_test_data(),
    n_boot = 3,
    weights = weights,
    threshold = 0.70,
    ties = "include",
    seed = 123
  )

  expect_length(received_weights, 3)
  expect_length(received_selection, 3)

  for (value in received_weights) {
    expect_equal(value, weights)
  }

  for (value in received_selection) {
    expect_identical(
      value,
      list(threshold = 0.70, ties = "include")
    )
  }

  expect_equal(result$settings$weights, weights)
  expect_equal(result$settings$threshold, 0.70)
  expect_identical(result$settings$ties, "include")
})

test_that("calculation errors and zero totals are reported separately", {

  call_number <- 0L
  original_IPR <- calculate_IPR

  testthat::local_mocked_bindings(
    calculate_IPR = function(plant_metrics, ...) {

      call_number <<- call_number + 1L

      if (call_number == 1L) {
        stop("simulated calculation error")
      }

      result <- original_IPR(plant_metrics, ...)

      if (call_number == 2L) {
        result$IPR <- 0
      }

      result
    },
    .package = "restaurabee"
  )

  expect_warning(
    result <- bootstrap_priority(
      bootstrap_test_data(),
      n_boot = 3,
      seed = 123
    ),
    "1 replicate\\(s\\) had calculation errors; 1 replicate\\(s\\)"
  )

  expect_equal(result$diagnostics$n_successful, 1)
  expect_equal(result$diagnostics$n_failed, 2)
  expect_equal(result$diagnostics$n_metrics_available, 2)
  expect_equal(result$diagnostics$n_calculation_failed, 1)
  expect_equal(result$diagnostics$n_selection_undefined, 1)

  expect_identical(
    result$diagnostics$failures$type,
    c("calculation_failed", "selection_undefined")
  )

  expect_match(
    result$diagnostics$failures$message[1],
    "simulated calculation error"
  )

  zero_result <- subset(result$replicates, replicate == 2)

  expect_equal(zero_result$IPR, rep(0, 3))
  expect_equal(zero_result$rank, rep(2, 3))
  expect_true(all(is.na(zero_result$selected)))

  expect_equal(result$summary$n_present, rep(1, 3))
  expect_equal(
    result$summary$selection_frequency,
    as.numeric(result$summary$n_selected)
  )
})

test_that("all zero totals retain results and return undefined summaries", {

  original_IPR <- calculate_IPR

  testthat::local_mocked_bindings(
    calculate_IPR = function(plant_metrics, ...) {
      result <- original_IPR(plant_metrics, ...)
      result$IPR <- 0
      result
    },
    .package = "restaurabee"
  )

  expect_warning(
    result <- bootstrap_priority(
      bootstrap_test_data(),
      n_boot = 3,
      seed = 123
    ),
    "Selection summaries use 0 of 3"
  )

  expect_equal(result$diagnostics$n_successful, 0)
  expect_equal(result$diagnostics$n_metrics_available, 3)
  expect_equal(result$diagnostics$n_selection_undefined, 3)
  expect_equal(result$diagnostics$n_calculation_failed, 0)

  expect_equal(nrow(result$replicates), 9)
  expect_true(all(is.na(result$replicates$selected)))
  expect_true(all(is.na(result$summary$presence_frequency)))
  expect_true(all(is.na(result$summary$selection_frequency)))
  expect_true(all(is.na(result$summary$selection_given_presence)))
  expect_true(all(is.na(result$summary$IPR_mean)))
  expect_true(all(is.na(result$summary$rank_median)))
})

test_that("the function stops when every calculation fails", {

  testthat::local_mocked_bindings(
    calculate_d_prime = function(...) {
      stop("simulated specialization error")
    },
    .package = "restaurabee"
  )

  expect_error(
    bootstrap_priority(
      bootstrap_test_data(),
      n_boot = 3,
      seed = 123
    ),
    "All bootstrap metric calculations failed.*simulated specialization error"
  )
})

test_that("absent plants count as not selected in defined replicates", {

  data <- data.frame(
    plant_species = c("A", "B"),
    bee_species = c("b1", "b2"),
    sample_unit = c("S1", "S2"),
    stringsAsFactors = FALSE
  )

  # These replacements isolate the frequency calculation.
  testthat::local_mocked_bindings(
    calculate_d_prime = function(interaction_matrix) {
      list(
        plant_dprime = data.frame(
          plant_species = rownames(interaction_matrix),
          d_prime = 0.5,
          stringsAsFactors = FALSE
        )
      )
    },
    calculate_IPR = function(plant_metrics, ...) {
      plant_metrics$IPR <- 1
      plant_metrics
    },
    .package = "restaurabee"
  )

  result <- bootstrap_priority(
    data,
    n_boot = 30,
    threshold = 1,
    seed = 123
  )

  expect_equal(result$diagnostics$n_successful, 30)
  expect_true(any(result$summary$n_present < 30))

  expect_equal(
    result$summary$presence_frequency,
    result$summary$n_present / 30
  )

  expect_equal(
    result$summary$selection_frequency,
    result$summary$presence_frequency
  )

  expect_equal(
    result$summary$selection_given_presence,
    rep(1, 2)
  )
})

test_that("invalid bootstrap settings are rejected", {

  data <- bootstrap_test_data()

  for (value in list(
    0, -1, 1.5, NA_real_, Inf, "10", TRUE, c(1, 2)
  )) {
    expect_error(
      bootstrap_priority(data, n_boot = value),
      "positive integer"
    )
  }

  for (value in list(
    0, 1, -1, NA_real_, Inf, "0.95", c(0.9, 0.95)
  )) {
    expect_error(
      bootstrap_priority(data, conf = value),
      "`conf`"
    )
  }

  for (value in list(
    0, -1, 1.1, NA_real_, Inf, "0.8", c(0.5, 0.8)
  )) {
    expect_error(
      bootstrap_priority(data, threshold = value),
      "`threshold`"
    )
  }

  for (value in list(
    -1, 1.5, NA_real_, Inf, "1", TRUE, c(1, 2)
  )) {
    expect_error(
      bootstrap_priority(data, seed = value),
      "non-negative integer"
    )
  }

  expect_error(
    bootstrap_priority(data, ties = "unknown"),
    "arg"
  )

  expect_error(
    bootstrap_priority(data, weights = c(1, 0, 0, 0))
  )
})

test_that("resampling identifiers and group membership are validated", {

  data <- bootstrap_test_data()

  expect_error(
    bootstrap_priority(data, resample_unit = "unknown"),
    "Resampling column not found"
  )

  for (value in list(
    NA_character_, "", character(0), c("site", "sample_unit"), 1
  )) {
    expect_error(
      bootstrap_priority(data, resample_unit = value),
      "single column name"
    )
  }

  invalid <- data
  invalid$site[1] <- NA_character_

  expect_error(
    bootstrap_priority(invalid, resample_unit = "site"),
    "missing or infinite values"
  )

  invalid <- data
  invalid$site[1] <- " "

  expect_error(
    bootstrap_priority(invalid, resample_unit = "site"),
    "non-empty identifiers"
  )

  invalid <- data
  invalid$site[1] <- " Site_1 "

  expect_error(
    bootstrap_priority(invalid, resample_unit = "site"),
    "leading or trailing whitespace"
  )

  invalid <- data
  invalid$site <- "Site_1"

  expect_error(
    bootstrap_priority(invalid, resample_unit = "site"),
    "At least two resampling units"
  )

  invalid <- data
  invalid$site[1] <- "Site_2"

  expect_error(
    bootstrap_priority(invalid, resample_unit = "site"),
    "must belong to only one resampling group"
  )
})
