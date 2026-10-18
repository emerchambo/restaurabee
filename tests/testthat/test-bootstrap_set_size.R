set_size_test_data <- function() {

  one_unit <- data.frame(
    plant_species = c(
      "A", "A", "A",
      "B", "B",
      "C", "C"
    ),
    bee_species = c(
      "b1", "b1", "b2",
      "b2", "b3",
      "b1", "b3"
    ),
    stringsAsFactors = FALSE
  )

  do.call(
    rbind,
    lapply(
      seq_len(3),
      function(i) {

        data <- one_unit
        data$sample_unit <- paste0("S", i)

        data
      }
    )
  )
}


test_that("set sizes agree with the existing replicate selections", {

  result <- bootstrap_priority(
    set_size_test_data(),
    n_boot = 8,
    seed = 123
  )

  expected <- vapply(
    seq_len(8),
    function(i) {

      rows <- result$replicates[
        result$replicates$replicate == i,
        ,
        drop = FALSE
      ]

      as.integer(sum(rows$selected))
    },
    integer(1)
  )

  expect_identical(
    result$set_size$replicates$replicate,
    seq_len(8)
  )

  expect_identical(
    result$set_size$replicates$k,
    expected
  )

  expect_true(
    all(result$set_size$replicates$status == "defined")
  )

  expect_equal(
    result$set_size$summary$n_defined,
    result$diagnostics$n_successful
  )

  expect_equal(
    result$set_size$summary$n_excluded,
    result$diagnostics$n_failed
  )

  expect_equal(
    result$set_size$summary$k_mean,
    mean(expected)
  )

  expect_equal(
    sum(result$set_size$frequencies$n_replicates),
    8
  )

  expect_equal(
    sum(result$set_size$frequencies$frequency),
    1
  )
})


test_that("frequency and summary values match known set sizes", {

  sizes <- c(1L, 2L, 2L, 3L)

  replicates <- lapply(
    sizes,
    function(k) {
      data.frame(selected = rep(TRUE, k))
    }
  )

  result <- summarize_bootstrap_set_size(
    replicates,
    conf = 0.50
  )

  expect_identical(
    result$replicates$k,
    sizes
  )

  expect_equal(
    result$frequencies$k,
    c(1, 2, 3)
  )

  expect_equal(
    result$frequencies$n_replicates,
    c(1, 2, 1)
  )

  expect_equal(
    result$frequencies$frequency,
    c(0.25, 0.50, 0.25)
  )

  expect_equal(result$summary$n_requested, 4)
  expect_equal(result$summary$n_defined, 4)
  expect_equal(result$summary$n_excluded, 0)

  expect_equal(result$summary$k_mean, 2)
  expect_equal(result$summary$k_sd, sqrt(2 / 3))
  expect_equal(result$summary$k_median, 2)
  expect_equal(result$summary$k_min, 1)
  expect_equal(result$summary$k_max, 3)

  expect_equal(result$summary$k_lower, 1.75)
  expect_equal(result$summary$k_upper, 2.25)
})


test_that("excluded replicates retain their statuses and undefined sizes", {

  replicates <- list(
    NULL,
    data.frame(selected = c(NA, NA)),
    data.frame(selected = c(TRUE, FALSE, TRUE))
  )

  result <- summarize_bootstrap_set_size(
    replicates,
    conf = 0.95
  )

  expect_identical(
    result$replicates$status,
    c(
      "calculation_failed",
      "selection_undefined",
      "defined"
    )
  )

  expect_identical(
    result$replicates$k,
    c(NA_integer_, NA_integer_, 2L)
  )

  expect_equal(result$summary$n_requested, 3)
  expect_equal(result$summary$n_defined, 1)
  expect_equal(result$summary$n_excluded, 2)

  expect_equal(result$summary$k_mean, 2)
  expect_equal(result$summary$k_median, 2)
  expect_equal(result$summary$k_lower, 2)
  expect_equal(result$summary$k_upper, 2)

  expect_true(is.na(result$summary$k_sd))

  expect_equal(
    result$frequencies$n_replicates,
    1
  )

  expect_equal(
    result$frequencies$frequency,
    1
  )
})


test_that("no defined selections produce undefined summaries", {

  replicates <- list(
    NULL,
    data.frame(selected = c(NA, NA))
  )

  result <- summarize_bootstrap_set_size(
    replicates,
    conf = 0.95
  )

  expect_equal(result$summary$n_requested, 2)
  expect_equal(result$summary$n_defined, 0)
  expect_equal(result$summary$n_excluded, 2)

  summary_columns <- c(
    "k_mean",
    "k_sd",
    "k_median",
    "k_min",
    "k_lower",
    "k_upper",
    "k_max"
  )

  expect_true(
    all(is.na(
      unlist(result$summary[, summary_columns])
    ))
  )

  expect_equal(
    nrow(result$frequencies),
    0
  )

  expect_equal(
    nrow(result$replicates),
    2
  )

  expect_true(
    all(is.na(result$replicates$k))
  )
})


test_that("summarizing existing draws does not change the RNG state", {

  withr::local_preserve_seed()

  set.seed(99)

  state_before <- .Random.seed

  replicates <- list(
    data.frame(selected = c(TRUE, FALSE)),
    data.frame(selected = c(TRUE, TRUE))
  )

  summarize_bootstrap_set_size(
    replicates,
    conf = 0.95
  )

  expect_identical(
    .Random.seed,
    state_before
  )
})


test_that("bootstrap passes the requested interval level to set sizes", {

  result <- bootstrap_priority(
    set_size_test_data(),
    n_boot = 8,
    conf = 0.80,
    ties = "include",
    seed = 123
  )

  sizes <- result$set_size$replicates$k[
    result$set_size$replicates$status == "defined"
  ]

  reference <- stats::quantile(
    sizes,
    probs = c(0.10, 0.90),
    names = FALSE,
    type = 7
  )

  expect_equal(
    result$set_size$summary$k_lower,
    reference[1]
  )

  expect_equal(
    result$set_size$summary$k_upper,
    reference[2]
  )

  expect_equal(result$settings$conf, 0.80)

  expect_identical(
    result$settings$ties,
    "include"
  )
})


test_that("bootstrap reports calculation failures and undefined selections", {

  original_IPR <- calculate_IPR
  call_number <- 0L

  testthat::local_mocked_bindings(
    calculate_IPR = function(plant_metrics, ...) {

      call_number <<- call_number + 1L

      if (call_number == 1L) {
        stop("simulated calculation error")
      }

      result <- original_IPR(
        plant_metrics,
        ...
      )

      if (call_number == 2L) {
        result$IPR <- 0
      }

      result
    },
    .package = "restaurabee"
  )

  expect_warning(
    result <- bootstrap_priority(
      set_size_test_data(),
      n_boot = 3,
      seed = 123
    ),
    "Selection summaries use 1 of 3"
  )

  expect_identical(
    result$set_size$replicates$status,
    c(
      "calculation_failed",
      "selection_undefined",
      "defined"
    )
  )

  expect_true(
    all(is.na(result$set_size$replicates$k[1:2]))
  )

  expect_equal(
    result$set_size$summary$n_defined,
    result$diagnostics$n_successful
  )

  expect_equal(
    result$set_size$summary$n_excluded,
    result$diagnostics$n_failed
  )

  expect_equal(
    sum(result$set_size$frequencies$n_replicates),
    1
  )
})
