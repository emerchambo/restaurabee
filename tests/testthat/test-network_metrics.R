network_test_data <- function() {
  data.frame(
    plant_species = c("A", "A", "A", "B", "B", "C", "C", "C"),
    bee_species = c("b1", "b1", "b2", "b2", "b3", "b1", "b3", "b3"),
    sample_unit = c("S1", "S2", "S2", "S1", "S2", "S1", "S2", "S3")
  )
}

test_that("default network metrics are explicit and reproducible", {
  dados <- network_test_data()

  primeiro <- restaurabee(dados)
  segundo <- restaurabee(dados)

  expect_setequal(
    names(primeiro$network_metrics),
    c(
      "connectance",
      "H2",
      "weighted NODF",
      "linkage density",
      "web asymmetry"
    )
  )

  expect_identical(
    primeiro$network_metrics,
    segundo$network_metrics
  )

  referencia <- bipartite::networklevel(
    primeiro$interaction_matrix,
    index = c(
      "connectance",
      "H2",
      "weighted NODF",
      "linkage density",
      "web asymmetry"
    )
  )

  expect_equal(primeiro$network_metrics, referencia)
})

test_that("requested indices are used without changing priorities", {
  dados <- network_test_data()

  padrao <- restaurabee(dados)

  personalizado <- restaurabee(
    dados,
    network_indices = c("connectance", "H2")
  )

  expect_setequal(
    names(personalizado$network_metrics),
    c("connectance", "H2")
  )

  expect_equal(personalizado$IIP, padrao$IIP)
  expect_equal(personalizado$d_prime, padrao$d_prime)
  expect_equal(personalizado$prioritization, padrao$prioritization)
  expect_equal(personalizado$selected_species, padrao$selected_species)
})

test_that("network metrics can be skipped", {
  testthat::local_mocked_bindings(
    networklevel = function(...) {
      stop("networklevel should not be called")
    },
    .package = "bipartite"
  )

  resultado <- restaurabee(
    network_test_data(),
    network_indices = NULL
  )

  expect_null(resultado$network_metrics)
  expect_true(is.data.frame(resultado$prioritization))
})

test_that("invalid network index specifications are rejected", {
  invalidos <- list(
    1, TRUE, character(0), NA_character_, "", " ",
    matrix("H2", nrow = 1)
  )

  for (valor in invalidos) {
    expect_error(
      restaurabee(network_test_data(), network_indices = valor),
      "non-empty character vector"
    )
  }
})

test_that("invalid seeds are rejected", {
  invalidos <- list(
    NA_real_, NaN, Inf, -Inf, -1, 1.5,
    "1", TRUE, numeric(0), c(1, 2),
    matrix(1, nrow = 1),
    as.double(.Machine$integer.max) + 1
  )

  for (valor in invalidos) {
    expect_error(
      restaurabee(network_test_data(), seed = valor),
      "non-negative integer"
    )
  }
})

test_that("seeded random calculations reproduce results and restore RNG state", {
  testthat::local_mocked_bindings(
    networklevel = function(...) {
      c(simulated_metric = stats::runif(1))
    },
    .package = "bipartite"
  )

  had_seed <- exists(
    ".Random.seed",
    envir = globalenv(),
    inherits = FALSE
  )

  if (had_seed) {
    saved_seed <- get(".Random.seed", envir = globalenv())
  }

  withr::defer({
    if (had_seed) {
      assign(".Random.seed", saved_seed, envir = globalenv())
    } else if (exists(
      ".Random.seed",
      envir = globalenv(),
      inherits = FALSE
    )) {
      rm(".Random.seed", envir = globalenv())
    }
  })

  set.seed(99)
  estado_anterior <- .Random.seed

  primeiro <- restaurabee(network_test_data(), seed = 123)

  expect_identical(.Random.seed, estado_anterior)

  segundo <- restaurabee(network_test_data(), seed = 123)

  expect_identical(
    primeiro$network_metrics,
    segundo$network_metrics
  )

  expect_identical(.Random.seed, estado_anterior)
})

test_that("RNG state is restored even when network metrics fail", {
  testthat::local_mocked_bindings(
    networklevel = function(...) {
      stats::runif(1)
      stop("simulated network failure")
    },
    .package = "bipartite"
  )

  had_seed <- exists(
    ".Random.seed",
    envir = globalenv(),
    inherits = FALSE
  )

  if (had_seed) {
    saved_seed <- get(".Random.seed", envir = globalenv())
  }

  withr::defer({
    if (had_seed) {
      assign(".Random.seed", saved_seed, envir = globalenv())
    } else if (exists(
      ".Random.seed",
      envir = globalenv(),
      inherits = FALSE
    )) {
      rm(".Random.seed", envir = globalenv())
    }
  })

  set.seed(99)
  estado_anterior <- .Random.seed

  expect_warning(
    resultado <- restaurabee(network_test_data(), seed = 123),
    "Network-level metrics failed: simulated network failure"
  )

  expect_null(resultado$network_metrics)
  expect_identical(.Random.seed, estado_anterior)
})
