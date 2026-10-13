ipr_test_metrics <- function() {
  data.frame(
    plant_species = c("A", "B", "C"),
    abundance_relative = c(0.6, 0.3, 0.1),
    frequency_relative = c(0.5, 0.3, 0.2),
    connectivity_relative = c(0.5, 0.25, 0.25),
    d_prime = c(0.2, 0.5, 0.8)
  )
}

test_that("standardized components and IPR match manual calculations", {
  resultado <- calculate_IPR(ipr_test_metrics())

  expect_identical(resultado$plant_species, c("A", "B", "C"))
  expect_equal(resultado$abundance_pad, c(1, 2 / 5, 0))
  expect_equal(resultado$frequency_pad, c(1, 1 / 3, 0))
  expect_equal(resultado$connectivity_pad, c(1, 0, 0))
  expect_equal(resultado$d_prime_pad, c(0, 1 / 2, 1))
  expect_equal(resultado$IPR, c(3 / 4, 37 / 120, 1 / 4))
})

test_that("custom weights are applied and positional arguments remain valid", {
  resultado <- calculate_IPR(
    ipr_test_metrics(),
    weight_abundance = 0,
    weight_specialization = 1,
    weight_connectivity = 0,
    weight_frequency = 0
  )

  expect_identical(resultado$plant_species, c("C", "B", "A"))
  expect_equal(resultado$IPR, c(1, 1 / 2, 0))

  expect_equal(
    resultado,
    calculate_IPR(ipr_test_metrics(), 0, 1, 0, 0)
  )
})

test_that("negative and incorrectly summed weights are rejected", {
  metricas <- ipr_test_metrics()

  expect_error(
    calculate_IPR(metricas, 1.5, -0.5, 0, 0),
    "non-negative"
  )

  expect_error(
    calculate_IPR(metricas, 0.5, 0.5, 0.5, 0.5),
    "sum to 1"
  )
})

test_that("each weight must be a finite numeric scalar", {
  argumentos <- c(
    "weight_abundance",
    "weight_specialization",
    "weight_connectivity",
    "weight_frequency"
  )

  invalidos <- list(
    NA_real_,
    NaN,
    Inf,
    -Inf,
    "0.25",
    TRUE,
    numeric(0),
    c(0.25, 0.25),
    matrix(0.25, nrow = 1)
  )

  for (argumento in argumentos) {
    for (valor in invalidos) {
      chamada <- list(plant_metrics = ipr_test_metrics())
      chamada[argumento] <- list(valor)

      expect_error(
        do.call(calculate_IPR, chamada),
        "finite numeric scalar"
      )
    }
  }
})

test_that("invalid metric tables are rejected", {
  metricas <- ipr_test_metrics()

  expect_error(calculate_IPR(NULL), "data frame")
  expect_error(calculate_IPR(metricas[0, ]), "at least one row")
  expect_error(calculate_IPR(metricas[, -2]), "Missing required columns")

  names(metricas)[3] <- names(metricas)[2]
  expect_error(calculate_IPR(metricas), "unique column names")
})

test_that("every metric must be numeric, finite and between zero and one", {
  colunas <- c(
    "abundance_relative",
    "frequency_relative",
    "connectivity_relative",
    "d_prime"
  )

  for (coluna in colunas) {
    metricas <- ipr_test_metrics()
    metricas[[coluna]] <- as.character(metricas[[coluna]])
    expect_error(calculate_IPR(metricas), "must be numeric")

    for (valor in c(NA_real_, NaN, Inf, -Inf)) {
      metricas <- ipr_test_metrics()
      metricas[[coluna]][1] <- valor

      expect_error(calculate_IPR(metricas), "infinite values")
    }

    for (valor in c(-0.1, 1.1)) {
      metricas <- ipr_test_metrics()
      metricas[[coluna]][1] <- valor

      expect_error(calculate_IPR(metricas), "between 0 and 1")
    }
  }
})

test_that("constant components receive zero without redistributing weights", {
  metricas <- ipr_test_metrics()
  metricas$d_prime <- rep(0.5, 3)

  resultado <- calculate_IPR(metricas)

  expect_equal(resultado$d_prime_pad, c(0, 0, 0))
  expect_equal(resultado$IPR, c(3 / 4, 11 / 60, 0))

  metricas[, 2:5] <- 0.5
  resultado <- calculate_IPR(metricas)

  expect_equal(resultado$IPR, c(0, 0, 0))
})

test_that("pre-existing grouping does not change standardization", {
  metricas <- ipr_test_metrics()

  agrupadas <- dplyr::group_by(metricas, plant_species)

  expect_equal(
    calculate_IPR(agrupadas),
    calculate_IPR(metricas)
  )
})

test_that("metric tables without species identifiers remain supported", {
  metricas <- ipr_test_metrics()

  expect_equal(
    calculate_IPR(metricas[, -1])$IPR,
    calculate_IPR(metricas)$IPR
  )
})
