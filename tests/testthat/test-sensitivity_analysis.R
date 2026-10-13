sensitivity_test_data <- function() {
  data.frame(
    plant_species = c("A", "B", "C"),
    abundance_pad = c(1, 0.4, 0),
    d_prime_pad = c(0, 0.5, 1),
    connectivity_pad = c(1, 0, 0),
    frequency_pad = c(1, 1 / 3, 0)
  )
}

test_that("scenario scores match manual calculations", {
  resultado <- sensitivity_analysis(sensitivity_test_data())
  tabela <- resultado$results_table

  expect_equal(tabela$IPR_equal, c(3 / 4, 37 / 120, 1 / 4))
  expect_equal(tabela$IPR_abundance, c(0.8, 49 / 150, 0.2))
  expect_equal(tabela$IPR_specialization, c(0.6, 26 / 75, 0.4))
  expect_equal(tabela$IPR_connectivity, c(0.8, 37 / 150, 0.2))
  expect_equal(tabela$IPR_frequency, c(0.8, 47 / 150, 0.2))

  expect_equal(tabela$rank_equal, c(1L, 2L, 3L))
  expect_equal(tabela$rank_specialization, c(1L, 3L, 2L))
})

test_that("Spearman correlations match the expected rankings", {
  correlacoes <- sensitivity_analysis(
    sensitivity_test_data()
  )$correlation_matrix

  expect_equal(dim(correlacoes), c(5L, 5L))
  expect_equal(unname(diag(correlacoes)), rep(1, 5))
  expect_equal(correlacoes, t(correlacoes))

  expect_equal(
    unname(correlacoes["rank_equal", "rank_specialization"]),
    0.5
  )

  expect_equal(
    unname(correlacoes["rank_equal", "rank_abundance"]),
    1
  )
})

test_that("constant rankings produce NA correlations and a warning", {
  dados <- sensitivity_test_data()
  dados[, 2:5] <- 0.5

  expect_warning(
    resultado <- sensitivity_analysis(dados),
    "constant rankings"
  )

  expect_true(all(is.na(resultado$correlation_matrix)))
  expect_equal(resultado$results_table$rank_equal, c(1L, 1L, 1L))
})

test_that("undefined scenarios do not prevent other correlations", {
  dados <- data.frame(
    abundance_pad = c(0, 1),
    d_prime_pad = c(1, 0),
    connectivity_pad = c(0, 0),
    frequency_pad = c(0, 0)
  )

  expect_warning(
    resultado <- sensitivity_analysis(dados),
    "constant rankings"
  )

  correlacoes <- resultado$correlation_matrix

  expect_true(all(is.na(correlacoes["rank_equal", ])))

  expect_equal(
    unname(correlacoes["rank_abundance", "rank_specialization"]),
    -1
  )
})

test_that("equal scores receive the same rank", {
  dados <- sensitivity_test_data()
  dados <- rbind(dados, dados[1, ])
  dados$plant_species[4] <- "D"

  tabela <- sensitivity_analysis(dados)$results_table

  expect_equal(tabela$rank_equal, c(1L, 3L, 4L, 1L))
})

test_that("invalid tables are rejected", {
  dados <- sensitivity_test_data()

  expect_error(sensitivity_analysis(NULL), "data frame")
  expect_error(
    sensitivity_analysis(dados[1, ]),
    "at least two species"
  )
  expect_error(
    sensitivity_analysis(dados[, -2]),
    "Missing required columns"
  )

  names(dados)[3] <- names(dados)[2]
  expect_error(sensitivity_analysis(dados), "unique column names")
})

test_that("standardized components must be numeric, finite and in range", {
  colunas <- c(
    "abundance_pad",
    "d_prime_pad",
    "connectivity_pad",
    "frequency_pad"
  )

  for (coluna in colunas) {
    dados <- sensitivity_test_data()
    dados[[coluna]] <- as.character(dados[[coluna]])

    expect_error(sensitivity_analysis(dados), "must be numeric")

    for (valor in c(NA_real_, NaN, Inf, -Inf)) {
      dados <- sensitivity_test_data()
      dados[[coluna]][1] <- valor

      expect_error(sensitivity_analysis(dados), "infinite values")
    }

    for (valor in c(-0.1, 1.1)) {
      dados <- sensitivity_test_data()
      dados[[coluna]][1] <- valor

      expect_error(sensitivity_analysis(dados), "between 0 and 1")
    }
  }
})

test_that("prior grouping does not change sensitivity results", {
  dados <- sensitivity_test_data()

  expect_equal(
    sensitivity_analysis(dplyr::group_by(dados, plant_species)),
    sensitivity_analysis(dados)
  )
})
