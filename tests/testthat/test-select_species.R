selection_test_data <- function() {
  data.frame(
    plant_species = c("A", "B", "C", "D"),
    IPR = c(0.4, 0.3, 0.3, 0)
  )
}

test_that("selection follows decreasing IPR and the cumulative threshold", {
  dados <- data.frame(
    plant_species = c("C", "A", "D", "B"),
    IPR = c(0.15, 0.5, 0.1, 0.25)
  )

  resultado <- select_species(dados)

  expect_identical(resultado$plant_species, c("A", "B", "C"))
  expect_equal(resultado$relative_IPR, c(0.5, 0.25, 0.15))
  expect_equal(resultado$cumulative_IPR, c(0.5, 0.75, 0.9))
})

test_that("relative contributions use the total IPR before selection", {
  dados <- data.frame(
    plant_species = c("A", "B", "C"),
    IPR = c(2, 1, 1)
  )

  resultado <- select_species(dados, threshold = 0.5)

  expect_identical(resultado$plant_species, "A")
  expect_equal(resultado$relative_IPR, 0.5)
  expect_equal(resultado$cumulative_IPR, 0.5)
})

test_that("default tie handling preserves input order", {
  dados <- selection_test_data()

  expect_identical(
    select_species(dados, threshold = 0.5)$plant_species,
    c("A", "B")
  )

  expect_identical(
    select_species(dados[c(1, 3, 2, 4), ], threshold = 0.5)$plant_species,
    c("A", "C")
  )
})

test_that("including boundary ties gives the same selected set", {
  dados <- selection_test_data()

  original <- select_species(
    dados,
    threshold = 0.5,
    ties = "include"
  )

  reordenado <- select_species(
    dados[c(1, 3, 2, 4), ],
    threshold = 0.5,
    ties = "include"
  )

  expect_identical(original$plant_species, c("A", "B", "C"))
  expect_setequal(original$plant_species, reordenado$plant_species)
})

test_that("a threshold of one retains all positive scores", {
  dados <- selection_test_data()

  for (opcao in c("first", "include")) {
    resultado <- select_species(dados, threshold = 1, ties = opcao)

    expect_identical(resultado$plant_species, c("A", "B", "C"))
    expect_equal(tail(resultado$cumulative_IPR, 1), 1)
  }
})

test_that("floating-point rounding does not prevent full selection", {
  dados <- data.frame(
    plant_species = LETTERS[1:10],
    IPR = rep(0.1, 10)
  )

  resultado <- select_species(dados, threshold = 1)

  expect_equal(nrow(resultado), 10)
  expect_equal(tail(resultado$cumulative_IPR, 1), 1)
})

test_that("invalid thresholds and tie options are rejected", {
  dados <- selection_test_data()

  invalidos <- list(
    NA_real_, NaN, Inf, -Inf, "0.8", TRUE,
    numeric(0), c(0.5, 0.8), matrix(0.8, nrow = 1)
  )

  for (valor in invalidos) {
    expect_error(
      select_species(dados, threshold = valor),
      "finite numeric scalar"
    )
  }

  for (valor in c(-0.1, 0, 1.1)) {
    expect_error(
      select_species(dados, threshold = valor),
      "greater than 0"
    )
  }

  expect_error(
    select_species(dados, ties = "unknown"),
    "'arg' should be"
  )
})

test_that("invalid tables and scores are rejected", {
  dados <- selection_test_data()

  expect_error(select_species(NULL), "data frame")
  expect_error(select_species(dados[0, ]), "at least one row")
  expect_error(
    select_species(dados[, "plant_species", drop = FALSE]),
    "IPR.*column"
  )

  duplicados <- dados
  names(duplicados) <- c("IPR", "IPR")
  expect_error(select_species(duplicados), "unique column names")

  dados$IPR <- as.character(dados$IPR)
  expect_error(select_species(dados), "numeric column")

  for (valor in c(NA_real_, NaN, Inf, -Inf)) {
    dados <- selection_test_data()
    dados$IPR[1] <- valor
    expect_error(select_species(dados), "infinite values")
  }

  dados <- selection_test_data()
  dados$IPR[1] <- -0.1
  expect_error(select_species(dados), "negative values")

  dados$IPR <- rep(0, nrow(dados))
  expect_error(select_species(dados), "greater than zero")
})

test_that("prior grouping does not affect selection", {
  dados <- selection_test_data()

  expect_equal(
    select_species(dplyr::group_by(dados, plant_species)),
    select_species(dados)
  )
})
