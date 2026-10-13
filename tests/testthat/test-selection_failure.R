selection_failure_data <- function() {
  matriz <- matrix(
    c(
      8, 2, 1,
      1, 7, 2,
      2, 1, 9
    ),
    nrow = 3,
    byrow = TRUE
  )

  pares <- expand.grid(
    plant_species = c("A", "B", "C"),
    bee_species = c("b1", "b2", "b3"),
    stringsAsFactors = FALSE
  )

  dados <- pares[
    rep(seq_len(nrow(pares)), times = as.vector(matriz)),
    ,
    drop = FALSE
  ]

  dados$sample_unit <- "S1"
  rownames(dados) <- NULL

  dados
}

test_that("selection failure is reported without selecting every plant", {
  testthat::local_mocked_bindings(
    select_species = function(...) {
      stop("simulated selection failure")
    },
    .package = "restaurabee"
  )

  # Network metrics are unrelated to this test.
  testthat::local_mocked_bindings(
    networklevel = function(...) NULL,
    .package = "bipartite"
  )

  expect_warning(
    resultado <- restaurabee(selection_failure_data()),
    "Species selection failed: simulated selection failure"
  )

  expect_null(resultado$selected_species)
  expect_s3_class(resultado, "restaurabee")
  expect_equal(nrow(resultado$prioritization), 3)
  expect_true(all(is.finite(resultado$prioritization$IPR)))

  saida_print <- capture.output(print(resultado))

  expect_true(any(grepl(
    "Selected species: not available",
    saida_print,
    fixed = TRUE
  )))

  expect_false(any(grepl(
    "Selected species (80% threshold):",
    saida_print,
    fixed = TRUE
  )))

  saida_summary <- capture.output(summary(resultado))

  expect_true(any(grepl(
    "Selection not available: the selection step failed.",
    saida_summary,
    fixed = TRUE
  )))

  expect_identical(
    withVisible(print(resultado))$visible,
    FALSE
  )

  expect_identical(
    withVisible(summary(resultado))$visible,
    FALSE
  )
})

test_that("successful selection is still displayed normally", {
  testthat::local_mocked_bindings(
    networklevel = function(...) NULL,
    .package = "bipartite"
  )

  resultado <- restaurabee(selection_failure_data())

  expect_true(is.data.frame(resultado$selected_species))
  expect_gt(nrow(resultado$selected_species), 0)

  saida_print <- capture.output(print(resultado))

  expect_true(any(grepl(
    "Selected species (80% threshold):",
    saida_print,
    fixed = TRUE
  )))

  expect_false(any(grepl(
    "selection failed",
    saida_print,
    fixed = TRUE
  )))

  saida_summary <- capture.output(summary(resultado))

  expect_false(any(grepl(
    "Selection not available",
    saida_summary,
    fixed = TRUE
  )))
})
