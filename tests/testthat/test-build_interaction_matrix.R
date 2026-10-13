test_that("interaction records are counted and missing pairs receive zero", {
  dados <- data.frame(
    plant_species = c("Plant_A", "Plant_A", "Plant_A", "Plant_B"),
    bee_species = c("Bee_1", "Bee_1", "Bee_2", "Bee_2")
  )

  esperada <- matrix(
    c(2, 0, 1, 1),
    nrow = 2,
    dimnames = list(
      c("Plant_A", "Plant_B"),
      c("Bee_1", "Bee_2")
    )
  )

  resultado <- build_interaction_matrix(dados)

  expect_identical(resultado, esperada)
  expect_equal(sum(resultado), nrow(dados))
})

test_that("sampling units and additional columns do not affect counts", {
  dados <- data.frame(
    plant_species = c("A", "A", "B"),
    bee_species = c("b1", "b1", "b2"),
    sample_unit = c("S1", "S2", "S3"),
    extra = c(10, 20, 30)
  )

  expect_identical(
    build_interaction_matrix(dados),
    build_interaction_matrix(dados[, c("plant_species", "bee_species")])
  )
})

test_that("species identifiers can match internal column names", {
  dados <- data.frame(
    plant_species = c("A", "A", "B"),
    bee_species = c("n", "plant_species", "n")
  )

  resultado <- build_interaction_matrix(dados)

  expect_equal(resultado["A", "n"], 1)
  expect_equal(resultado["A", "plant_species"], 1)
  expect_equal(resultado["B", "plant_species"], 0)
  expect_equal(sum(resultado), 3)
})

test_that("numeric and factor identifiers are accepted", {
  dados <- data.frame(
    plant_species = c(1L, 1L, 2L),
    bee_species = factor(c("b1", "b1", "b2"))
  )

  resultado <- build_interaction_matrix(dados)

  expect_identical(rownames(resultado), c("1", "2"))
  expect_equal(resultado["1", "b1"], 2)
})

test_that("invalid inputs are rejected when called directly", {
  dados <- data.frame(
    plant_species = c("A", "B"),
    bee_species = c("b1", "b2")
  )

  expect_error(build_interaction_matrix(NULL), "data frame")
  expect_error(
    build_interaction_matrix(dados[, "plant_species", drop = FALSE]),
    "Missing required columns"
  )
  expect_error(
    build_interaction_matrix(dados[0, ]),
    "at least one row"
  )

  dados$plant_species[1] <- NA
  expect_error(build_interaction_matrix(dados), "missing or empty")

  dados$plant_species[1] <- " A "
  expect_error(build_interaction_matrix(dados), "extra whitespace")
})

test_that("one-row and one-column matrices retain their dimensions", {
  dados <- data.frame(
    plant_species = c("A", "A"),
    bee_species = c("b1", "b1")
  )

  resultado <- build_interaction_matrix(dados)

  expect_true(is.matrix(resultado))
  expect_identical(dim(resultado), c(1L, 1L))
  expect_equal(resultado["A", "b1"], 2)
})
