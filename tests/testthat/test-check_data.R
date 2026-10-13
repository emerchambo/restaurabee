test_that("valid interaction records are preserved", {
  dados <- data.frame(
    plant_species = c("Plant_A", "Plant_A", "Plant_B"),
    bee_species = c("Bee_1", "Bee_1", "Bee_2"),
    sample_unit = c("S1", "S1", "S2")
  )

  expect_identical(check_data(dados), dados)
})

test_that("numeric species codes are converted to character", {
  dados <- data.frame(
    plant_species = c(1L, 1L, 2L),
    bee_species = c("Bee_1", "Bee_1", "Bee_2"),
    sample_unit = c("S1", "S1", "S2")
  )

  expect_identical(
    check_data(dados)$plant_species,
    c("1", "1", "2")
  )
})

test_that("missing identifiers are rejected", {
  dados <- data.frame(
    plant_species = c("Plant_A", NA, "Plant_B"),
    bee_species = c("Bee_1", "Bee_1", "Bee_2"),
    sample_unit = c("S1", "S1", "S2")
  )

  expect_error(
    check_data(dados),
    "missing or empty identifiers"
  )
})

test_that("whitespace cleaning requires an explicit choice", {
  dados <- data.frame(
    plant_species = c("Plant_A", " Plant_A ", "Plant_B"),
    bee_species = c("Bee_1", "Bee_1", "Bee_2"),
    sample_unit = c("S1", "S1", "S2")
  )

  expect_error(check_data(dados), "extra whitespace")

  expect_warning(
    limpos <- check_data(dados, clean_names = TRUE),
    "distinct identifiers merged: 1"
  )

  expect_identical(
    limpos$plant_species,
    c("Plant_A", "Plant_A", "Plant_B")
  )

  expect_equal(nrow(limpos), nrow(dados))
})
