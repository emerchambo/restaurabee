test_that("IIP matches manually calculated values", {
  dados <- data.frame(
    plant_species = c("A", "A", "A", "B"),
    bee_species = c("b1", "b1", "b2", "b2"),
    sample_unit = c("S1", "S1", "S2", "S2")
  )

  resultado <- calculate_IIP(dados)

  expect_identical(resultado$plant_species, c("A", "B"))
  expect_equal(resultado$visits, c(3, 1))
  expect_equal(resultado$occurrences, c(2, 1))
  expect_equal(resultado$partners, c(2, 1))
  expect_equal(resultado$abundance_relative, c(3 / 4, 1 / 4))
  expect_equal(resultado$frequency_relative, c(2 / 3, 1 / 3))
  expect_equal(resultado$connectivity_relative, c(2 / 3, 1 / 3))
  expect_equal(resultado$IIP, c(25 / 36, 11 / 36))
})

test_that("relative components and IIP sum to one", {
  dados <- data.frame(
    plant_species = c("A", "A", "B", "C"),
    bee_species = c("b1", "b2", "b2", "b1"),
    sample_unit = c("S1", "S2", "S2", "S3")
  )

  resultado <- calculate_IIP(dados)

  expect_equal(sum(resultado$abundance_relative), 1)
  expect_equal(sum(resultado$frequency_relative), 1)
  expect_equal(sum(resultado$connectivity_relative), 1)
  expect_equal(sum(resultado$IIP), 1)
})

test_that("repeated records increase visits but not occurrences or partners", {
  dados <- data.frame(
    plant_species = c("A", "B"),
    bee_species = c("b1", "b2"),
    sample_unit = c("S1", "S2")
  )

  original <- calculate_IIP(dados)
  repetidos <- calculate_IIP(rbind(dados, dados[1, ]))

  expect_equal(repetidos$visits[repetidos$plant_species == "A"], 2)

  expect_equal(
    repetidos$occurrences[match(original$plant_species,
                                repetidos$plant_species)],
    original$occurrences
  )

  expect_equal(
    repetidos$partners[match(original$plant_species,
                             repetidos$plant_species)],
    original$partners
  )
})

test_that("pre-existing grouping does not change the results", {
  dados <- data.frame(
    plant_species = c("A", "A", "B"),
    bee_species = c("b1", "b2", "b2"),
    sample_unit = c("S1", "S2", "S2"),
    site = c("X", "Y", "Y")
  )

  agrupados <- dplyr::group_by(dados, site)

  expect_equal(
    calculate_IIP(agrupados),
    calculate_IIP(dados)
  )
})

test_that("invalid identifiers are rejected when called directly", {
  dados <- data.frame(
    plant_species = c("A", "B"),
    bee_species = c("b1", "b2"),
    sample_unit = c("S1", "S2")
  )

  expect_error(calculate_IIP(NULL), "data frame")
  expect_error(calculate_IIP(dados[0, ]), "at least one row")
  expect_error(calculate_IIP(dados[, -3]), "Missing required columns")

  dados$sample_unit[1] <- NA
  expect_error(calculate_IIP(dados), "missing or empty")

  dados$sample_unit[1] <- " S1 "
  expect_error(calculate_IIP(dados), "extra whitespace")
})

test_that("numeric and factor identifiers are accepted", {
  dados <- data.frame(
    plant_species = c(1L, 1L, 2L),
    bee_species = factor(c("b1", "b2", "b2")),
    sample_unit = c(1L, 2L, 2L)
  )

  resultado <- calculate_IIP(dados)

  expect_identical(resultado$plant_species, c("1", "2"))
  expect_equal(resultado$IIP, c(2 / 3, 1 / 3))
})
