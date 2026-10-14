coverage_test_matrix <- function() {
  matrix(
    c(
      2, 1, 0,
      0, 3, 0,
      0, 0, 1
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      c("Plant_A", "Plant_B", "Plant_C"),
      c("Bee_1", "Bee_2", "Bee_3")
    )
  )
}

test_that("coverage counts distinct bee partners of selected plants", {
  result <- pollinator_coverage(
    c("Plant_A", "Plant_B"),
    coverage_test_matrix()
  )

  expect_equal(result$coverage, 2 / 3)
  expect_equal(result$n_plants, 2)
  expect_equal(result$n_bees_total, 3)
  expect_equal(result$n_bees_covered, 2)
  expect_identical(result$covered, c("Bee_1", "Bee_2"))
  expect_identical(result$not_covered, "Bee_3")
})

test_that("selecting all plants covers all recorded bees", {
  result <- pollinator_coverage(
    c("Plant_A", "Plant_B", "Plant_C"),
    coverage_test_matrix()
  )

  expect_equal(result$coverage, 1)
  expect_equal(result$n_bees_covered, 3)
  expect_identical(result$not_covered, character(0))
})

test_that("an empty selection has zero coverage", {
  result <- pollinator_coverage(
    character(0),
    coverage_test_matrix()
  )

  expect_equal(result$coverage, 0)
  expect_equal(result$n_plants, 0)
  expect_equal(result$n_bees_covered, 0)
  expect_identical(result$covered, character(0))
  expect_identical(
    result$not_covered,
    c("Bee_1", "Bee_2", "Bee_3")
  )
})

test_that("repeated plant identifiers do not change coverage", {
  matrix <- coverage_test_matrix()

  expect_identical(
    pollinator_coverage(c("Plant_A", "Plant_A"), matrix),
    pollinator_coverage("Plant_A", matrix)
  )
})

test_that("coverage depends on recorded partners rather than counts", {
  matrix <- coverage_test_matrix()
  incidence <- matrix
  incidence[] <- as.numeric(matrix > 0)

  expect_identical(
    pollinator_coverage("Plant_A", matrix),
    pollinator_coverage("Plant_A", incidence)
  )
})

test_that("bee columns without interactions are excluded", {
  matrix <- cbind(
    coverage_test_matrix(),
    Bee_4 = 0
  )

  result <- pollinator_coverage("Plant_A", matrix)

  expect_equal(result$n_bees_total, 3)
  expect_equal(result$coverage, 2 / 3)
  expect_identical(result$not_covered, "Bee_3")
})

test_that("a plant without interactions adds no bee coverage", {
  matrix <- rbind(
    coverage_test_matrix(),
    Plant_D = 0
  )

  result <- pollinator_coverage("Plant_D", matrix)

  expect_equal(result$coverage, 0)
  expect_equal(result$n_plants, 1)
  expect_equal(result$n_bees_covered, 0)
})

test_that("a one-cell matrix is supported", {
  matrix <- matrix(
    1,
    nrow = 1,
    dimnames = list("Plant_A", "Bee_1")
  )

  result <- pollinator_coverage("Plant_A", matrix)

  expect_equal(result$coverage, 1)
  expect_identical(result$covered, "Bee_1")
})

test_that("invalid plant selections are rejected", {
  matrix <- coverage_test_matrix()

  for (selection in list(
    1,
    TRUE,
    factor("Plant_A"),
    matrix("Plant_A", nrow = 1)
  )) {
    expect_error(
      pollinator_coverage(selection, matrix),
      "character vector"
    )
  }

  for (selection in list(NA_character_, "", " ")) {
    expect_error(
      pollinator_coverage(selection, matrix),
      "missing or empty identifiers"
    )
  }

  expect_error(
    pollinator_coverage(" Plant_A ", matrix),
    "leading or trailing whitespace"
  )

  expect_error(
    pollinator_coverage("Plant_unknown", matrix),
    "Selected plants not found"
  )
})

test_that("invalid matrix types and dimensions are rejected", {
  expect_error(
    pollinator_coverage("Plant_A", data.frame(Bee_1 = 1)),
    "numeric matrix"
  )

  expect_error(
    pollinator_coverage("Plant_A", matrix("1", nrow = 1)),
    "numeric matrix"
  )

  expect_error(
    pollinator_coverage(character(0), matrix(numeric(0), nrow = 0, ncol = 1)),
    "at least one row and one column"
  )

  expect_error(
    pollinator_coverage(character(0), matrix(numeric(0), nrow = 1, ncol = 0)),
    "at least one row and one column"
  )
})

test_that("invalid interaction values are rejected", {
  for (value in c(NA_real_, NaN, Inf, -Inf)) {
    matrix <- coverage_test_matrix()
    matrix[1, 1] <- value

    expect_error(
      pollinator_coverage("Plant_A", matrix),
      "NA, NaN or infinite values"
    )
  }

  matrix <- coverage_test_matrix()
  matrix[1, 1] <- -1

  expect_error(
    pollinator_coverage("Plant_A", matrix),
    "non-negative values"
  )

  matrix[] <- 0

  expect_error(
    pollinator_coverage("Plant_A", matrix),
    "at least one positive interaction"
  )
})

test_that("matrix species identifiers are validated", {
  for (axis in c("row", "column")) {
    for (case in c("missing", "empty", "duplicate", "whitespace", "absent")) {
      matrix <- coverage_test_matrix()

      identifiers <- if (axis == "row") {
        rownames(matrix)
      } else {
        colnames(matrix)
      }

      pattern <- switch(
        case,
        missing = "non-empty",
        empty = "non-empty",
        duplicate = "unique",
        whitespace = "leading or trailing whitespace",
        absent = "non-empty"
      )

      if (case == "missing") {
        identifiers[1] <- NA_character_
      } else if (case == "empty") {
        identifiers[1] <- ""
      } else if (case == "duplicate") {
        identifiers[2] <- identifiers[1]
      } else if (case == "whitespace") {
        identifiers[1] <- paste0(" ", identifiers[1], " ")
      } else {
        identifiers <- NULL
      }

      if (axis == "row") {
        rownames(matrix) <- identifiers
      } else {
        colnames(matrix) <- identifiers
      }

      expect_error(
        pollinator_coverage(character(0), matrix),
        pattern
      )
    }
  }
})
