dprime_test_matrix <- function() {
  matrix(
    c(
      8, 2, 1,
      1, 7, 2,
      2, 1, 9
    ),
    nrow = 3,
    byrow = TRUE,
    dimnames = list(
      c("Plant_A", "Plant_B", "Plant_C"),
      c("Bee_1", "Bee_2", "Bee_3")
    )
  )
}

test_that("d' matches the direct bipartite calculation", {
  matriz <- dprime_test_matrix()

  resultado <- calculate_d_prime(matriz)
  referencia <- bipartite::specieslevel(matriz, index = "d")

  expect_identical(
    resultado$plant_dprime$plant_species,
    rownames(referencia$`lower level`)
  )

  expect_equal(
    resultado$plant_dprime$d_prime,
    referencia$`lower level`$d
  )

  expect_identical(
    resultado$bee_dprime$bee_species,
    rownames(referencia$`higher level`)
  )

  expect_equal(
    resultado$bee_dprime$d_prime,
    referencia$`higher level`$d
  )
})

test_that("species remain associated with their values after reordering", {
  matriz <- dprime_test_matrix()
  original <- calculate_d_prime(matriz)

  reordenado <- calculate_d_prime(
    matriz[c(3, 1, 2), c(2, 3, 1)]
  )

  plantas <- match(
    original$plant_dprime$plant_species,
    reordenado$plant_dprime$plant_species
  )

  abelhas <- match(
    original$bee_dprime$bee_species,
    reordenado$bee_dprime$bee_species
  )

  expect_false(anyNA(plantas))
  expect_false(anyNA(abelhas))

  expect_equal(
    reordenado$plant_dprime$d_prime[plantas],
    original$plant_dprime$d_prime
  )

  expect_equal(
    reordenado$bee_dprime$d_prime[abelhas],
    original$bee_dprime$d_prime
  )
})

test_that("non-numeric inputs are rejected", {
  expect_error(calculate_d_prime(NULL), "numeric matrix")

  expect_error(
    calculate_d_prime(as.data.frame(dprime_test_matrix())),
    "numeric matrix"
  )

  expect_error(
    calculate_d_prime(matrix("a", nrow = 2, ncol = 2)),
    "numeric matrix"
  )
})

test_that("missing, infinite and negative values are rejected", {
  for (valor in c(NA_real_, NaN, Inf, -Inf)) {
    matriz <- dprime_test_matrix()
    matriz[1, 1] <- valor

    expect_error(
      calculate_d_prime(matriz),
      "infinite values"
    )
  }

  matriz <- dprime_test_matrix()
  matriz[1, 1] <- -1

  expect_error(calculate_d_prime(matriz), "negative values")
})

test_that("species identifiers must be present and unique", {
  matriz <- dprime_test_matrix()
  rownames(matriz) <- NULL
  expect_error(calculate_d_prime(matriz), "Row names")

  matriz <- dprime_test_matrix()
  colnames(matriz) <- NULL
  expect_error(calculate_d_prime(matriz), "Column names")

  matriz <- dprime_test_matrix()
  rownames(matriz) <- c("A", "A", "C")
  expect_error(calculate_d_prime(matriz), "Row names")

  matriz <- dprime_test_matrix()
  colnames(matriz) <- c("b1", "", "b3")
  expect_error(calculate_d_prime(matriz), "Column names")

  matriz <- dprime_test_matrix()
  rownames(matriz)[1] <- NA_character_
  expect_error(calculate_d_prime(matriz), "Row names")
})

test_that("small networks are rejected with a clear message", {
  matriz <- dprime_test_matrix()

  expect_error(
    calculate_d_prime(matriz[1, , drop = FALSE]),
    "At least two"
  )

  expect_error(
    calculate_d_prime(matriz[, 1, drop = FALSE]),
    "At least two"
  )
})

test_that("species without interaction records are rejected", {
  matriz <- dprime_test_matrix()
  matriz[1, ] <- 0

  expect_error(
    calculate_d_prime(matriz),
    "Plants without interaction records: Plant_A"
  )

  matriz <- dprime_test_matrix()
  matriz[, 2] <- 0

  expect_error(
    calculate_d_prime(matriz),
    "Bees without interaction records: Bee_2"
  )
})
