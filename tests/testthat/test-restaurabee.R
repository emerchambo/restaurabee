test_that("restaurabee runs without error", {

  data <- data.frame(
    plant_species = c("Plant_A","Plant_A","Plant_B","Plant_C"),
    bee_species = c("Bee_1","Bee_2","Bee_1","Bee_3"),
    sample_unit = c("S1","S1","S2","S3")
  )

  result <- suppressWarnings(restaurabee(data))

  expect_type(result, "list")

  # checa apenas o essencial
  expect_true("interaction_matrix" %in% names(result))
  expect_true("plant_metrics" %in% names(result))
  expect_true("prioritization" %in% names(result))
})
