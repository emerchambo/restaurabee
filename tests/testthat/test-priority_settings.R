priority_settings_data <- function() {
  data.frame(
    plant_species = c("A", "A", "A", "B", "B", "C", "C", "C"),
    bee_species = c("b1", "b1", "b2", "b2", "b3", "b1", "b3", "b3"),
    sample_unit = c("S1", "S2", "S2", "S1", "S2", "S1", "S2", "S3")
  )
}

priority_settings_weights <- function() {
  c(
    abundance = 0.20,
    specialization = 0.40,
    connectivity = 0.20,
    frequency = 0.20
  )
}

test_that("custom weights are used for IPR and sensitivity", {
  w <- priority_settings_weights()

  resultado <- restaurabee(
    priority_settings_data(),
    network_indices = NULL,
    weights = w,
    threshold = 0.70,
    ties = "include"
  )

  referencia <- calculate_IPR(
    resultado$plant_metrics,
    weight_abundance = 0.20,
    weight_specialization = 0.40,
    weight_connectivity = 0.20,
    weight_frequency = 0.20
  )

  expect_equal(resultado$prioritization, referencia)
  expect_identical(resultado$settings$weights, w)
  expect_equal(resultado$settings$threshold, 0.70)
  expect_identical(resultado$settings$ties, "include")

  expect_equal(
    resultado$sensitivity$results_table$IPR_base,
    resultado$prioritization$IPR
  )

  expect_equal(
    resultado$sensitivity$scenario_weights["base", ],
    w
  )

  expect_equal(
    resultado$selected_species,
    select_species(referencia, threshold = 0.70, ties = "include")
  )
})

test_that("alternative weights increase one component proportionally", {
  resultado <- restaurabee(
    priority_settings_data(),
    network_indices = NULL,
    weights = priority_settings_weights()
  )

  cenarios <- resultado$sensitivity$scenario_weights

  expect_equal(
    unname(cenarios["abundance", ]),
    c(0.35, 0.325, 0.1625, 0.1625)
  )

  expect_equal(
    unname(cenarios["specialization", ]),
    c(0.15, 0.55, 0.15, 0.15)
  )

  expect_equal(unname(rowSums(cenarios)), rep(1, 5))
  expect_true(all(cenarios >= 0 & cenarios <= 1))
})

test_that("default settings preserve previous calculations", {
  resultado <- restaurabee(
    priority_settings_data(),
    network_indices = NULL
  )

  expect_equal(
    resultado$prioritization,
    calculate_IPR(resultado$plant_metrics)
  )

  expect_equal(
    resultado$selected_species,
    select_species(resultado$prioritization)
  )

  expect_equal(
    resultado$sensitivity$results_table$IPR_equal,
    resultado$prioritization$IPR
  )

  expect_equal(
    unname(resultado$sensitivity$scenario_weights["abundance", ]),
    c(0.40, 0.20, 0.20, 0.20)
  )

  expect_equal(resultado$settings$threshold, 0.80)
  expect_identical(resultado$settings$ties, "first")
})

test_that("weight names determine their order", {
  w <- priority_settings_weights()
  shuffled <- w[c(4, 2, 1, 3)]

  expect_identical(validate_ipr_weights(shuffled), w)

  expect_equal(
    restaurabee(
      priority_settings_data(),
      network_indices = NULL,
      weights = shuffled
    ),
    restaurabee(
      priority_settings_data(),
      network_indices = NULL,
      weights = w
    )
  )
})

test_that("invalid weights are rejected", {
  w <- priority_settings_weights()

  expect_error(validate_ipr_weights(unname(w)), "unique names")
  expect_error(validate_ipr_weights(w[1:3]), "length four")

  duplicados <- w
  names(duplicados)[4] <- "abundance"
  expect_error(validate_ipr_weights(duplicados), "unique names")

  desconhecidos <- w
  names(desconhecidos)[4] <- "unknown"
  expect_error(validate_ipr_weights(desconhecidos), "unique names")

  for (valor in c(NA_real_, NaN, Inf, -Inf)) {
    invalidos <- w
    invalidos[1] <- valor
    expect_error(validate_ipr_weights(invalidos), "finite values")
  }

  negativos <- c(
    abundance = -0.1,
    specialization = 0.5,
    connectivity = 0.3,
    frequency = 0.3
  )

  expect_error(validate_ipr_weights(negativos), "non-negative")
  expect_error(validate_ipr_weights(w * 2), "sum to 1")

  expect_error(
    restaurabee(priority_settings_data(), weights = negativos),
    "non-negative"
  )

  pad <- data.frame(
    abundance_pad = c(1, 0),
    d_prime_pad = c(0, 1),
    connectivity_pad = c(1, 0),
    frequency_pad = c(1, 0)
  )

  expect_error(
    sensitivity_analysis(pad, weights = negativos),
    "non-negative"
  )
})

test_that("weights at the boundary remain valid in sensitivity scenarios", {
  pad <- data.frame(
    abundance_pad = c(1, 0),
    d_prime_pad = c(0, 1),
    connectivity_pad = c(1, 0),
    frequency_pad = c(1, 0)
  )

  w <- c(
    abundance = 1,
    specialization = 0,
    connectivity = 0,
    frequency = 0
  )

  cenarios <- sensitivity_analysis(pad, weights = w)$scenario_weights

  expect_equal(cenarios["base", ], w)
  expect_equal(cenarios["abundance", ], w)

  expect_equal(
    unname(cenarios["specialization", ]),
    c(0.85, 0.15, 0, 0)
  )

  expect_equal(unname(rowSums(cenarios)), rep(1, 5))
})

test_that("threshold and ties are passed to species selection", {
  original_select <- select_species
  received <- NULL

  testthat::local_mocked_bindings(
    select_species = function(prioritization, threshold, ties) {
      received <<- list(threshold = threshold, ties = ties)

      original_select(
        prioritization,
        threshold = threshold,
        ties = ties
      )
    },
    .package = "restaurabee"
  )

  resultado <- restaurabee(
    priority_settings_data(),
    network_indices = NULL,
    threshold = 0.60,
    ties = "include"
  )

  expect_equal(received$threshold, 0.60)
  expect_identical(received$ties, "include")
  expect_true(is.data.frame(resultado$selected_species))
})

test_that("invalid selection settings stop the workflow", {
  for (valor in list(NA_real_, Inf, "0.8", numeric(0), c(0.5, 0.8))) {
    expect_error(
      restaurabee(priority_settings_data(), threshold = valor),
      "finite numeric scalar"
    )
  }

  for (valor in c(0, -0.1, 1.1)) {
    expect_error(
      restaurabee(priority_settings_data(), threshold = valor),
      "greater than 0"
    )
  }

  expect_error(
    restaurabee(priority_settings_data(), ties = "unknown"),
    "'arg' should be"
  )
})

test_that("print and summary display the chosen settings", {
  resultado <- restaurabee(
    priority_settings_data(),
    network_indices = NULL,
    weights = priority_settings_weights(),
    threshold = 0.70,
    ties = "include"
  )

  saida_print <- capture.output(print(resultado))
  saida_summary <- capture.output(summary(resultado))

  expect_true(any(grepl(
    "Selected species (70% threshold):",
    saida_print,
    fixed = TRUE
  )))

  expect_true(any(grepl(
    "Specialization:\\s+0\\.4",
    saida_print
  )))

  expect_true(any(grepl(
    "Selection threshold:\\s+70%",
    saida_summary
  )))

  expect_true(any(grepl(
    "Boundary ties:\\s+include",
    saida_summary
  )))
})

test_that("older objects without settings can still be displayed", {
  resultado <- restaurabee(
    priority_settings_data(),
    network_indices = NULL
  )

  resultado$settings <- NULL

  saida_print <- capture.output(print(resultado))
  saida_summary <- capture.output(summary(resultado))

  expect_true(any(grepl(
    "Selected species (80% threshold):",
    saida_print,
    fixed = TRUE
  )))

  expect_true(any(grepl(
    "Selection threshold:\\s+80%",
    saida_summary
  )))
})
