omission_test_data <- function() {

  data.frame(
    plant_species = c("A", "B", "C", "D"),
    abundance_pad = c(1, 0.7, 0.2, 0),
    d_prime_pad = c(0.1, 0.8, 0.2, 1),
    connectivity_pad = c(0, 0.5, 1, 0.2),
    frequency_pad = c(1, 0.6, 0.1, 0)
  )
}


test_that("reference scores follow the supplied weights", {

  data <- omission_test_data()

  weights <- c(
    abundance = 0.20,
    specialization = 0.40,
    connectivity = 0.30,
    frequency = 0.10
  )

  result <- component_omission(
    data,
    weights = weights
  )

  reference <- result$results_table[
    result$results_table$scenario == "reference",
    ,
    drop = FALSE
  ]

  expected <-
    0.20 * data$abundance_pad +
    0.40 * data$d_prime_pad +
    0.30 * data$connectivity_pad +
    0.10 * data$frequency_pad

  expect_equal(reference$IPR, expected)

  expect_equal(
    reference$rank,
    rank(-expected, ties.method = "average")
  )

  expect_equal(
    reference$rank_change,
    rep(0, nrow(data))
  )

  expect_equal(result$summary$jaccard[1], 1)

  expect_identical(
    result$settings$weights,
    weights
  )
})


test_that("each omission redistributes the remaining weights", {

  result <- component_omission(omission_test_data())

  expect_identical(
    rownames(result$scenario_weights),
    c(
      "reference",
      "without_abundance",
      "without_specialization",
      "without_connectivity",
      "without_frequency"
    )
  )

  expect_equal(
    unname(result$scenario_weights["reference", ]),
    rep(0.25, 4)
  )

  for (component in colnames(result$scenario_weights)) {

    scenario <- paste0("without_", component)

    expect_equal(
      result$scenario_weights[scenario, component],
      0
    )

    expect_equal(
      unname(
        result$scenario_weights[
          scenario,
          colnames(result$scenario_weights) != component
        ]
      ),
      rep(1 / 3, 3)
    )
  }

  expect_equal(
    unname(rowSums(result$scenario_weights)),
    rep(1, 5)
  )
})


test_that("custom weights are redistributed proportionally", {

  weights <- c(
    abundance = 0.20,
    specialization = 0.40,
    connectivity = 0.30,
    frequency = 0.10
  )

  data <- omission_test_data()

  result <- component_omission(
    data,
    weights = weights
  )

  expect_equal(
    result$scenario_weights["without_specialization", ],
    c(
      abundance = 1 / 3,
      specialization = 0,
      connectivity = 1 / 2,
      frequency = 1 / 6
    )
  )

  without_d <- result$results_table[
    result$results_table$scenario == "without_specialization",
    ,
    drop = FALSE
  ]

  expected <-
    data$abundance_pad / 3 +
    data$connectivity_pad / 2 +
    data$frequency_pad / 6

  expect_equal(without_d$IPR, expected)
})


test_that("rank and membership summaries match detailed results", {

  result <- component_omission(omission_test_data())

  reference <- result$results_table[
    result$results_table$scenario == "reference",
    ,
    drop = FALSE
  ]

  reference_set <- reference$plant_species[
    reference$selected
  ]

  for (i in seq_len(nrow(result$summary))) {

    scenario <- result$summary$scenario[i]

    current <- result$results_table[
      result$results_table$scenario == scenario,
      ,
      drop = FALSE
    ]

    current_set <- current$plant_species[
      current$selected
    ]

    changes <- current$rank - reference$rank

    expect_equal(
      current$rank_change,
      changes
    )

    expect_equal(
      result$summary$mean_absolute_rank_change[i],
      mean(abs(changes))
    )

    expect_equal(
      result$summary$max_absolute_rank_change[i],
      max(abs(changes))
    )

    expect_equal(
      result$summary$spearman_vs_reference[i],
      unname(stats::cor(
        current$rank,
        reference$rank,
        method = "spearman"
      ))
    )

    expect_equal(
      result$summary$kendall_vs_reference[i],
      unname(stats::cor(
        current$rank,
        reference$rank,
        method = "kendall"
      ))
    )

    expect_equal(
      result$summary$n_selected[i],
      length(current_set)
    )

    expect_equal(
      result$summary$n_shared[i],
      length(intersect(reference_set, current_set))
    )

    expect_equal(
      result$summary$jaccard[i],
      length(intersect(reference_set, current_set)) /
        length(union(reference_set, current_set))
    )
  }

  expect_identical(
    result$summary$added[
      result$summary$scenario == "without_abundance"
    ],
    "D"
  )

  expect_identical(
    result$summary$removed[
      result$summary$scenario == "without_abundance"
    ],
    "A"
  )
})


test_that("omitting the only positive weight is explicitly undefined", {

  weights <- c(
    abundance = 1,
    specialization = 0,
    connectivity = 0,
    frequency = 0
  )

  result <- component_omission(
    omission_test_data(),
    weights = weights
  )

  omitted <- result$summary$scenario == "without_abundance"

  expect_identical(
    result$summary$status[omitted],
    "no_remaining_weight"
  )

  expect_false(
    result$summary$selection_defined[omitted]
  )

  expect_true(
    is.na(result$summary$n_selected[omitted])
  )

  expect_true(
    all(is.na(
      result$scenario_weights["without_abundance", ]
    ))
  )

  details <- result$results_table[
    result$results_table$scenario == "without_abundance",
    ,
    drop = FALSE
  ]

  expect_true(all(is.na(details$IPR)))
  expect_true(all(is.na(details$rank)))
  expect_true(all(is.na(details$selected)))

  expect_equal(
    result$summary$spearman_vs_reference[!omitted],
    rep(1, 4)
  )
})


test_that("zero totals retain scores but have undefined selection", {

  data <- omission_test_data()

  for (column in c(
    "abundance_pad",
    "d_prime_pad",
    "connectivity_pad",
    "frequency_pad"
  )) {
    data[[column]] <- 0
  }

  expect_warning(
    result <- component_omission(data),
    NA
  )

  expect_true(
    all(result$summary$status == "zero_total")
  )

  expect_true(
    all(!result$summary$selection_defined)
  )

  expect_true(
    all(is.na(result$summary$n_selected))
  )

  expect_true(
    all(is.na(result$summary$jaccard))
  )

  expect_true(
    all(is.na(result$summary$spearman_vs_reference))
  )

  expect_true(
    all(is.na(result$results_table$selected))
  )

  expect_equal(
    result$results_table$IPR,
    rep(0, 20)
  )

  expect_equal(
    result$results_table$rank,
    rep(2.5, 20)
  )
})


test_that("tie rules and alphabetical ordering are respected", {

  data <- omission_test_data()
  data$plant_species <- c("D", "C", "B", "A")

  for (column in c(
    "abundance_pad",
    "d_prime_pad",
    "connectivity_pad",
    "frequency_pad"
  )) {
    data[[column]] <- 1
  }

  first <- component_omission(
    data,
    threshold = 0.5
  )

  alphabetical <- component_omission(
    data,
    threshold = 0.5,
    tie_break = "alphabetical"
  )

  included <- component_omission(
    data,
    threshold = 0.5,
    ties = "include"
  )

  first_set <- first$results_table[
    first$results_table$scenario == "reference" &
      first$results_table$selected,
    "plant_species"
  ]

  alphabetical_set <- alphabetical$results_table[
    alphabetical$results_table$scenario == "reference" &
      alphabetical$results_table$selected,
    "plant_species"
  ]

  expect_identical(first_set, c("D", "C"))

  expect_identical(
    alphabetical_set,
    c("A", "B")
  )

  expect_equal(
    included$summary$n_selected,
    rep(4, 5)
  )

  expect_true(
    all(is.na(first$summary$spearman_vs_reference))
  )
})


test_that("input values are preserved and supplied IPR is recalculated", {

  data <- omission_test_data()
  data$IPR <- 99
  original <- data

  result <- component_omission(data)

  expect_identical(data, original)

  reference <- result$results_table[
    result$results_table$scenario == "reference",
    ,
    drop = FALSE
  ]

  expect_equal(
    reference$IPR,
    with(
      data,
      0.25 * abundance_pad +
        0.25 * d_prime_pad +
        0.25 * connectivity_pad +
        0.25 * frequency_pad
    )
  )

  expect_identical(
    reference$abundance_pad,
    data$abundance_pad
  )

  expect_identical(
    reference$d_prime_pad,
    data$d_prime_pad
  )
})


test_that("invalid thresholds and identifiers are rejected", {

  for (value in list(
    NA_real_, NaN, Inf, -Inf,
    0, -0.1, 1.1, TRUE,
    "0.8", numeric(0), c(0.5, 0.8),
    matrix(0.8)
  )) {

    expect_error(
      component_omission(
        omission_test_data(),
        threshold = value
      ),
      "threshold"
    )
  }

  data <- omission_test_data()
  data$plant_species[2] <- data$plant_species[1]

  expect_error(
    component_omission(data),
    "unique, non-empty identifiers"
  )

  data <- omission_test_data()
  data$plant_species[1] <- ""

  expect_error(
    component_omission(data),
    "unique, non-empty identifiers"
  )

  data <- omission_test_data()
  data$plant_species[1] <- NA_character_

  expect_error(
    component_omission(data),
    "unique, non-empty identifiers"
  )

  expect_error(
    component_omission(
      omission_test_data(),
      tie_break = "unknown"
    ),
    "arg"
  )
})
