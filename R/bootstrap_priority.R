#' Bootstrap stability of plant prioritization
#'
#' Resamples sampling units with replacement and recalculates plant
#' metrics, IPR scores and species selection in each replicate.
#'
#' @param interaction_data A data frame containing \code{plant_species},
#' \code{bee_species} and \code{sample_unit}. Each row represents one
#' recorded interaction.
#' @param n_boot Number of bootstrap replicates. Default is 1000.
#' @param resample_unit Name of the column identifying the units to
#' resample. Default is \code{"sample_unit"}. To keep repeated visits
#' to the same site together, supply a column identifying that site.
#' @param weights Named numeric vector with weights for \code{abundance},
#' \code{specialization}, \code{connectivity} and \code{frequency}.
#' Weights must be non-negative and sum to one.
#' @param threshold Cumulative proportion of total IPR used to select
#' plants. Must be greater than zero and at most one. Default is 0.80.
#' @param ties Treatment of ties at the selection boundary:
#' \code{"first"} or \code{"include"}. Passed to \code{select_species()}.
#' @param conf Level used to calculate central empirical intervals
#' for IPR scores, ranks and the number of selected plants.
#' Default is 0.95.
#' @param seed Optional non-negative integer seed. The previous
#' random-number state is restored when the function finishes.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{summary}: presence and selection frequencies, and
#'   summaries of IPR scores and ranks for each plant.
#'   \item \code{replicates}: scores, ranks and selection status for
#'   plants in replicates with completed calculations. Selection is
#'   \code{NA} when total IPR is zero.
#'   \item \code{set_size}: number of selected plants in each requested
#'   replicate (\code{replicates}), its frequency distribution
#'   (\code{frequencies}) and a one-row \code{summary}.
#'   Failed calculations and undefined selections have \code{NA}
#'   sizes and are excluded from this summary. Their counts and
#'   statuses remain available.
#'   \item \code{diagnostics}: counts of completed calculations,
#'   defined selections, calculation errors and undefined selections.
#'   The \code{failures} table identifies excluded replicates and
#'   explains why they were excluded.
#'   \item \code{settings}: options used in the analysis.
#' }
#'
#' @details
#' Each replicate draws as many units as there are distinct units in
#' \code{resample_unit}. All records belonging to a drawn unit are
#' retained. Copies receive new sampling-unit identifiers so that
#' repeated draws remain distinct when calculating occurrence frequency.
#'
#' The resampling unit should reflect the study design. When sampling
#' units belong to a larger group whose observations should remain
#' together, use a column identifying that group. Each sampling-unit
#' identifier must belong to only one group.
#'
#' All summaries use replicates with a defined selection.
#' Selection frequency is the number of replicates in which a plant
#' was selected divided by the number with defined selection.
#' Plants absent from those replicates count as not selected.
#' Selection frequency conditional on presence is also reported.
#'
#' IPR and rank summaries additionally require the plant to be present.
#' Equal IPR scores receive average ranks. The reported intervals
#' describe the bootstrap distributions; they are not calibrated
#' confidence intervals for true priorities.
#'
#' Replicates with calculation errors are reported separately from
#' replicates whose total IPR is zero. When total IPR is zero, scores
#' and ranks are retained, but selection is recorded as \code{NA}.
#'
#' Excluding replicates can affect the interpretation and should be
#' examined. If none allow selection, summary frequencies and intervals
#' are returned as \code{NA}. If all metric calculations fail, the
#' function stops.
#'
#' In \code{diagnostics}, \code{n_successful} counts replicates with
#' defined selection. The count \code{n_failed} includes calculation
#' errors and undefined selections, reported separately as
#' \code{n_calculation_failed} and \code{n_selection_undefined}.
#'
#' Set-size summaries use the same replicates with defined selection
#' and the interval level supplied in \code{conf}. No additional
#' draws are generated. The standard deviation is \code{NA} when
#' fewer than two replicates have defined selection.
#'
#' The number of plants refers to the threshold and tie rule used
#' in that bootstrap run. Its percentile bounds describe resampling
#' variation, not an ideal number of plants for restoration.
#'
#' When units without interactions are absent from the input, the
#' analysis is restricted to units represented in the data.
#' Resampling does not correct differences in observation effort.
#'
#' @export
bootstrap_priority <- function(
    interaction_data,
    n_boot = 1000,
    resample_unit = "sample_unit",
    weights = c(
      abundance = 0.25,
      specialization = 0.25,
      connectivity = 0.25,
      frequency = 0.25
    ),
    threshold = 0.80,
    ties = c("first", "include"),
    conf = 0.95,
    seed = NULL
) {

  is_scalar_number <- function(x) {

    is.numeric(x) &&
      length(x) == 1L &&
      is.null(dim(x)) &&
      is.finite(x)
  }

  if (
    !is_scalar_number(n_boot) ||
    n_boot < 1 ||
    n_boot != floor(n_boot) ||
    n_boot > .Machine$integer.max
  ) {
    stop(
      "`n_boot` must be a positive integer.",
      call. = FALSE
    )
  }

  if (
    !is.character(resample_unit) ||
    length(resample_unit) != 1L ||
    !is.null(dim(resample_unit)) ||
    is.na(resample_unit) ||
    trimws(resample_unit) == ""
  ) {
    stop(
      "`resample_unit` must be a single column name.",
      call. = FALSE
    )
  }

  weights <- validate_ipr_weights(weights)

  if (
    !is_scalar_number(threshold) ||
    threshold <= 0 ||
    threshold > 1
  ) {
    stop(
      "`threshold` must be greater than 0 and at most 1.",
      call. = FALSE
    )
  }

  ties <- match.arg(ties)

  if (
    !is_scalar_number(conf) ||
    conf <= 0 ||
    conf >= 1
  ) {
    stop(
      "`conf` must be greater than 0 and less than 1.",
      call. = FALSE
    )
  }

  if (!is.null(seed)) {

    if (
      !is_scalar_number(seed) ||
      seed < 0 ||
      seed != floor(seed) ||
      seed > .Machine$integer.max
    ) {
      stop(
        "`seed` must be a non-negative integer or NULL.",
        call. = FALSE
      )
    }
  }

  interaction_data <- check_data(interaction_data)
  interaction_data <- as.data.frame(interaction_data)

  if (!resample_unit %in% names(interaction_data)) {
    stop(
      "Resampling column not found: ",
      resample_unit,
      ".",
      call. = FALSE
    )
  }

  unit_ids <- interaction_data[[resample_unit]]

  if (is.factor(unit_ids)) {
    unit_ids <- as.character(unit_ids)
  }

  if (
    (!is.character(unit_ids) && !is.numeric(unit_ids)) ||
    !is.null(dim(unit_ids))
  ) {
    stop(
      "The resampling column must contain character or numeric identifiers.",
      call. = FALSE
    )
  }

  if (
    anyNA(unit_ids) ||
    (is.numeric(unit_ids) && any(!is.finite(unit_ids)))
  ) {
    stop(
      "The resampling column must not contain missing or infinite values.",
      call. = FALSE
    )
  }

  unit_ids <- as.character(unit_ids)

  if (
    any(trimws(unit_ids) == "") ||
    any(unit_ids != trimws(unit_ids))
  ) {
    stop(
      "The resampling column must contain non-empty identifiers ",
      "without leading or trailing whitespace.",
      call. = FALSE
    )
  }

  units <- unique(unit_ids)
  n_units <- length(units)

  if (n_units < 2L) {
    stop(
      "At least two resampling units are required.",
      call. = FALSE
    )
  }

  # Each sampling unit must belong to one resampling group.
  membership <- unique(
    data.frame(
      sample_unit = interaction_data$sample_unit,
      group = unit_ids,
      stringsAsFactors = FALSE
    )
  )

  if (anyDuplicated(membership$sample_unit)) {
    stop(
      "Each `sample_unit` must belong to only one resampling group. ",
      "Use unique sampling-unit identifiers.",
      call. = FALSE
    )
  }

  blocks <- lapply(
    units,
    function(unit) which(unit_ids == unit)
  )

  species <- unique(interaction_data$plant_species)

  if (!is.null(seed)) {

    had_seed <- exists(
      ".Random.seed",
      envir = globalenv(),
      inherits = FALSE
    )

    if (had_seed) {

      previous_seed <- get(
        ".Random.seed",
        envir = globalenv(),
        inherits = FALSE
      )
    }

    on.exit(
      {
        if (had_seed) {

          assign(
            ".Random.seed",
            previous_seed,
            envir = globalenv()
          )

        } else if (exists(
          ".Random.seed",
          envir = globalenv(),
          inherits = FALSE
        )) {

          rm(".Random.seed", envir = globalenv())
        }
      },
      add = TRUE
    )

    set.seed(as.integer(seed))
  }

  n_boot <- as.integer(n_boot)

  successful <- vector("list", n_boot)
  errors <- rep(NA_character_, n_boot)

  for (replicate_id in seq_len(n_boot)) {

    attempt <- tryCatch(
      {

        drawn_units <- sample.int(
          n_units,
          size = n_units,
          replace = TRUE
        )

        copies <- lapply(
          seq_along(drawn_units),
          function(draw_id) {

            block <- interaction_data[
              blocks[[drawn_units[draw_id]]],
              ,
              drop = FALSE
            ]

            local_unit <- match(
              block$sample_unit,
              unique(block$sample_unit)
            )

            block$sample_unit <- paste0(
              "draw_", draw_id,
              "_unit_", local_unit
            )

            block
          }
        )

        bootstrap_data <- do.call(rbind, copies)
        rownames(bootstrap_data) <- NULL

        interaction_matrix <- build_interaction_matrix(
          bootstrap_data
        )

        IIP <- calculate_IIP(bootstrap_data)

        specialization <- calculate_d_prime(
          interaction_matrix
        )

        plant_metrics <- dplyr::left_join(
          IIP,
          specialization$plant_dprime,
          by = "plant_species"
        )

        priorities <- calculate_IPR(
          plant_metrics,
          weight_abundance = weights[["abundance"]],
          weight_specialization = weights[["specialization"]],
          weight_connectivity = weights[["connectivity"]],
          weight_frequency = weights[["frequency"]]
        )

        selection_defined <- sum(priorities$IPR) > 0

        if (selection_defined) {

          selected <- select_species(
            priorities,
            threshold = threshold,
            ties = ties
          )

          selected_status <- priorities$plant_species %in%
            selected$plant_species

        } else {

          selected_status <- rep(
            NA,
            nrow(priorities)
          )
        }

        data.frame(
          replicate = replicate_id,
          plant_species = priorities$plant_species,
          IPR = priorities$IPR,
          rank = rank(
            -priorities$IPR,
            ties.method = "average"
          ),
          selected = selected_status,
          stringsAsFactors = FALSE
        )
      },
      error = function(e) e
    )

    if (inherits(attempt, "error")) {

      errors[replicate_id] <- conditionMessage(attempt)

    } else {

      successful[[replicate_id]] <- attempt
    }
  }

  metrics_available <- !vapply(
    successful,
    is.null,
    logical(1)
  )

  selection_available <- vapply(
    successful,
    function(result) {
      !is.null(result) && !anyNA(result$selected)
    },
    logical(1)
  )

  n_metrics <- sum(metrics_available)
  n_successful <- sum(selection_available)

  n_calculation_failed <- sum(!metrics_available)

  n_selection_undefined <- sum(
    metrics_available & !selection_available
  )

  valid <- selection_available
  n_failed <- n_boot - n_successful

  if (n_metrics == 0L) {
    stop(
      "All bootstrap metric calculations failed. First error: ",
      errors[which(!is.na(errors))[1L]],
      call. = FALSE
    )
  }

  undefined <- which(
    metrics_available & !selection_available
  )

  errors[undefined] <- "Selection undefined: total IPR is zero."

  replicates <- do.call(
    rbind,
    successful[metrics_available]
  )

  rownames(replicates) <- NULL

  selection_replicates <- replicates[
    replicates$replicate %in% which(selection_available),
    ,
    drop = FALSE
  ]

  probabilities <- c(
    (1 - conf) / 2,
    1 - (1 - conf) / 2
  )

  summary_rows <- lapply(
    species,
    function(plant) {

      plant_results <- selection_replicates[
        selection_replicates$plant_species == plant,
        ,
        drop = FALSE
      ]

      n_present <- nrow(plant_results)
      n_selected <- sum(plant_results$selected)

      ipr_interval <- rank_interval <- c(
        NA_real_,
        NA_real_
      )

      ipr_mean <- rank_median <- NA_real_

      if (n_present > 0L) {

        ipr_mean <- mean(plant_results$IPR)

        rank_median <- stats::median(
          plant_results$rank
        )

        ipr_interval <- stats::quantile(
          plant_results$IPR,
          probs = probabilities,
          names = FALSE,
          type = 7
        )

        rank_interval <- stats::quantile(
          plant_results$rank,
          probs = probabilities,
          names = FALSE,
          type = 7
        )
      }

      data.frame(
        plant_species = plant,
        n_present = n_present,

        presence_frequency = if (n_successful > 0L) {
          n_present / n_successful
        } else {
          NA_real_
        },

        n_selected = n_selected,

        selection_frequency = if (n_successful > 0L) {
          n_selected / n_successful
        } else {
          NA_real_
        },

        selection_given_presence = if (n_present > 0L) {
          n_selected / n_present
        } else {
          NA_real_
        },

        IPR_mean = ipr_mean,
        IPR_lower = ipr_interval[1],
        IPR_upper = ipr_interval[2],
        rank_median = rank_median,
        rank_lower = rank_interval[1],
        rank_upper = rank_interval[2],
        stringsAsFactors = FALSE
      )
    }
  )

  summary_table <- do.call(
    rbind,
    summary_rows
  )

  rownames(summary_table) <- NULL

  failure_table <- data.frame(
    replicate = which(!valid),

    type = ifelse(
      metrics_available[!valid],
      "selection_undefined",
      "calculation_failed"
    ),

    message = errors[!valid],
    stringsAsFactors = FALSE
  )

  if (n_failed > 0L) {

    warning(
      n_calculation_failed,
      " replicate(s) had calculation errors; ",
      n_selection_undefined,
      " replicate(s) had undefined selection because total IPR was zero. ",
      "Selection summaries use ",
      n_successful,
      " of ",
      n_boot,
      " replicates. See `diagnostics$failures`.",
      call. = FALSE
    )
  }

  list(
    summary = summary_table,
    replicates = replicates,

    set_size = summarize_bootstrap_set_size(
      successful,
      conf
    ),

    diagnostics = list(
      n_requested = n_boot,
      n_successful = n_successful,
      n_failed = n_failed,
      n_metrics_available = n_metrics,
      n_calculation_failed = n_calculation_failed,
      n_selection_undefined = n_selection_undefined,
      failures = failure_table
    ),

    settings = list(
      n_boot = n_boot,
      resample_unit = resample_unit,
      n_units = n_units,
      weights = weights,
      threshold = threshold,
      ties = ties,
      conf = conf,
      seed = seed
    )
  )
}
