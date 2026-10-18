#' Ecological Restoration Prioritization Using Plant-Pollinator Networks
#'
#' Calculates plant priority scores from interaction records and provides
#' component metrics, cumulative sets and diagnostic comparisons.
#'
#' @param interaction_data A data frame containing:
#' \describe{
#'   \item{plant_species}{Plant species identifier.}
#'   \item{bee_species}{Bee species identifier.}
#'   \item{sample_unit}{Sampling unit identifier.}
#' }
#' @param network_indices Character vector of indices passed to
#' \code{bipartite::networklevel()}. The default calculates connectance,
#' H2, weighted NODF, linkage density and web asymmetry.
#' Use \code{"ALLBUTDD"} for the broader set provided by bipartite,
#' or \code{NULL} to skip network metrics.
#' @param seed Optional non-negative integer seed for network metrics.
#' The previous random-number state is restored afterwards.
#' @param weights Named numeric vector with weights for \code{abundance},
#' \code{specialization}, \code{connectivity} and \code{frequency}.
#' Weights must be finite, non-negative and sum to one.
#' The default is 0.25 for each component.
#' @param threshold Cumulative proportion of total IPR to retain.
#' Must be greater than zero and less than or equal to one.
#' Default is 0.80.
#' @param ties Treatment of ties at the selection boundary.
#' \code{"first"} keeps the first rows needed to reach the threshold.
#' \code{"include"} also keeps all plants tied with the last selected plant.
#' @param diagnostics Logical. Calculate cumulative IPR, component
#' correlations and component omission comparisons. Default is \code{TRUE}.
#'
#' @return An object of class \code{restaurabee} containing:
#' \itemize{
#'   \item \code{metadata}: numbers of records, plants, bees and sampling units.
#'   \item \code{interaction_matrix}: plant-by-bee interaction matrix.
#'   \item \code{IIP}: Interaction Importance Index results.
#'   \item \code{d_prime}: specialization values for plants and bees.
#'   \item \code{plant_metrics}: ecological metrics for each plant.
#'   \item \code{prioritization}: IPR scores and standardized components.
#'   \item \code{selected_species}: plants retained at the cumulative
#'   threshold, or \code{NULL} if selection fails.
#'   \item \code{sensitivity}: ranking comparisons using the supplied
#'   weights as the reference.
#'   \item \code{network_metrics}: requested network descriptors, or
#'   \code{NULL} if skipped or if their calculation fails.
#'   \item \code{diagnostics}: results from \code{ipr_accumulation()},
#'   \code{component_correlations()} and \code{component_omission()},
#'   or \code{NULL} when disabled.
#'   \item \code{settings}: weights, threshold, tie handling,
#'   requested network indices, seed and diagnostic option.
#' }
#'
#' @details
#' The same weights are used for IPR and the reference rankings in
#' sensitivity and component omission analyses.
#' Changing the threshold or tie handling changes the cumulative set,
#' not the IPR scores.
#'
#' The threshold describes a proportion of total IPR, not bee coverage
#' or restoration success. Cumulative sets are summaries of the scores,
#' not prescribed planting lists.
#'
#' Diagnostics include cumulative proportions of 70%, 80% and 90%,
#' together with the requested threshold. Omission comparisons use
#' the requested threshold and retain input order for tied scores.
#' Each omission redistributes the remaining weights proportionally.
#'
#' Use \code{diagnostics = FALSE} to skip the additional diagnostics.
#' This does not change IPR scores, selection or sensitivity scenarios.
#'
#' Some network indices use random simulations. Supply a seed to
#' reproduce these calculations in the same software environment.
#' Network indices do not affect priority scores or selection.
#'
#' Coverage and bootstrap analyses are called separately with
#' \code{pollinator_coverage()} and \code{bootstrap_priority()}.
#'
#' @examples
#' data <- data.frame(
#'   plant_species = c("Plant_A", "Plant_A", "Plant_B", "Plant_C"),
#'   bee_species = c("Bee_1", "Bee_2", "Bee_1", "Bee_3"),
#'   sample_unit = c("S1", "S1", "S2", "S3")
#' )
#'
#' result <- restaurabee(data)
#'
#' result$diagnostics$ipr_accumulation$thresholds
#' result$diagnostics$component_correlations$correlation_matrix
#' result$diagnostics$component_omission$summary
#'
#' result_custom <- restaurabee(
#'   data,
#'   network_indices = NULL,
#'   weights = c(
#'     abundance = 0.20,
#'     specialization = 0.40,
#'     connectivity = 0.20,
#'     frequency = 0.20
#'   ),
#'   threshold = 0.70,
#'   ties = "include"
#' )
#'
#' @export
restaurabee <- function(
    interaction_data,
    network_indices = c(
      "connectance",
      "H2",
      "weighted NODF",
      "linkage density",
      "web asymmetry"
    ),
    seed = NULL,
    weights = c(
      abundance = 0.25,
      specialization = 0.25,
      connectivity = 0.25,
      frequency = 0.25
    ),
    threshold = 0.80,
    ties = c("first", "include"),
    diagnostics = TRUE
) {

  if (
    !is.logical(diagnostics) ||
    length(diagnostics) != 1L ||
    !is.null(dim(diagnostics)) ||
    is.na(diagnostics)
  ) {
    stop(
      "`diagnostics` must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  weights <- validate_ipr_weights(weights)
  ties <- match.arg(ties)

  if (
    !is.numeric(threshold) ||
    length(threshold) != 1L ||
    !is.null(dim(threshold)) ||
    !is.finite(threshold)
  ) {
    stop(
      "`threshold` must be a finite numeric scalar.",
      call. = FALSE
    )
  }

  if (threshold <= 0 || threshold > 1) {
    stop(
      "`threshold` must be greater than 0 and less than or equal to 1.",
      call. = FALSE
    )
  }

  if (!is.null(network_indices)) {

    if (
      !is.character(network_indices) ||
      !is.null(dim(network_indices)) ||
      length(network_indices) == 0L ||
      anyNA(network_indices) ||
      any(trimws(network_indices) == "")
    ) {
      stop(
        "`network_indices` must be a non-empty character vector or NULL.",
        call. = FALSE
      )
    }

    network_indices <- unique(network_indices)
  }

  if (!is.null(seed)) {

    if (
      !is.numeric(seed) ||
      length(seed) != 1L ||
      !is.null(dim(seed)) ||
      !is.finite(seed)
    ) {
      stop(
        "`seed` must be a non-negative integer or NULL.",
        call. = FALSE
      )
    }

    if (
      seed < 0 ||
      seed > .Machine$integer.max ||
      seed != floor(seed)
    ) {
      stop(
        "`seed` must be a non-negative integer or NULL.",
        call. = FALSE
      )
    }
  }

  interaction_data <- check_data(interaction_data)

  metadata <- list(
    observations = nrow(interaction_data),
    plant_species = length(unique(interaction_data$plant_species)),
    bee_species = length(unique(interaction_data$bee_species)),
    sampling_units = length(unique(interaction_data$sample_unit))
  )

  interaction_matrix <- build_interaction_matrix(
    interaction_data
  )

  IIP <- calculate_IIP(interaction_data)

  d_prime <- calculate_d_prime(interaction_matrix)

  plant_metrics <- dplyr::left_join(
    IIP,
    d_prime$plant_dprime,
    by = "plant_species"
  )

  prioritization <- calculate_IPR(
    plant_metrics,
    weight_abundance = weights[["abundance"]],
    weight_specialization = weights[["specialization"]],
    weight_connectivity = weights[["connectivity"]],
    weight_frequency = weights[["frequency"]]
  )

  diagnostic_results <- NULL

  if (diagnostics) {

    diagnostic_results <- list(
      ipr_accumulation = ipr_accumulation(
        prioritization,
        thresholds = sort(
          unique(c(0.70, 0.80, 0.90, threshold))
        ),
        ties = ties
      ),

      component_correlations = component_correlations(
        prioritization
      ),

      component_omission = component_omission(
        prioritization,
        weights = weights,
        threshold = threshold,
        ties = ties,
        tie_break = "input"
      )
    )
  }

  sensitivity <- tryCatch(
    sensitivity_analysis(
      prioritization,
      weights = weights
    ),

    error = function(e) {

      warning(
        "Sensitivity analysis failed: ",
        conditionMessage(e),
        call. = FALSE
      )

      base_name <- if (all(weights == 0.25)) {
        "equal"
      } else {
        "base"
      }

      scenario_names <- paste0(
        "rank_",
        c(base_name, names(weights))
      )

      correlation_matrix <- matrix(
        NA_real_,
        nrow = 5,
        ncol = 5,
        dimnames = list(
          scenario_names,
          scenario_names
        )
      )

      list(
        correlation_matrix = correlation_matrix,
        results_table = prioritization,
        scenario_weights = NULL
      )
    }
  )

  selected_species <- tryCatch(
    select_species(
      prioritization,
      threshold = threshold,
      ties = ties
    ),

    error = function(e) {

      warning(
        "Species selection failed: ",
        conditionMessage(e),
        call. = FALSE
      )

      NULL
    }
  )

  calculate_network_metrics <- function() {

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

        on.exit(
          assign(
            ".Random.seed",
            previous_seed,
            envir = globalenv()
          ),
          add = TRUE
        )

      } else {

        on.exit(
          {
            if (exists(
              ".Random.seed",
              envir = globalenv(),
              inherits = FALSE
            )) {
              rm(
                ".Random.seed",
                envir = globalenv()
              )
            }
          },
          add = TRUE
        )
      }

      set.seed(as.integer(seed))
    }

    bipartite::networklevel(
      interaction_matrix,
      index = network_indices
    )
  }

  network_metrics <- NULL

  if (!is.null(network_indices)) {

    network_metrics <- tryCatch(
      calculate_network_metrics(),

      error = function(e) {

        warning(
          "Network-level metrics failed: ",
          conditionMessage(e),
          call. = FALSE
        )

        NULL
      }
    )
  }

  result <- list(
    metadata = metadata,
    interaction_matrix = interaction_matrix,
    IIP = IIP,
    d_prime = d_prime,
    plant_metrics = plant_metrics,
    prioritization = prioritization,
    selected_species = selected_species,
    sensitivity = sensitivity,
    network_metrics = network_metrics,
    diagnostics = diagnostic_results,

    settings = list(
      weights = weights,
      threshold = threshold,
      ties = ties,
      network_indices = network_indices,
      seed = seed,
      diagnostics = diagnostics
    )
  )

  class(result) <- "restaurabee"

  result
}
