#' Ecological Restoration Prioritization Using Plant-Pollinator Networks
#'
#' Runs the restoration prioritization workflow from plant-pollinator
#' interaction records.
#'
#' Calculates the Interaction Importance Index (IIP), species
#' specialization (d'), restoration priority scores (IPR),
#' species selection, sensitivity analysis and network descriptors.
#'
#' @param interaction_data A data frame containing:
#' \describe{
#'   \item{plant_species}{Plant species identifier.}
#'   \item{bee_species}{Bee species identifier.}
#'   \item{sample_unit}{Sampling unit identifier.}
#' }
#' @param network_indices Character vector of indices passed to
#' \code{bipartite::networklevel()}. By default, calculates connectance,
#' H2, weighted NODF, linkage density and web asymmetry.
#' Use \code{"ALLBUTDD"} for the broader set provided by bipartite,
#' or \code{NULL} to skip network metrics.
#' @param seed Optional non-negative integer seed for network metrics.
#' The previous random-number state is restored afterwards.
#'
#' @return An object of class \code{restaurabee} containing:
#' \itemize{
#'   \item \code{metadata}: numbers of records, plants, bees and sampling units.
#'   \item \code{interaction_matrix}: plant-by-bee interaction matrix.
#'   \item \code{IIP}: Interaction Importance Index results.
#'   \item \code{d_prime}: specialization values for plants and bees.
#'   \item \code{plant_metrics}: ecological metrics for each plant.
#'   \item \code{prioritization}: IPR scores and standardized components.
#'   \item \code{selected_species}: plants selected by cumulative IPR,
#'   or \code{NULL} if selection fails.
#'   \item \code{sensitivity}: comparisons among weighting scenarios.
#'   \item \code{network_metrics}: requested network descriptors, or
#'   \code{NULL} if skipped or if their calculation fails.
#' }
#'
#' @details
#' Some additional network indices use random simulations. Supply a seed
#' when requesting these indices to reproduce results in the same
#' software environment.
#'
#' Changing network indices does not change IIP, d', IPR or species
#' selection. Additional metrics are available in \code{network_metrics},
#' even when they are not displayed by \code{print()} or \code{summary()}.
#'
#' @examples
#' data <- data.frame(
#'   plant_species = c("Plant_A", "Plant_A", "Plant_B", "Plant_C"),
#'   bee_species = c("Bee_1", "Bee_2", "Bee_1", "Bee_3"),
#'   sample_unit = c("S1", "S1", "S2", "S3")
#' )
#'
#' result <- restaurabee(data)
#' result_without_metrics <- restaurabee(data, network_indices = NULL)
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
    seed = NULL
) {

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

  interaction_matrix <- build_interaction_matrix(interaction_data)

  IIP <- calculate_IIP(interaction_data)

  d_prime <- calculate_d_prime(interaction_matrix)

  plant_metrics <- dplyr::left_join(
    IIP,
    d_prime$plant_dprime,
    by = "plant_species"
  )

  prioritization <- calculate_IPR(plant_metrics)

  sensitivity <- tryCatch(
    sensitivity_analysis(prioritization),
    error = function(e) {
      warning("Sensitivity analysis failed: ", e$message)

      scenario_names <- c(
        "rank_equal",
        "rank_abundance",
        "rank_specialization",
        "rank_connectivity",
        "rank_frequency"
      )

      correlation_matrix <- matrix(
        NA_real_,
        nrow = 5,
        ncol = 5,
        dimnames = list(scenario_names, scenario_names)
      )

      list(
        correlation_matrix = correlation_matrix,
        results_table = prioritization
      )
    }
  )

  selected_species <- tryCatch(
    select_species(prioritization),
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
              rm(".Random.seed", envir = globalenv())
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
    network_metrics = network_metrics
  )

  class(result) <- "restaurabee"

  result
}
