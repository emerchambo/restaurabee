#' Ecological Restoration Prioritization Using Plant-Pollinator Networks
#'
#' Runs the complete restoration prioritization workflow based on
#' plant-pollinator interaction networks.
#'
#' The workflow calculates species importance through the Interaction
#' Importance Index (IIP), species specialization (d'), restoration
#' priority scores (IPR), sensitivity analyses, and network-level
#' descriptors.
#'
#' The IPR integrates standardized ecological components, including
#' interaction abundance, occurrence frequency, connectivity, and species
#' specialization quantified by the d' index, allowing the identification
#' of plant species with the highest restoration priority.
#'
#' @param interaction_data A data frame containing:
#' \describe{
#'   \item{plant_species}{Plant species identifier.}
#'   \item{bee_species}{Bee species identifier.}
#'   \item{sample_unit}{Sampling unit identifier.}
#' }
#'
#' @return An object of class \code{restaurabee} containing:
#' \itemize{
#'   \item \code{metadata}: summary of observations, plant species,
#'   bee species, and sampling units.
#'   \item \code{interaction_matrix}: plant-pollinator interaction matrix.
#'   \item \code{IIP}: Interaction Importance Index results.
#'   \item \code{d_prime}: species specialization (d') for plants and bees.
#'   \item \code{plant_metrics}: merged ecological metrics per plant species.
#'   \item \code{prioritization}: Interaction-based Restoration Priority Index.
#'   \item \code{selected_species}: subset of priority species based on
#'   cumulative IPR.
#'   \item \code{sensitivity}: sensitivity analysis of ranking stability.
#'   \item \code{network_metrics}: network-level descriptors computed with
#'   \code{bipartite}.
#' }
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
#' @export
restaurabee <- function(interaction_data) {

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
      warning("Species selection failed: ", e$message)
      prioritization
    }
  )

  network_metrics <- tryCatch(
    bipartite::networklevel(interaction_matrix),
    error = function(e) {
      warning("Network-level metrics failed: ", e$message)
      NULL
    }
  )

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
