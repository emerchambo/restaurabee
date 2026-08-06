#' Build interaction matrix
#'
#' Converts a plant-pollinator interaction dataset into a quantitative
#' bipartite interaction matrix suitable for ecological network analyses.
#'
#' The resulting matrix contains plant species as rows, bee species as
#' columns, and the total number of observed interactions between each
#' plant-bee pair as cell values, regardless of sampling unit.
#'
#' This quantitative interaction matrix serves as the basis for calculating
#' species specialization, network-level metrics, and other ecological
#' descriptors throughout the restaurabee workflow.
#'
#' @param interaction_data A data frame containing at least the columns
#' \code{plant_species} and \code{bee_species}.
#'
#' @return A quantitative interaction matrix with plant species in rows,
#' bee species in columns, and interaction frequencies as values.
#'
#' @export

build_interaction_matrix <- function(interaction_data) {

  interaction_matrix <-
    interaction_data |>
    dplyr::count(
      plant_species,
      bee_species,
      name = "n"
    ) |>
    tidyr::pivot_wider(
      names_from = bee_species,
      values_from = n,
      values_fill = 0
    )

  if (nrow(interaction_matrix) == 0) {
    stop(
      "No interactions available to build the interaction matrix.",
      call. = FALSE
    )
  }

  interaction_matrix <- as.data.frame(interaction_matrix)

  rownames(interaction_matrix) <- interaction_matrix$plant_species

  as.matrix(
    interaction_matrix[, -1, drop = FALSE]
  )
}
