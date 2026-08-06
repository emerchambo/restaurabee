#' Calculate species specialization (d')
#'
#' Calculates the species-level specialization index d' for plant and bee
#' species based on a quantitative plant-pollinator interaction matrix.
#'
#' The index is computed using the \code{specieslevel()} function from the
#' \code{bipartite} package.
#'
#' @param interaction_matrix A quantitative interaction matrix with plant
#' species in rows and bee species in columns.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{plant_dprime}: data frame with plant species and their d' values.
#'   \item \code{bee_dprime}: data frame with bee species and their d' values.
#' }
#'
#' @export
calculate_d_prime <- function(interaction_matrix) {

  dp <- bipartite::specieslevel(
    interaction_matrix,
    index = "d"
  )

  list(
    plant_dprime = data.frame(
      plant_species = rownames(dp$`lower level`),
      d_prime = dp$`lower level`$d
    ),
    bee_dprime = data.frame(
      bee_species = rownames(dp$`higher level`),
      d_prime = dp$`higher level`$d
    )
  )
}
