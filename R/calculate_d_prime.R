#' Calculate species specialization (d')
#'
#' Calculates the species-level specialization index d' for plant and bee
#' species from a quantitative plant-pollinator interaction matrix.
#'
#' The index is computed using the \code{specieslevel()} function from the
#' \code{bipartite} package.
#'
#' @param interaction_matrix A numeric matrix with plants in rows and bees
#' in columns. Values must be finite and non-negative. Rows and columns
#' must have unique, non-empty species identifiers.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{plant_dprime}: data frame with plant identifiers and d' values.
#'   \item \code{bee_dprime}: data frame with bee identifiers and d' values.
#' }
#'
#' @details
#' This function requires at least two plant species and two bee species,
#' with at least one interaction recorded for every species.
#'
#' Specialization is calculated relative to the partner interaction totals
#' in the supplied matrix. Independent partner abundances are not used.
#'
#' Missing or non-finite d' values stop the calculation and identify the
#' affected species.
#'
#' @export
calculate_d_prime <- function(interaction_matrix) {

  if (
    !is.matrix(interaction_matrix) ||
    !is.numeric(interaction_matrix)
  ) {
    stop(
      "`interaction_matrix` must be a numeric matrix.",
      call. = FALSE
    )
  }

  if (
    nrow(interaction_matrix) < 2L ||
    ncol(interaction_matrix) < 2L
  ) {
    stop(
      "At least two plant species and two bee species are required.",
      call. = FALSE
    )
  }

  if (any(!is.finite(interaction_matrix))) {
    stop(
      "`interaction_matrix` must not contain NA, NaN or infinite values.",
      call. = FALSE
    )
  }

  if (any(interaction_matrix < 0)) {
    stop(
      "`interaction_matrix` must not contain negative values.",
      call. = FALSE
    )
  }

  check_identifiers <- function(ids, label) {

    if (
      is.null(ids) ||
      anyNA(ids) ||
      any(trimws(ids) == "") ||
      anyDuplicated(ids)
    ) {
      stop(
        label,
        " must be unique, non-empty species identifiers without NA.",
        call. = FALSE
      )
    }
  }

  check_identifiers(
    rownames(interaction_matrix),
    "Row names"
  )

  check_identifiers(
    colnames(interaction_matrix),
    "Column names"
  )

  plants_without_records <- rownames(interaction_matrix)[
    rowSums(interaction_matrix > 0) == 0
  ]

  bees_without_records <- colnames(interaction_matrix)[
    colSums(interaction_matrix > 0) == 0
  ]

  if (length(plants_without_records) > 0) {
    stop(
      "Plants without interaction records: ",
      paste(plants_without_records, collapse = ", "),
      call. = FALSE
    )
  }

  if (length(bees_without_records) > 0) {
    stop(
      "Bees without interaction records: ",
      paste(bees_without_records, collapse = ", "),
      call. = FALSE
    )
  }

  dp <- bipartite::specieslevel(
    interaction_matrix,
    index = "d"
  )

  plant_dprime <- data.frame(
    plant_species = rownames(dp$`lower level`),
    d_prime = dp$`lower level`$d,
    stringsAsFactors = FALSE
  )

  bee_dprime <- data.frame(
    bee_species = rownames(dp$`higher level`),
    d_prime = dp$`higher level`$d,
    stringsAsFactors = FALSE
  )

  invalid_plants <- !is.finite(plant_dprime$d_prime)
  invalid_bees <- !is.finite(bee_dprime$d_prime)

  if (any(invalid_plants)) {
    stop(
      "Non-finite d' values for plants: ",
      paste(
        plant_dprime$plant_species[invalid_plants],
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  if (any(invalid_bees)) {
    stop(
      "Non-finite d' values for bees: ",
      paste(
        bee_dprime$bee_species[invalid_bees],
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  list(
    plant_dprime = plant_dprime,
    bee_dprime = bee_dprime
  )
}
