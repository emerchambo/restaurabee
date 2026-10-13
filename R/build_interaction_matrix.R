#' Build interaction matrix
#'
#' Counts the interaction records for each plant-bee pair.
#'
#' Plants are represented in rows and bees in columns. Each cell contains
#' the number of recorded interactions, pooled across sampling units.
#' Pairs without records receive zero.
#'
#' @param interaction_data A data frame containing the columns
#' \code{plant_species} and \code{bee_species}.
#'
#' @return A numeric matrix with plant identifiers as row names and bee
#' identifiers as column names.
#'
#' @details
#' Each row represents one interaction record. Repeated records are
#' counted separately. Additional columns are ignored.
#'
#' Numeric and factor identifiers are converted to character. Missing,
#' empty identifiers and extra whitespace are rejected.
#'
#' @export
build_interaction_matrix <- function(interaction_data) {

  if (!is.data.frame(interaction_data)) {
    stop(
      "`interaction_data` must be a data frame.",
      call. = FALSE
    )
  }

  if (anyDuplicated(names(interaction_data))) {
    stop(
      "`interaction_data` must have unique column names.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "plant_species",
    "bee_species"
  )

  missing_columns <- setdiff(
    required_columns,
    names(interaction_data)
  )

  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  records <- as.data.frame(
    interaction_data[, required_columns, drop = FALSE]
  )

  # Only species identifiers are needed here. This temporary value
  # allows reuse of check_data() without requiring sampling-unit data.
  records$sample_unit <- rep("matrix", nrow(records))

  records <- check_data(records)

  pair_counts <- records |>
    dplyr::count(
      plant_species,
      bee_species,
      name = "n"
    )

  plants <- unique(pair_counts$plant_species)
  bees <- unique(pair_counts$bee_species)

  interaction_matrix <- matrix(
    0,
    nrow = length(plants),
    ncol = length(bees),
    dimnames = list(plants, bees)
  )

  positions <- cbind(
    match(pair_counts$plant_species, plants),
    match(pair_counts$bee_species, bees)
  )

  interaction_matrix[positions] <- pair_counts$n

  interaction_matrix
}
