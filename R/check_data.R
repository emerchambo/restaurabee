#' Validate interaction data
#'
#' Checks whether the input dataset contains the minimum required columns
#' for the restaurabee workflow and whether the input object is a data frame.
#'
#' @param interaction_data A data frame containing plant-pollinator
#' interaction records.
#'
#' @return The original data frame if all validation checks are passed.
#'
#' @details
#' The function verifies whether the input object is a data frame and whether
#' it contains the following required columns:
#' \itemize{
#'   \item \code{plant_species}: plant species identifier.
#'   \item \code{bee_species}: bee species identifier.
#'   \item \code{sample_unit}: sampling unit identifier.
#' }
#'
#' An error is returned if the input is not a data frame, if one or more
#' required columns are missing, or if the dataset has no rows.
#'
#' @keywords internal
check_data <- function(interaction_data) {

  if (!is.data.frame(interaction_data)) {
    stop("`interaction_data` must be a data frame.", call. = FALSE)
  }

  required_columns <- c("plant_species", "bee_species", "sample_unit")

  missing_columns <- setdiff(required_columns, names(interaction_data))

  if (length(missing_columns) > 0) {
    stop(
      paste(
        "Missing required columns:",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (nrow(interaction_data) == 0) {
    stop("`interaction_data` must contain at least one row.", call. = FALSE)
  }

  interaction_data
}
