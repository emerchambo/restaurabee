#' Validate interaction data
#'
#' Checks the required columns and identifiers used by restaurabee.
#'
#' @param interaction_data A data frame containing plant-pollinator
#' interaction records.
#' @param clean_names Logical. If FALSE (default), identifiers with extra
#' whitespace are rejected. If TRUE, whitespace is standardized and a
#' warning reports changes and merged identifiers.
#'
#' @return The input data frame with the required identifier columns
#' converted to character. All records and additional columns are retained.
#'
#' @details
#' The required columns are:
#' \itemize{
#'   \item \code{plant_species}: plant species identifier.
#'   \item \code{bee_species}: bee species identifier.
#'   \item \code{sample_unit}: sampling unit identifier.
#' }
#'
#' Missing and empty identifiers are rejected. Character values, factors
#' and finite numeric codes are accepted.
#'
#' Optional cleaning replaces non-breaking spaces, removes leading and
#' trailing whitespace, and collapses repeated whitespace. Punctuation
#' and letter case are preserved. Repeated interaction records are retained.
#'
#' @keywords internal
check_data <- function(interaction_data, clean_names = FALSE) {

  if (!is.data.frame(interaction_data)) {
    stop(
      "`interaction_data` must be a data frame.",
      call. = FALSE
    )
  }

  if (
    !is.logical(clean_names) ||
    length(clean_names) != 1L ||
    is.na(clean_names)
  ) {
    stop(
      "`clean_names` must be TRUE or FALSE.",
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
    "bee_species",
    "sample_unit"
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

  if (nrow(interaction_data) == 0L) {
    stop(
      "`interaction_data` must contain at least one row.",
      call. = FALSE
    )
  }

  for (column in required_columns) {

    original <- interaction_data[[column]]

    if (
      is.null(dim(original)) &&
      (is.character(original) || is.factor(original))
    ) {

      values <- as.character(original)

    } else if (
      is.null(dim(original)) &&
      is.numeric(original) &&
      !is.object(original)
    ) {

      if (any(!is.finite(original))) {
        stop(
          "Column `", column,
          "` contains missing or non-finite identifiers.",
          call. = FALSE
        )
      }

      values <- as.character(original)

    } else {

      stop(
        "Column `", column,
        "` must contain character, factor or numeric identifiers.",
        call. = FALSE
      )
    }

    # Preserve punctuation and letter case when checking whitespace.
    cleaned <- gsub(
      "\u00a0",
      " ",
      enc2utf8(values),
      fixed = TRUE
    )

    cleaned <- gsub(
      "[[:space:]]+",
      " ",
      cleaned,
      perl = TRUE
    )

    cleaned <- trimws(cleaned)

    invalid <- is.na(cleaned) | cleaned == ""

    if (any(invalid)) {
      stop(
        "Column `", column,
        "` contains missing or empty identifiers ",
        "(first at row ", which(invalid)[1L], ").",
        call. = FALSE
      )
    }

    changed <- cleaned != values

    if (any(changed)) {

      if (!clean_names) {
        stop(
          "Column `", column,
          "` contains extra whitespace ",
          "(first at row ", which(changed)[1L], "). ",
          "Correct identifiers or use clean_names = TRUE.",
          call. = FALSE
        )
      }

      merged <- length(unique(values)) -
        length(unique(cleaned))

      warning(
        "Column `", column, "`: standardized ",
        sum(changed), " identifier(s); ",
        "distinct identifiers merged: ", merged, ".",
        call. = FALSE
      )

      values <- cleaned
    }

    interaction_data[[column]] <- values
  }

  interaction_data
}
