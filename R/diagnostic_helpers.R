# Check the input used by the diagnostic functions.
validate_diagnostic_table <- function(
    x,
    columns,
    species = FALSE
) {

  if (!is.data.frame(x)) {
    stop(
      "`prioritization` must be a data frame.",
      call. = FALSE
    )
  }

  if (anyDuplicated(names(x))) {
    stop(
      "`prioritization` must have unique column names.",
      call. = FALSE
    )
  }

  required <- c(
    if (species) "plant_species",
    columns
  )

  missing_columns <- setdiff(required, names(x))

  if (length(missing_columns) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(x) == 0L) {
    stop(
      "`prioritization` must contain at least one row.",
      call. = FALSE
    )
  }

  if (species) {

    ids <- x$plant_species

    if (is.factor(ids)) {
      ids <- as.character(ids)
    }

    if (
      !is.character(ids) ||
      !is.null(dim(ids)) ||
      anyNA(ids) ||
      any(trimws(ids) == "") ||
      anyDuplicated(ids)
    ) {
      stop(
        "`plant_species` must contain unique, non-empty identifiers.",
        call. = FALSE
      )
    }
  }

  for (column in columns) {

    values <- x[[column]]

    if (
      !is.numeric(values) ||
      !is.null(dim(values)) ||
      any(!is.finite(values)) ||
      any(values < 0)
    ) {
      stop(
        "Column `", column,
        "` must contain finite, non-negative numeric values.",
        call. = FALSE
      )
    }

    if (column != "IPR" && any(values > 1)) {
      stop(
        "Column `", column,
        "` must contain values between 0 and 1.",
        call. = FALSE
      )
    }
  }

  as.data.frame(x)
}


# Match component names to their standardized columns.
ipr_component_columns <- function() {

  c(
    abundance = "abundance_pad",
    specialization = "d_prime_pad",
    connectivity = "connectivity_pad",
    frequency = "frequency_pad"
  )
}


# Return NA when a rank correlation cannot be calculated.
diagnostic_rank_correlation <- function(x, y, method) {

  if (
    length(x) < 2L ||
    length(unique(x)) < 2L ||
    length(unique(y)) < 2L
  ) {
    return(NA_real_)
  }

  unname(
    stats::cor(
      x,
      y,
      method = method
    )
  )
}
