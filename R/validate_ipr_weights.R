# Validate and order the four IPR weights.
validate_ipr_weights <- function(weights) {

  components <- c(
    "abundance",
    "specialization",
    "connectivity",
    "frequency"
  )

  if (
    !is.numeric(weights) ||
    !is.null(dim(weights)) ||
    length(weights) != 4L
  ) {
    stop(
      "`weights` must be a named numeric vector of length four.",
      call. = FALSE
    )
  }

  weight_names <- names(weights)

  if (
    is.null(weight_names) ||
    anyNA(weight_names) ||
    anyDuplicated(weight_names) ||
    !setequal(weight_names, components)
  ) {
    stop(
      "`weights` must have unique names: ",
      paste(components, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  if (any(!is.finite(weights))) {
    stop(
      "`weights` must contain finite values without NA.",
      call. = FALSE
    )
  }

  if (any(weights < 0)) {
    stop(
      "`weights` must be non-negative.",
      call. = FALSE
    )
  }

  if (abs(sum(weights) - 1) > 1e-8) {
    stop(
      "The four weights must sum to 1.",
      call. = FALSE
    )
  }

  weights[components]
}
