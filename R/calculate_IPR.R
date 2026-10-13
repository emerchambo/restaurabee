#' Calculate the Interaction-based Restoration Priority Index (IPR)
#'
#' Calculates restoration priority scores from interaction abundance,
#' occurrence frequency, connectivity and species specialization.
#'
#' Each component is standardized using min-max scaling. By default,
#' the four components receive equal weights.
#'
#' @param plant_metrics A data frame containing the numeric columns
#' \code{abundance_relative}, \code{frequency_relative},
#' \code{connectivity_relative} and \code{d_prime}.
#' Values must be finite and between zero and one.
#' @param weight_abundance Weight assigned to interaction abundance
#' (default = 0.25).
#' @param weight_specialization Weight assigned to specialization
#' (default = 0.25).
#' @param weight_connectivity Weight assigned to connectivity
#' (default = 0.25).
#' @param weight_frequency Weight assigned to occurrence frequency
#' (default = 0.25).
#'
#' @return A data frame containing the original columns, standardized
#' components and IPR, ordered from highest to lowest IPR.
#'
#' @details
#' Components are standardized as:
#'
#' \deqn{
#' x_{pad} = \frac{x - \min(x)}{\max(x) - \min(x)}
#' }
#'
#' When a component has the same value for every plant, its standardized
#' values are set to zero. Its weight is not redistributed.
#'
#' The IPR is calculated as:
#'
#' \deqn{
#' IPR_i = w_A A_i + w_D D_i + w_C C_i + w_F F_i
#' }
#'
#' where \eqn{A_i}, \eqn{D_i}, \eqn{C_i} and \eqn{F_i} are standardized
#' interaction abundance, specialization, connectivity and occurrence
#' frequency, respectively.
#'
#' Each weight must be a finite, non-negative numeric scalar. The four
#' weights must sum to one, allowing a numerical tolerance of 1e-8.
#'
#' Scores are relative to the supplied set of plants. A zero score does
#' not necessarily mean that a plant has no ecological importance.
#'
#' @export
calculate_IPR <- function(
    plant_metrics,
    weight_abundance = 0.25,
    weight_specialization = 0.25,
    weight_connectivity = 0.25,
    weight_frequency = 0.25
) {

  if (!is.data.frame(plant_metrics)) {
    stop(
      "`plant_metrics` must be a data frame.",
      call. = FALSE
    )
  }

  if (anyDuplicated(names(plant_metrics))) {
    stop(
      "`plant_metrics` must have unique column names.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "abundance_relative",
    "frequency_relative",
    "connectivity_relative",
    "d_prime"
  )

  missing_columns <- setdiff(
    required_columns,
    names(plant_metrics)
  )

  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(plant_metrics) == 0L) {
    stop(
      "`plant_metrics` must contain at least one row.",
      call. = FALSE
    )
  }

  weights <- list(
    weight_abundance = weight_abundance,
    weight_specialization = weight_specialization,
    weight_connectivity = weight_connectivity,
    weight_frequency = weight_frequency
  )

  for (weight_name in names(weights)) {

    value <- weights[[weight_name]]

    if (
      !is.numeric(value) ||
      length(value) != 1L ||
      !is.null(dim(value)) ||
      !is.finite(value)
    ) {
      stop(
        "`", weight_name,
        "` must be a finite numeric scalar.",
        call. = FALSE
      )
    }

    if (value < 0) {
      stop(
        "`", weight_name, "` must be non-negative.",
        call. = FALSE
      )
    }
  }

  weight_sum <- sum(unlist(weights, use.names = FALSE))

  if (abs(weight_sum - 1) > 1e-8) {
    stop(
      "The four weights must sum to 1.",
      call. = FALSE
    )
  }

  for (column in required_columns) {

    values <- plant_metrics[[column]]

    if (!is.numeric(values) || !is.null(dim(values))) {
      stop(
        "Column `", column, "` must be numeric.",
        call. = FALSE
      )
    }

    if (any(!is.finite(values))) {
      stop(
        "Column `", column,
        "` must not contain NA, NaN or infinite values.",
        call. = FALSE
      )
    }

    if (any(values < 0 | values > 1)) {
      stop(
        "Column `", column,
        "` must contain values between 0 and 1.",
        call. = FALSE
      )
    }
  }

  # Standardize across all plants, regardless of prior grouping.
  plant_metrics <- as.data.frame(plant_metrics)

  scale_minmax <- function(x) {

    x_min <- min(x)
    x_max <- max(x)

    if (x_max == x_min) {
      return(rep(0, length(x)))
    }

    (x - x_min) / (x_max - x_min)
  }

  plant_metrics |>
    dplyr::mutate(
      abundance_pad = scale_minmax(abundance_relative),
      d_prime_pad = scale_minmax(d_prime),
      connectivity_pad = scale_minmax(connectivity_relative),
      frequency_pad = scale_minmax(frequency_relative),
      IPR =
        weight_abundance * abundance_pad +
        weight_specialization * d_prime_pad +
        weight_connectivity * connectivity_pad +
        weight_frequency * frequency_pad
    ) |>
    dplyr::arrange(dplyr::desc(IPR))
}
