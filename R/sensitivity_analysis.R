#' Sensitivity analysis for restoration prioritization
#'
#' Checks how the order of plant species changes when the IPR weights
#' are changed. Rankings are compared using Spearman correlations.
#'
#' @param prioritization A data frame produced by \code{calculate_IPR()},
#' with the columns \code{abundance_pad}, \code{d_prime_pad},
#' \code{connectivity_pad} and \code{frequency_pad}.
#' These columns must contain numbers between zero and one,
#' without missing or infinite values.
#' @param weights Named numeric vector with weights for \code{abundance},
#' \code{specialization}, \code{connectivity} and \code{frequency}.
#' Weights must be non-negative and sum to one.
#' The default is 0.25 for each component.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{correlation_matrix}: Spearman correlations between
#'   the rankings.
#'   \item \code{results_table}: the input data with IPR scores and
#'   ranks for each set of weights.
#'   \item \code{scenario_weights}: the weights used in each comparison.
#' }
#'
#' @details
#' The analysis starts with the weights supplied by the user. It then
#' increases one weight at a time by 0.15, up to a maximum of one.
#' The other weights are reduced in proportion to their starting values.
#'
#' With the default weights, each comparison uses 0.40 for one component
#' and 0.20 for the other three. If a component already has weight one,
#' increasing that weight leaves the original ranking unchanged.
#'
#' The starting scores and ranks are named \code{IPR_equal} and
#' \code{rank_equal} when all weights are 0.25. For other weights,
#' they are named \code{IPR_base} and \code{rank_base}.
#'
#' Species with larger scores come first. Equal scores receive the
#' same rank, following \code{dplyr::min_rank()}.
#'
#' When every species has the same score, the correlation cannot be
#' calculated. The function returns \code{NA} for those comparisons
#' and gives a warning.
#'
#' High correlations mean that the tested weights give similar rankings.
#' They do not describe sampling uncertainty or ensure that the same
#' plants will be selected.
#'
#' When calling this function directly, supply the weights used to
#' calculate the original IPR.
#'
#' @export
sensitivity_analysis <- function(
    prioritization,
    weights = c(
      abundance = 0.25,
      specialization = 0.25,
      connectivity = 0.25,
      frequency = 0.25
    )
) {

  weights <- validate_ipr_weights(weights)

  if (!is.data.frame(prioritization)) {
    stop(
      "`prioritization` must be a data frame.",
      call. = FALSE
    )
  }

  if (anyDuplicated(names(prioritization))) {
    stop(
      "`prioritization` must have unique column names.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "abundance_pad",
    "d_prime_pad",
    "connectivity_pad",
    "frequency_pad"
  )

  missing_columns <- setdiff(
    required_columns,
    names(prioritization)
  )

  if (length(missing_columns) > 0) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  if (nrow(prioritization) < 2L) {
    stop(
      "`prioritization` must contain at least two species.",
      call. = FALSE
    )
  }

  for (column in required_columns) {

    values <- prioritization[[column]]

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

  prioritization_test <- as.data.frame(prioritization)

  base_name <- if (all(weights == 0.25)) {
    "equal"
  } else {
    "base"
  }

  components <- names(weights)

  scenario_weights <- matrix(
    NA_real_,
    nrow = 5,
    ncol = 4,
    dimnames = list(
      c(base_name, components),
      components
    )
  )

  scenario_weights[base_name, ] <- weights

  for (component in components) {

    alternative <- weights
    increased_weight <- min(1, weights[[component]] + 0.15)
    others <- setdiff(components, component)
    remaining_weight <- sum(weights[others])

    alternative[[component]] <- increased_weight

    if (remaining_weight > 0) {

      alternative[others] <-
        weights[others] * (1 - increased_weight) / remaining_weight

    } else {

      alternative[others] <- 0
    }

    scenario_weights[component, ] <- alternative
  }

  for (scenario in rownames(scenario_weights)) {

    w <- scenario_weights[scenario, ]

    scores <-
      w[["abundance"]] * prioritization_test$abundance_pad +
      w[["specialization"]] * prioritization_test$d_prime_pad +
      w[["connectivity"]] * prioritization_test$connectivity_pad +
      w[["frequency"]] * prioritization_test$frequency_pad

    prioritization_test[[paste0("IPR_", scenario)]] <- scores
  }

  for (scenario in rownames(scenario_weights)) {

    score_column <- paste0("IPR_", scenario)
    rank_column <- paste0("rank_", scenario)

    prioritization_test[[rank_column]] <- dplyr::min_rank(
      dplyr::desc(prioritization_test[[score_column]])
    )
  }

  rank_columns <- paste0("rank_", rownames(scenario_weights))

  rank_table <- prioritization_test[
    , rank_columns,
    drop = FALSE
  ]

  varying <- vapply(
    rank_table,
    function(x) length(unique(x)) > 1L,
    logical(1)
  )

  correlation_matrix <- matrix(
    NA_real_,
    nrow = length(rank_columns),
    ncol = length(rank_columns),
    dimnames = list(rank_columns, rank_columns)
  )

  if (any(varying)) {
    correlation_matrix[varying, varying] <- stats::cor(
      rank_table[, varying, drop = FALSE],
      method = "spearman"
    )
  }

  if (any(!varying)) {
    warning(
      "Undefined correlations for constant rankings: ",
      paste(rank_columns[!varying], collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  list(
    correlation_matrix = correlation_matrix,
    results_table = prioritization_test,
    scenario_weights = scenario_weights
  )
}
