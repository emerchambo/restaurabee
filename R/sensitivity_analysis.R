#' Sensitivity analysis for restoration prioritization
#'
#' Compares IPR rankings under five weighting schemes using Spearman
#' rank correlations.
#'
#' The reference scenario assigns 0.25 to each component. Four alternative
#' scenarios assign 0.40 to one component and 0.20 to each of the others.
#'
#' @param prioritization A data frame produced by \code{calculate_IPR()},
#' containing the numeric columns \code{abundance_pad},
#' \code{d_prime_pad}, \code{connectivity_pad} and \code{frequency_pad}.
#' Values must be finite and between zero and one.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{correlation_matrix}: Spearman correlations between
#'   rankings under the five scenarios.
#'   \item \code{results_table}: original columns with the scores and
#'   ranks calculated for each scenario.
#' }
#'
#' @details
#' Larger scores receive smaller rank numbers. Equal scores receive
#' the same rank, using \code{dplyr::min_rank()}.
#'
#' If all species receive the same score in a scenario, its ranking
#' is constant and its correlations are undefined. These entries are
#' returned as \code{NA}, with a warning identifying the scenario.
#'
#' High correlations indicate similar rankings under the tested weights.
#' They do not measure sampling uncertainty or guarantee that the same
#' species will be selected at a cumulative threshold.
#'
#' These scenarios always use the fixed weights described above,
#' regardless of the weights used to calculate the supplied IPR.
#'
#' @export
sensitivity_analysis <- function(prioritization) {

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

  # Columns follow abundance, specialization, connectivity and frequency.
  scenario_weights <- rbind(
    equal          = c(0.25, 0.25, 0.25, 0.25),
    abundance      = c(0.40, 0.20, 0.20, 0.20),
    specialization = c(0.20, 0.40, 0.20, 0.20),
    connectivity   = c(0.20, 0.20, 0.40, 0.20),
    frequency      = c(0.20, 0.20, 0.20, 0.40)
  )

  for (scenario in rownames(scenario_weights)) {

    weights <- scenario_weights[scenario, ]

    scores <-
      weights[1] * prioritization_test$abundance_pad +
      weights[2] * prioritization_test$d_prime_pad +
      weights[3] * prioritization_test$connectivity_pad +
      weights[4] * prioritization_test$frequency_pad

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
    results_table = prioritization_test
  )
}
