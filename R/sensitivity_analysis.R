#' Sensitivity analysis for restoration prioritization
#'
#' Evaluates the robustness of species ranking in the Interaction-based
#' Restoration Priority Index (IPR) under different weighting schemes.
#'
#' The analysis compares the default equal-weight scenario with alternative
#' scenarios that emphasize interaction abundance, species specialization,
#' connectivity, or occurrence frequency.
#'
#' @param prioritization A data frame produced by \code{calculate_IPR()},
#' containing the standardized variables \code{abundance_pad},
#' \code{d_prime_pad}, \code{connectivity_pad}, and
#' \code{frequency_pad}.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{correlation_matrix}: Spearman correlation matrix between
#'   ranking orders under different weighting scenarios.
#'   \item \code{results_table}: data frame containing the IPR values and
#'   ranks obtained under each weighting scenario.
#' }
#'
#' @export
sensitivity_analysis <- function(prioritization) {

  if (!is.data.frame(prioritization)) {
    stop(
      "'prioritization' must be a data frame.",
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
      paste(
        "Missing required columns:",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (nrow(prioritization) < 2) {
    stop(
      "'prioritization' must contain at least two species.",
      call. = FALSE
    )
  }

  prioritization_test <-
    prioritization |>
    dplyr::mutate(
      IPR_equal =
        0.25 * abundance_pad +
        0.25 * d_prime_pad +
        0.25 * connectivity_pad +
        0.25 * frequency_pad,

      IPR_abundance =
        0.40 * abundance_pad +
        0.20 * d_prime_pad +
        0.20 * connectivity_pad +
        0.20 * frequency_pad,

      IPR_specialization =
        0.20 * abundance_pad +
        0.40 * d_prime_pad +
        0.20 * connectivity_pad +
        0.20 * frequency_pad,

      IPR_connectivity =
        0.20 * abundance_pad +
        0.20 * d_prime_pad +
        0.40 * connectivity_pad +
        0.20 * frequency_pad,

      IPR_frequency =
        0.20 * abundance_pad +
        0.20 * d_prime_pad +
        0.20 * connectivity_pad +
        0.40 * frequency_pad,

      rank_equal =
        dplyr::min_rank(dplyr::desc(IPR_equal)),

      rank_abundance =
        dplyr::min_rank(dplyr::desc(IPR_abundance)),

      rank_specialization =
        dplyr::min_rank(dplyr::desc(IPR_specialization)),

      rank_connectivity =
        dplyr::min_rank(dplyr::desc(IPR_connectivity)),

      rank_frequency =
        dplyr::min_rank(dplyr::desc(IPR_frequency))
    )

  rank_table <-
    prioritization_test |>
    dplyr::select(
      rank_equal,
      rank_abundance,
      rank_specialization,
      rank_connectivity,
      rank_frequency
    )

  correlation_matrix <-
    suppressWarnings(
      stats::cor(
        rank_table,
        method = "spearman",
        use = "pairwise.complete.obs"
      )
    )

  list(
    correlation_matrix = correlation_matrix,
    results_table = prioritization_test
  )
}
