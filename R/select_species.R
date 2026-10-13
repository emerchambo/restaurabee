#' Select priority species for restoration
#'
#' Selects plants by their cumulative contribution to the
#' Interaction-based Restoration Priority Index (IPR).
#'
#' Plants are ordered by decreasing IPR. Selection stops when the
#' cumulative proportion reaches the specified threshold.
#'
#' @param prioritization A data frame containing a numeric \code{IPR}
#' column with finite, non-negative values.
#' @param threshold Cumulative proportion of total IPR to retain.
#' Must be greater than zero and less than or equal to one.
#' Default is 0.80.
#' @param ties Treatment of ties at the selection boundary.
#' \code{"first"} (default) keeps the original behaviour, selecting
#' the first rows needed to reach the threshold.
#' \code{"include"} also selects all plants with exactly the same
#' IPR as the last selected plant.
#'
#' @return A data frame containing the selected rows and two
#' additional columns:
#' \itemize{
#'   \item \code{relative_IPR}: IPR divided by the total IPR.
#'   \item \code{cumulative_IPR}: cumulative proportion in ranking order.
#' }
#'
#' @details
#' The relative contribution is calculated as:
#'
#' \deqn{
#' relative\_IPR_i = \frac{IPR_i}{\sum_j IPR_j}
#' }
#'
#' With \code{ties = "first"}, plants with equal IPR retain their
#' input order. Their selection can therefore depend on row order.
#' With \code{ties = "include"}, all plants tied at the boundary
#' are retained, which can increase the number selected.
#'
#' A threshold of one retains all plants with positive IPR.
#' Plants with zero IPR do not contribute to the cumulative total.
#' If all scores are zero, selection cannot be calculated.
#'
#' The threshold refers to a proportion of the supplied scores,
#' not pollinator coverage or expected restoration success.
#' Min-max scores depend on the set of plants being evaluated.
#'
#' @examples
#' prioritization <- data.frame(
#'   plant_species = c("A", "B", "C"),
#'   IPR = c(0.4, 0.3, 0.3)
#' )
#'
#' select_species(prioritization, threshold = 0.5)
#' select_species(prioritization, threshold = 0.5, ties = "include")
#'
#' @export
select_species <- function(
    prioritization,
    threshold = 0.80,
    ties = c("first", "include")
) {

  ties <- match.arg(ties)

  if (
    !is.numeric(threshold) ||
    length(threshold) != 1L ||
    !is.null(dim(threshold)) ||
    !is.finite(threshold)
  ) {
    stop(
      "`threshold` must be a finite numeric scalar.",
      call. = FALSE
    )
  }

  if (threshold <= 0 || threshold > 1) {
    stop(
      "`threshold` must be greater than 0 and less than or equal to 1.",
      call. = FALSE
    )
  }

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

  if (!"IPR" %in% names(prioritization)) {
    stop(
      "`prioritization` must contain an `IPR` column.",
      call. = FALSE
    )
  }

  if (nrow(prioritization) == 0L) {
    stop(
      "`prioritization` must contain at least one row.",
      call. = FALSE
    )
  }

  scores <- prioritization$IPR

  if (!is.numeric(scores) || !is.null(dim(scores))) {
    stop(
      "`IPR` must be a numeric column.",
      call. = FALSE
    )
  }

  if (any(!is.finite(scores))) {
    stop(
      "`IPR` must not contain NA, NaN or infinite values.",
      call. = FALSE
    )
  }

  if (any(scores < 0)) {
    stop(
      "`IPR` must not contain negative values.",
      call. = FALSE
    )
  }

  total_IPR <- sum(scores)

  if (!is.finite(total_IPR) || total_IPR <= 0) {
    stop(
      "The sum of `IPR` must be finite and greater than zero.",
      call. = FALSE
    )
  }

  prioritization <- as.data.frame(prioritization)

  result <- prioritization |>
    dplyr::arrange(dplyr::desc(IPR)) |>
    dplyr::mutate(
      relative_IPR = IPR / total_IPR,
      cumulative_IPR = cumsum(relative_IPR)
    )

  last_positive <- max(which(result$IPR > 0))

  # The cumulative total is one after the last positive score.
  result$cumulative_IPR <- pmin(result$cumulative_IPR, 1)
  result$cumulative_IPR[
    seq.int(last_positive, nrow(result))
  ] <- 1

  if (threshold == 1) {

    last_species <- last_positive

  } else {

    tolerance <- 8 * .Machine$double.eps

    last_species <- which(
      result$cumulative_IPR >= threshold |
        abs(result$cumulative_IPR - threshold) <= tolerance
    )[1L]
  }

  if (ties == "include") {

    boundary_IPR <- result$IPR[last_species]

    last_species <- max(
      which(result$IPR == boundary_IPR)
    )
  }

  result |>
    dplyr::slice(seq_len(last_species))
}
