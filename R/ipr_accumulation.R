#' Describe cumulative IPR scores
#'
#' Shows how IPR scores accumulate as plants are added in decreasing
#' score order. Also reports how many plants are needed to reach
#' each requested proportion of total IPR.
#'
#' @param prioritization A data frame containing a numeric \code{IPR}
#' column with finite, non-negative values.
#' Other columns are retained in the cumulative table.
#' @param thresholds Numeric vector of proportions greater than zero
#' and at most one. Defaults to 0.70, 0.80 and 0.90.
#' @param ties Treatment of ties at each boundary:
#' \code{"first"} or \code{"include"}, passed to
#' \code{select_species()}.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{curve}: all plants ordered by decreasing IPR, with
#'   \code{k}, \code{relative_IPR} and \code{cumulative_IPR}.
#'   \item \code{thresholds}: requested proportions, number of plants
#'   (\code{k}), achieved cumulative IPR, boundary IPR, calculation
#'   status and an explanatory message when needed.
#'   \item \code{settings}: requested thresholds, tie rule and total IPR.
#' }
#'
#' @details
#' Plants with equal scores retain their input order. Threshold results
#' follow the same numerical and tie rules as \code{select_species()}.
#'
#' The cumulative proportions describe the supplied scores. They do not
#' measure bee coverage or prescribe a planting list.
#'
#' When all scores are zero, the input rows are retained, but relative
#' and cumulative proportions are undefined. Threshold sizes are
#' returned as \code{NA}, with status \code{"zero_total"}.
#' Non-finite score totals stop the calculation.
#'
#' The function returns the values needed to draw a cumulative curve;
#' it does not create a plot.
#'
#' @examples
#' prioritization <- data.frame(
#'   plant_species = c("A", "B", "C"),
#'   IPR = c(0.4, 0.3, 0.3)
#' )
#'
#' accumulated <- ipr_accumulation(
#'   prioritization,
#'   thresholds = c(0.5, 0.8, 1)
#' )
#'
#' accumulated$thresholds
#' accumulated$curve
#'
#' @export
ipr_accumulation <- function(
    prioritization,
    thresholds = c(0.70, 0.80, 0.90),
    ties = c("first", "include")
) {

  ties <- match.arg(ties)

  x <- validate_diagnostic_table(
    prioritization,
    columns = "IPR"
  )

  if (
    !is.numeric(thresholds) ||
    !is.null(dim(thresholds)) ||
    length(thresholds) == 0L ||
    any(!is.finite(thresholds)) ||
    any(thresholds <= 0 | thresholds > 1)
  ) {
    stop(
      "`thresholds` must be a non-empty numeric vector in (0, 1].",
      call. = FALSE
    )
  }

  x <- x[
    order(-x$IPR, method = "radix"),
    ,
    drop = FALSE
  ]

  rownames(x) <- NULL

  total <- sum(x$IPR)

  if (!is.finite(total)) {
    stop(
      "The sum of `IPR` must be finite.",
      call. = FALSE
    )
  }

  x$k <- seq_len(nrow(x))

  if (total > 0) {

    x$relative_IPR <- x$IPR / total

    x$cumulative_IPR <- pmin(
      cumsum(x$relative_IPR),
      1
    )

    last_positive <- max(which(x$IPR > 0))

    # The total reaches one after the last positive score.
    x$cumulative_IPR[
      seq.int(last_positive, nrow(x))
    ] <- 1

  } else {

    x$relative_IPR <- rep(NA_real_, nrow(x))
    x$cumulative_IPR <- rep(NA_real_, nrow(x))
  }

  rows <- lapply(
    thresholds,
    function(threshold) {

      selected <- if (total > 0) {

        tryCatch(
          select_species(
            x,
            threshold = threshold,
            ties = ties
          ),
          error = function(e) e
        )

      } else {

        NULL
      }

      defined <-
        !is.null(selected) &&
        !inherits(selected, "error")

      k <- if (defined) {
        nrow(selected)
      } else {
        NA_integer_
      }

      status <- if (total == 0) {
        "zero_total"
      } else if (defined) {
        "defined"
      } else {
        "selection_error"
      }

      message <- if (total == 0) {
        "Total IPR is zero."
      } else if (defined) {
        ""
      } else {
        conditionMessage(selected)
      }

      data.frame(
        threshold = threshold,
        k = k,
        achieved_cumulative_IPR = if (defined) {
          selected$cumulative_IPR[k]
        } else {
          NA_real_
        },
        boundary_IPR = if (defined) {
          selected$IPR[k]
        } else {
          NA_real_
        },
        status = status,
        message = message,
        stringsAsFactors = FALSE
      )
    }
  )

  list(
    curve = x,
    thresholds = do.call(rbind, rows),
    settings = list(
      thresholds = thresholds,
      ties = ties,
      total_IPR = total
    )
  )
}
