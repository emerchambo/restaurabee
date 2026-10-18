#' Compare IPR results after omitting each component
#'
#' Removes one component at a time and compares scores, ranks and
#' cumulative sets with a reference calculated from the supplied weights.
#'
#' @param prioritization A data frame with unique \code{plant_species}
#' identifiers and the columns \code{abundance_pad},
#' \code{d_prime_pad}, \code{connectivity_pad} and
#' \code{frequency_pad}. Component values must be finite and between
#' zero and one.
#' @param weights Named numeric vector with weights for
#' \code{abundance}, \code{specialization}, \code{connectivity}
#' and \code{frequency}. Weights must be non-negative and sum to one.
#' Defaults to 0.25 for each component.
#' @param threshold Cumulative proportion of total IPR used to compare
#' plant sets. Default is 0.80.
#' @param ties Treatment of boundary ties:
#' \code{"first"} or \code{"include"}.
#' @param tie_break \code{"input"} preserves input order for tied scores.
#' \code{"alphabetical"} orders ties by plant identifier.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{summary}: ranking correlations, changes in position,
#'   set sizes, overlap, Jaccard similarity, membership changes,
#'   calculation status and explanatory messages.
#'   \item \code{results_table}: input columns with recalculated
#'   \code{IPR}, \code{rank}, \code{rank_reference},
#'   \code{rank_change}, \code{selected} and \code{scenario}.
#'   \item \code{scenario_weights}: weights used in the reference
#'   and omission scenarios.
#'   \item \code{settings}: reference weights, threshold and tie rules.
#' }
#'
#' @details
#' Each omission sets one weight to zero and divides the remaining
#' weights by their sum. Components are not standardized again.
#' The comparisons therefore combine omission and redistribution
#' of weights.
#'
#' Reference scores are recalculated from the supplied weights.
#' If the input contains an \code{IPR} column, it is replaced in the
#' detailed results. Supply the intended reference weights when
#' calling this function directly.
#'
#' Larger scores receive smaller rank numbers. Equal scores receive
#' average ranks. Negative rank changes indicate movement toward
#' the beginning of the ranking.
#'
#' Omitting the only component with positive weight leaves no weights
#' to redistribute. This scenario receives status
#' \code{"no_remaining_weight"}, with undefined scores and selection.
#'
#' When total IPR is zero, scores and ranks are retained, but selection
#' is undefined and status is \code{"zero_total"}.
#' Constant rankings have undefined correlations.
#'
#' Jaccard similarity is the number of shared selected plants divided
#' by the number in the union of the two sets. Membership changes are
#' displayed as semicolon-separated identifiers. The logical
#' \code{selected} column records membership for each plant.
#'
#' No scenario is treated as an optimal weighting scheme.
#'
#' @examples
#' prioritization <- data.frame(
#'   plant_species = c("A", "B", "C", "D"),
#'   abundance_pad = c(1, 0.7, 0.2, 0),
#'   d_prime_pad = c(0.1, 0.8, 0.2, 1),
#'   connectivity_pad = c(0, 0.5, 1, 0.2),
#'   frequency_pad = c(1, 0.6, 0.1, 0)
#' )
#'
#' omission <- component_omission(prioritization)
#'
#' omission$summary
#' omission$scenario_weights
#'
#' @export
component_omission <- function(
    prioritization,
    weights = c(
      abundance = 0.25,
      specialization = 0.25,
      connectivity = 0.25,
      frequency = 0.25
    ),
    threshold = 0.80,
    ties = c("first", "include"),
    tie_break = c("input", "alphabetical")
) {

  weights <- validate_ipr_weights(weights)

  ties <- match.arg(ties)
  tie_break <- match.arg(tie_break)

  if (
    !is.numeric(threshold) ||
    length(threshold) != 1L ||
    !is.null(dim(threshold)) ||
    !is.finite(threshold) ||
    threshold <= 0 ||
    threshold > 1
  ) {
    stop(
      "`threshold` must be a finite number in (0, 1].",
      call. = FALSE
    )
  }

  columns <- ipr_component_columns()

  x <- validate_diagnostic_table(
    prioritization,
    columns = unname(columns),
    species = TRUE
  )

  x$plant_species <- as.character(x$plant_species)

  if (tie_break == "alphabetical") {
    x <- x[
      order(x$plant_species),
      ,
      drop = FALSE
    ]
  }

  scenarios <- c(
    "reference",
    paste0("without_", names(weights))
  )

  scenario_weights <- matrix(
    NA_real_,
    nrow = length(scenarios),
    ncol = length(weights),
    dimnames = list(
      scenarios,
      names(weights)
    )
  )

  scenario_weights["reference", ] <- weights

  for (component in names(weights)) {

    remaining <- weights
    remaining[[component]] <- 0

    if (sum(remaining) > 0) {

      scenario_weights[
        paste0("without_", component),
      ] <- remaining / sum(remaining)
    }
  }

  details <- lapply(
    scenarios,
    function(scenario) {

      w <- scenario_weights[scenario, ]

      out <- x
      out$scenario <- scenario

      if (anyNA(w)) {

        out$IPR <- rep(NA_real_, nrow(out))
        out$rank <- rep(NA_real_, nrow(out))
        out$selected <- rep(NA, nrow(out))

        return(list(
          data = out,
          status = "no_remaining_weight",
          message = "No positive weight remains."
        ))
      }

      out$IPR <-
        w[["abundance"]] * out$abundance_pad +
        w[["specialization"]] * out$d_prime_pad +
        w[["connectivity"]] * out$connectivity_pad +
        w[["frequency"]] * out$frequency_pad

      out$rank <- rank(
        -out$IPR,
        ties.method = "average"
      )

      out$selected <- rep(NA, nrow(out))

      if (sum(out$IPR) == 0) {

        return(list(
          data = out,
          status = "zero_total",
          message = "Total IPR is zero."
        ))
      }

      selected <- tryCatch(
        select_species(
          out,
          threshold = threshold,
          ties = ties
        ),
        error = function(e) e
      )

      if (inherits(selected, "error")) {

        return(list(
          data = out,
          status = "selection_error",
          message = conditionMessage(selected)
        ))
      }

      out$selected <- out$plant_species %in%
        selected$plant_species

      list(
        data = out,
        status = "defined",
        message = ""
      )
    }
  )

  reference <- details[[1L]]$data

  reference_set <- reference$plant_species[
    which(reference$selected)
  ]

  reference_defined <-
    details[[1L]]$status == "defined"

  for (i in seq_along(details)) {

    current <- details[[i]]$data

    current$rank_reference <- reference$rank

    current$rank_change <-
      current$rank - reference$rank

    details[[i]]$data <- current
  }

  summary_rows <- lapply(
    seq_along(details),
    function(i) {

      current <- details[[i]]$data

      comparable <- all(is.finite(current$rank))

      delta <- if (comparable) {
        current$rank - reference$rank
      } else {
        rep(NA_real_, nrow(x))
      }

      defined <- details[[i]]$status == "defined"

      current_set <- current$plant_species[
        which(current$selected)
      ]

      overlap_defined <- defined && reference_defined

      union_size <- length(
        union(reference_set, current_set)
      )

      data.frame(
        scenario = scenarios[i],

        spearman_vs_reference = if (comparable) {
          diagnostic_rank_correlation(
            current$rank,
            reference$rank,
            method = "spearman"
          )
        } else {
          NA_real_
        },

        kendall_vs_reference = if (comparable) {
          diagnostic_rank_correlation(
            current$rank,
            reference$rank,
            method = "kendall"
          )
        } else {
          NA_real_
        },

        mean_absolute_rank_change = if (comparable) {
          mean(abs(delta))
        } else {
          NA_real_
        },

        max_absolute_rank_change = if (comparable) {
          max(abs(delta))
        } else {
          NA_real_
        },

        selection_defined = defined,

        n_selected = if (defined) {
          length(current_set)
        } else {
          NA_integer_
        },

        n_shared = if (overlap_defined) {
          length(intersect(reference_set, current_set))
        } else {
          NA_integer_
        },

        jaccard = if (
          overlap_defined &&
          union_size > 0L
        ) {
          length(intersect(reference_set, current_set)) /
            union_size
        } else {
          NA_real_
        },

        added = if (overlap_defined) {
          paste(
            setdiff(current_set, reference_set),
            collapse = "; "
          )
        } else {
          NA_character_
        },

        removed = if (overlap_defined) {
          paste(
            setdiff(reference_set, current_set),
            collapse = "; "
          )
        } else {
          NA_character_
        },

        status = details[[i]]$status,
        message = details[[i]]$message,
        stringsAsFactors = FALSE
      )
    }
  )

  results <- do.call(
    rbind,
    lapply(details, function(scenario) scenario$data)
  )

  rownames(results) <- NULL

  list(
    summary = do.call(rbind, summary_rows),
    results_table = results,
    scenario_weights = scenario_weights,
    settings = list(
      weights = weights,
      threshold = threshold,
      ties = ties,
      tie_break = tie_break
    )
  )
}
