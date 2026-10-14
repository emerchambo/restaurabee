#' Summary method for restaurabee objects
#'
#' Displays analysis settings, priority scores, selected plants
#' and network results.
#'
#' @param object An object of class \code{restaurabee}.
#' @param ... Additional arguments, currently ignored.
#'
#' @return The input object, invisibly.
#'
#' @details
#' If selection failed, the output reports that selection is unavailable.
#' Objects without stored settings are displayed using the original defaults.
#'
#' @export
summary.restaurabee <- function(object, ...) {

  weights <- object$settings$weights
  threshold <- object$settings$threshold
  ties <- object$settings$ties

  if (is.null(weights)) {
    weights <- c(
      abundance = 0.25,
      specialization = 0.25,
      connectivity = 0.25,
      frequency = 0.25
    )
  }

  if (is.null(threshold)) {
    threshold <- 0.80
  }

  if (is.null(ties)) {
    ties <- "first"
  }

  threshold_text <- format(
    100 * threshold,
    trim = TRUE,
    scientific = FALSE,
    digits = 10
  )

  cat("\n")
  cat("=========================================\n")
  cat("         restaurabee summary\n")
  cat("=========================================\n\n")

  cat("General information\n")
  cat("----------------------------\n")

  cat(sprintf(
    "Observations:      %d\n",
    object$metadata$observations
  ))

  cat(sprintf(
    "Plant species:     %d\n",
    object$metadata$plant_species
  ))

  cat(sprintf(
    "Bee species:       %d\n",
    object$metadata$bee_species
  ))

  cat(sprintf(
    "Sampling units:    %d\n\n",
    object$metadata$sampling_units
  ))

  cat("Analysis settings\n")
  cat("----------------------------\n")

  cat(sprintf(
    "Abundance weight:      %.4g\n",
    weights[["abundance"]]
  ))

  cat(sprintf(
    "Specialization weight: %.4g\n",
    weights[["specialization"]]
  ))

  cat(sprintf(
    "Connectivity weight:   %.4g\n",
    weights[["connectivity"]]
  ))

  cat(sprintf(
    "Frequency weight:      %.4g\n",
    weights[["frequency"]]
  ))

  cat(sprintf(
    "Selection threshold:   %s%%\n",
    threshold_text
  ))

  cat(sprintf(
    "Boundary ties:         %s\n\n",
    ties
  ))

  cat("Interaction Importance Index (Top 5)\n")
  cat("-----------------------------------------\n")

  print(
    utils::head(
      object$IIP[, c("plant_species", "IIP")],
      5
    ),
    row.names = FALSE
  )

  cat("\n")

  cat("Interaction-based Restoration Priority (Top 5)\n")
  cat("-----------------------------------------\n")

  print(
    utils::head(
      object$prioritization[, c("plant_species", "IPR")],
      5
    ),
    row.names = FALSE
  )

  cat("\n")

  cat("Selected species\n")
  cat("-----------------------------------------\n")

  if (is.null(object$selected_species)) {

    cat("Selection not available: the selection step failed.\n")

  } else {

    print(
      object$selected_species$plant_species,
      quote = FALSE
    )
  }

  cat("\n")

  if (!is.null(object$network_metrics)) {

    cat("Network metrics\n")
    cat("-----------------------------------------\n")

    metrics <- c(
      "connectance",
      "H2",
      "weighted NODF",
      "linkage density",
      "web asymmetry"
    )

    for (m in metrics) {

      if (m %in% names(object$network_metrics)) {
        cat(sprintf(
          "%-20s %.3f\n",
          paste0(m, ":"),
          unname(object$network_metrics[m])
        ))
      }
    }

    cat("\n")
  }

  cat("Sensitivity analysis\n")
  cat("-----------------------------------------\n")

  corr <- object$sensitivity$correlation_matrix

  if (!is.null(corr)) {

    correlations <- corr[upper.tri(corr)]
    correlations <- correlations[is.finite(correlations)]

  } else {

    correlations <- numeric(0)
  }

  if (length(correlations) > 0L) {

    cat(sprintf(
      "Mean correlation: %.3f\n",
      mean(correlations)
    ))

    cat(sprintf(
      "Minimum correlation: %.3f\n",
      min(correlations)
    ))

    cat(sprintf(
      "Maximum correlation: %.3f\n",
      max(correlations)
    ))

  } else {

    cat("Sensitivity results are not available.\n")
  }

  invisible(object)
}
