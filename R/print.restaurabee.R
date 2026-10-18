#' Print method for restaurabee objects
#'
#' Displays the dataset size, analysis settings and main results.
#'
#' @param x An object of class \code{restaurabee}.
#' @param ... Additional arguments, currently ignored.
#'
#' @return The input object \code{x}, invisibly.
#'
#' @details
#' If selection failed, the output reports that selection is unavailable.
#' Objects without stored settings are displayed using the original defaults.
#' When ranking diagnostics are present, the output indicates where to
#' find them.
#'
#' @export
print.restaurabee <- function(x, ...) {

  weights <- x$settings$weights
  threshold <- x$settings$threshold
  ties <- x$settings$ties

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
  cat("         restaurabee results\n")
  cat("=========================================\n\n")

  cat(sprintf(
    "Observations:      %d\n",
    x$metadata$observations
  ))

  cat(sprintf(
    "Plant species:     %d\n",
    x$metadata$plant_species
  ))

  cat(sprintf(
    "Bee species:       %d\n",
    x$metadata$bee_species
  ))

  cat(sprintf(
    "Sampling units:    %d\n",
    x$metadata$sampling_units
  ))

  cat("\nIPR weights\n")

  cat(sprintf(
    "Abundance:        %.4g\n",
    weights[["abundance"]]
  ))

  cat(sprintf(
    "Specialization:   %.4g\n",
    weights[["specialization"]]
  ))

  cat(sprintf(
    "Connectivity:     %.4g\n",
    weights[["connectivity"]]
  ))

  cat(sprintf(
    "Frequency:        %.4g\n",
    weights[["frequency"]]
  ))

  cat(sprintf(
    "Selection threshold: %s%%\n",
    threshold_text
  ))

  cat(sprintf(
    "Boundary ties:       %s\n",
    ties
  ))

  cat("\nTop priority species\n")
  cat("----------------------------\n")

  n_top <- min(5, nrow(x$prioritization))
  top_species <- x$prioritization$plant_species[seq_len(n_top)]

  for (i in seq_along(top_species)) {
    cat(sprintf("%d. %s\n", i, top_species[i]))
  }

  cat("\n")

  if (is.null(x$selected_species)) {

    cat("Selected species: not available (selection failed).\n")

  } else {

    cat(sprintf(
      "Selected species (%s%% threshold): %d\n",
      threshold_text,
      nrow(x$selected_species)
    ))
  }

  if (!is.null(x$network_metrics)) {

    if ("connectance" %in% names(x$network_metrics)) {
      cat(sprintf(
        "Network connectance: %.3f\n",
        unname(x$network_metrics["connectance"])
      ))
    }

    if ("H2" %in% names(x$network_metrics)) {
      cat(sprintf(
        "H2': %.3f\n",
        unname(x$network_metrics["H2"])
      ))
    }
  }

  correlation_matrix <- x$sensitivity$correlation_matrix

  if (!is.null(correlation_matrix)) {

    correlations <- correlation_matrix[
      upper.tri(correlation_matrix)
    ]

    correlations <- correlations[is.finite(correlations)]

  } else {

    correlations <- numeric(0)
  }

  if (length(correlations) > 0L) {

    cat(sprintf(
      "Mean sensitivity correlation: %.3f\n",
      mean(correlations)
    ))

  } else {

    cat("Sensitivity correlations: not available.\n")
  }

  if (!is.null(x$diagnostics)) {
    cat("Ranking diagnostics: available in result$diagnostics.\n")
  }

  cat("\nDetailed results:\n")
  cat("  result$IIP\n")
  cat("  result$prioritization\n")
  cat("  result$selected_species\n")
  cat("  result$d_prime\n")
  cat("  result$sensitivity\n")
  cat("  result$network_metrics\n")

  if (!is.null(x$diagnostics)) {
    cat("  result$diagnostics\n")
  }

  cat("  result$settings\n")

  cat("\n")

  invisible(x)
}
