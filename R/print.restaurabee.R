#' Print method for restaurabee objects
#'
#' Displays a concise summary of the results produced by
#' \code{restaurabee()}.
#'
#' @param x An object of class \code{restaurabee}.
#' @param ... Additional arguments, currently ignored.
#'
#' @return The input object \code{x}, invisibly.
#'
#' @details
#' If selection failed, the output reports that selection is unavailable.
#'
#' @export
print.restaurabee <- function(x, ...) {

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
      "Selected species (80%% threshold): %d\n",
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

  if (
    !is.null(correlation_matrix) &&
    any(!is.na(correlation_matrix))
  ) {

    upper_values <- correlation_matrix[upper.tri(correlation_matrix)]

    mean_correlation <- mean(
      upper_values,
      na.rm = TRUE
    )

    cat(sprintf(
      "Mean sensitivity correlation: %.3f\n",
      mean_correlation
    ))
  }

  cat("\nDetailed results:\n")
  cat("  result$IIP\n")
  cat("  result$prioritization\n")
  cat("  result$selected_species\n")
  cat("  result$d_prime\n")
  cat("  result$sensitivity\n")
  cat("  result$network_metrics\n")

  cat("\n")

  invisible(x)
}
