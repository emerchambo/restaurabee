#' Summary method for restaurabee objects
#'
#' Displays a detailed summary of the results produced by
#' \code{restaurabee()}.
#'
#' @param object An object of class \code{restaurabee}.
#' @param ... Additional arguments, currently ignored.
#'
#' @return The input object, invisibly.
#'
#' @export
summary.restaurabee <- function(object, ...) {

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

  print(
    object$selected_species$plant_species,
    quote = FALSE
  )

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

  if (
    !is.null(corr) &&
    any(!is.na(corr))
  ) {

    upper <- corr[upper.tri(corr)]

    cat(sprintf(
      "Mean correlation: %.3f\n",
      mean(upper, na.rm = TRUE)
    ))

    cat(sprintf(
      "Minimum correlation: %.3f\n",
      min(upper, na.rm = TRUE)
    ))

    cat(sprintf(
      "Maximum correlation: %.3f\n",
      max(upper, na.rm = TRUE)
    ))

  } else {

    cat("Sensitivity results are not available.\n")
  }

  invisible(object)
}
