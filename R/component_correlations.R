#' Examine associations among IPR components
#'
#' Calculates Spearman correlations among the four standardized
#' components used to calculate IPR.
#'
#' @param prioritization A data frame containing the numeric columns
#' \code{abundance_pad}, \code{d_prime_pad},
#' \code{connectivity_pad} and \code{frequency_pad}.
#' Values must be finite and between zero and one.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{correlation_matrix}: Spearman correlations among
#'   abundance, specialization, connectivity and occurrence frequency.
#'   \item \code{pairwise}: the six component pairs, their correlations
#'   and calculation status.
#'   \item \code{constant_components}: names of components with the
#'   same value for every plant.
#'   \item \code{n_plants}: number of rows used in the calculation.
#' }
#'
#' @details
#' Each row should represent one plant species.
#' Correlations describe associations across the plants supplied.
#' They do not measure the independent contribution or ecological
#' value of a component.
#'
#' Constant components have undefined correlations, including their
#' diagonal entries. These values are returned as \code{NA}.
#' With only one plant, all correlations are undefined.
#'
#' The pairwise status is \code{"defined"},
#' \code{"constant_component"} or \code{"insufficient_plants"}.
#' Undefined correlations are reported without generating warnings.
#'
#' No correlation cutoff or automatic adjustment of weights is applied.
#' The input scores are not changed.
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
#' associations <- component_correlations(prioritization)
#'
#' associations$correlation_matrix
#' associations$pairwise
#'
#' @export
component_correlations <- function(prioritization) {

  columns <- ipr_component_columns()

  x <- validate_diagnostic_table(
    prioritization,
    columns = unname(columns)
  )

  values <- x[
    ,
    unname(columns),
    drop = FALSE
  ]

  names(values) <- names(columns)

  varying <- vapply(
    values,
    function(component) {
      length(unique(component)) > 1L
    },
    logical(1)
  )

  correlation_matrix <- matrix(
    NA_real_,
    nrow = length(columns),
    ncol = length(columns),
    dimnames = list(
      names(columns),
      names(columns)
    )
  )

  if (any(varying)) {

    correlation_matrix[varying, varying] <- stats::cor(
      values[
        ,
        varying,
        drop = FALSE
      ],
      method = "spearman"
    )
  }

  pairs <- utils::combn(
    names(columns),
    2L
  )

  pairwise <- data.frame(
    component_1 = pairs[1L, ],
    component_2 = pairs[2L, ],
    spearman = vapply(
      seq_len(ncol(pairs)),
      function(i) {

        correlation_matrix[
          pairs[1L, i],
          pairs[2L, i]
        ]
      },
      numeric(1)
    ),
    stringsAsFactors = FALSE
  )

  if (nrow(x) < 2L) {

    pairwise$status <- "insufficient_plants"

  } else {

    pairwise$status <- ifelse(
      is.na(pairwise$spearman),
      "constant_component",
      "defined"
    )
  }

  list(
    correlation_matrix = correlation_matrix,
    pairwise = pairwise,
    constant_components = names(columns)[!varying],
    n_plants = nrow(x)
  )
}
