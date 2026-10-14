#' Pollinator coverage of selected plant species
#'
#' Calculates the proportion of recorded bee species that interact with
#' at least one of the selected plant species.
#'
#' @param plant_species Character vector with the identifiers of the
#' selected plants. Identifiers must match the matrix row names.
#' Use \code{character(0)} to evaluate an empty selection.
#' @param interaction_matrix Numeric matrix with plants in rows, bees
#' in columns and interaction counts as values. Values must be finite
#' and non-negative. Row and column names must be unique and non-empty.
#'
#' @return A list containing:
#' \itemize{
#'   \item \code{coverage}: proportion of recorded bee species represented
#'   by the selected plants, between zero and one.
#'   \item \code{n_plants}: number of distinct selected plants.
#'   \item \code{n_bees_total}: number of bee species recorded in the matrix.
#'   \item \code{n_bees_covered}: number of bee species that interact with
#'   at least one selected plant.
#'   \item \code{covered}: identifiers of the represented bee species.
#'   \item \code{not_covered}: identifiers of the remaining bee species.
#' }
#'
#' @details
#' A bee species is counted once, regardless of how many selected plants
#' it interacts with or how many interactions were recorded.
#'
#' Columns containing only zeros are excluded from the total number of
#' recorded bee species. Repeated plant identifiers are counted once.
#' An empty selection returns zero coverage.
#'
#' Coverage describes the interactions recorded in the supplied matrix.
#' It does not estimate sampling completeness or predict whether the
#' selected plants will maintain bee populations after restoration.
#'
#' @examples
#' interaction_matrix <- matrix(
#'   c(
#'     2, 1, 0,
#'     0, 3, 0,
#'     0, 0, 1
#'   ),
#'   nrow = 3,
#'   byrow = TRUE,
#'   dimnames = list(
#'     c("Plant_A", "Plant_B", "Plant_C"),
#'     c("Bee_1", "Bee_2", "Bee_3")
#'   )
#' )
#'
#' pollinator_coverage(
#'   plant_species = c("Plant_A", "Plant_B"),
#'   interaction_matrix = interaction_matrix
#' )
#'
#' @export
pollinator_coverage <- function(
    plant_species,
    interaction_matrix
) {

  if (
    !is.matrix(interaction_matrix) ||
    !is.numeric(interaction_matrix)
  ) {
    stop(
      "`interaction_matrix` must be a numeric matrix.",
      call. = FALSE
    )
  }

  if (
    nrow(interaction_matrix) == 0L ||
    ncol(interaction_matrix) == 0L
  ) {
    stop(
      "`interaction_matrix` must have at least one row and one column.",
      call. = FALSE
    )
  }

  if (any(!is.finite(interaction_matrix))) {
    stop(
      "`interaction_matrix` must not contain NA, NaN or infinite values.",
      call. = FALSE
    )
  }

  if (any(interaction_matrix < 0)) {
    stop(
      "`interaction_matrix` must contain non-negative values.",
      call. = FALSE
    )
  }

  validate_identifiers <- function(identifiers, axis) {

    if (
      is.null(identifiers) ||
      anyNA(identifiers) ||
      any(trimws(identifiers) == "")
    ) {
      stop(
        "`interaction_matrix` must have non-empty ",
        axis,
        " names without missing values.",
        call. = FALSE
      )
    }

    if (anyDuplicated(identifiers)) {
      stop(
        "`interaction_matrix` must have unique ",
        axis,
        " names.",
        call. = FALSE
      )
    }

    if (any(identifiers != trimws(identifiers))) {
      stop(
        "`interaction_matrix` ",
        axis,
        " names must not contain leading or trailing whitespace.",
        call. = FALSE
      )
    }
  }

  validate_identifiers(
    rownames(interaction_matrix),
    "row"
  )

  validate_identifiers(
    colnames(interaction_matrix),
    "column"
  )

  if (
    !is.character(plant_species) ||
    !is.null(dim(plant_species))
  ) {
    stop(
      "`plant_species` must be a character vector.",
      call. = FALSE
    )
  }

  if (
    anyNA(plant_species) ||
    any(trimws(plant_species) == "")
  ) {
    stop(
      "`plant_species` must not contain missing or empty identifiers.",
      call. = FALSE
    )
  }

  if (any(plant_species != trimws(plant_species))) {
    stop(
      "`plant_species` must not contain leading or trailing whitespace.",
      call. = FALSE
    )
  }

  plant_species <- unique(plant_species)

  unknown_plants <- setdiff(
    plant_species,
    rownames(interaction_matrix)
  )

  if (length(unknown_plants) > 0L) {
    stop(
      "Selected plants not found in `interaction_matrix`: ",
      paste(unknown_plants, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  recorded_interactions <- interaction_matrix > 0

  recorded_bees <- colSums(recorded_interactions) > 0

  n_bees_total <- sum(recorded_bees)

  if (n_bees_total == 0L) {
    stop(
      "`interaction_matrix` must contain at least one positive interaction.",
      call. = FALSE
    )
  }

  represented_bees <- colSums(
    recorded_interactions[
      plant_species,
      ,
      drop = FALSE
    ]
  ) > 0

  bee_names <- colnames(interaction_matrix)

  covered <- bee_names[
    recorded_bees & represented_bees
  ]

  not_covered <- bee_names[
    recorded_bees & !represented_bees
  ]

  list(
    coverage = length(covered) / n_bees_total,
    n_plants = length(plant_species),
    n_bees_total = n_bees_total,
    n_bees_covered = length(covered),
    covered = covered,
    not_covered = not_covered
  )
}
