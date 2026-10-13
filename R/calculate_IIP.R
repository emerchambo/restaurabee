#' Calculate the Interaction Importance Index (IIP)
#'
#' Calculates the Interaction Importance Index (IIP) for plant species
#' from relative interaction abundance, occurrence frequency and connectivity.
#'
#' The IIP is the mean of these three components:
#'
#' \deqn{
#' IIP_i = \frac{1}{3}
#' \left(
#' \frac{V_i}{\sum_{j=1}^{n} V_j} +
#' \frac{O_i}{\sum_{j=1}^{n} O_j} +
#' \frac{P_i}{\sum_{j=1}^{n} P_j}
#' \right)
#' }
#'
#' where \eqn{V_i} is the number of interaction records for plant species
#' \eqn{i}, \eqn{O_i} is the number of distinct sampling units with
#' interaction records for that plant, \eqn{P_i} is the number of distinct
#' bee species recorded with it, and \eqn{n} is the number of plant species
#' in the dataset.
#'
#' @param interaction_data A data frame containing the columns
#' \code{plant_species}, \code{bee_species} and \code{sample_unit}.
#'
#' @return A data frame ordered by decreasing IIP, containing:
#' \itemize{
#'   \item \code{plant_species}: plant species identifier.
#'   \item \code{visits}: number of interaction records.
#'   \item \code{occurrences}: number of distinct sampling units with
#'   interaction records for the plant.
#'   \item \code{partners}: number of distinct bee species recorded
#'   with the plant.
#'   \item \code{abundance_relative}: visits divided by total visits.
#'   \item \code{frequency_relative}: occurrences divided by the sum
#'   of occurrences across plants.
#'   \item \code{connectivity_relative}: partners divided by the sum
#'   of partner counts across plants.
#'   \item \code{IIP}: mean of the three relative components.
#' }
#'
#' @details
#' Each row represents one interaction record and contributes to visits.
#' For each plant species, occurrences counts distinct sampling units,
#' while partners counts distinct bee species.
#'
#' A sampling unit can contribute to the occurrence count of several
#' plants. Similarly, a bee species can contribute to the partner count
#' of several plants. The denominators therefore sum the counts for
#' each plant, rather than counting unique units or bees in the dataset.
#'
#' Occurrence indicates where interactions were recorded; it does not
#' measure plant presence independently. Sampling-unit identifiers must
#' distinguish units across sites and dates when appropriate.
#'
#' Each relative component sums to one across plants, as does the IIP.
#'
#' @export
calculate_IIP <- function(interaction_data) {

  interaction_data <- check_data(interaction_data)

  # Keep the required columns and remove any pre-existing grouping.
  records <- as.data.frame(
    interaction_data[
      , c("plant_species", "bee_species", "sample_unit"),
      drop = FALSE
    ]
  )

  visits <- records |>
    dplyr::count(
      plant_species,
      name = "visits"
    )

  frequency <- records |>
    dplyr::distinct(
      sample_unit,
      plant_species
    ) |>
    dplyr::count(
      plant_species,
      name = "occurrences"
    )

  partners <- records |>
    dplyr::distinct(
      plant_species,
      bee_species
    ) |>
    dplyr::count(
      plant_species,
      name = "partners"
    )

  visits |>
    dplyr::left_join(
      frequency,
      by = "plant_species"
    ) |>
    dplyr::left_join(
      partners,
      by = "plant_species"
    ) |>
    dplyr::mutate(
      abundance_relative = visits / sum(visits),
      frequency_relative = occurrences / sum(occurrences),
      connectivity_relative = partners / sum(partners),
      IIP = (
        abundance_relative +
          frequency_relative +
          connectivity_relative
      ) / 3
    ) |>
    dplyr::arrange(dplyr::desc(IIP))
}
