#' Calculate the Interaction Importance Index (IIP)
#'
#' Calculates the Interaction Importance Index (IIP) for plant species based on
#' three components derived from plant-pollinator interaction data: relative
#' interaction abundance, relative occurrence frequency, and relative
#' connectivity.
#'
#' The IIP is calculated as the mean of three relative components:
#'
#' \deqn{
#' IIP_i =
#' \frac{
#' \left(
#' \frac{V_i}{\sum V_i} +
#' \frac{O_i}{\sum O_i} +
#' \frac{P_i}{\sum P_i}
#' \right)
#' }{3}
#' }
#'
#' where \eqn{V_i} is the total number of recorded interactions for plant
#' species \eqn{i}, \eqn{O_i} is the number of sampling units in which plant
#' species \eqn{i} occurred, and \eqn{P_i} is the number of bee species
#' interacting with plant species \eqn{i}.
#'
#' @param interaction_data A data frame containing at least the columns
#' \code{plant_species}, \code{bee_species}, and \code{sample_unit}.
#'
#' @return A data frame containing:
#' \itemize{
#'   \item \code{plant_species}: plant species name.
#'   \item \code{visits}: total number of recorded interactions.
#'   \item \code{occurrences}: number of sampling units where the species occurred.
#'   \item \code{partners}: number of interacting bee species.
#'   \item \code{abundance_relative}: relative interaction abundance.
#'   \item \code{frequency_relative}: relative occurrence frequency.
#'   \item \code{connectivity_relative}: relative number of pollinator partners.
#'   \item \code{IIP}: Interaction Importance Index.
#' }
#'
#' @export
calculate_IIP <- function(interaction_data) {

  visits <- interaction_data |>
    dplyr::count(plant_species, name = "visits")

  frequency <- interaction_data |>
    dplyr::distinct(sample_unit, plant_species) |>
    dplyr::count(plant_species, name = "occurrences")

  partners <- interaction_data |>
    dplyr::distinct(plant_species, bee_species) |>
    dplyr::count(plant_species, name = "partners")

  visits |>
    dplyr::left_join(frequency, by = "plant_species") |>
    dplyr::left_join(partners, by = "plant_species") |>
    dplyr::mutate(
      abundance_relative = visits / sum(visits),
      frequency_relative = occurrences / sum(occurrences),
      connectivity_relative = partners / sum(partners),
      IIP =
        (
          abundance_relative +
            frequency_relative +
            connectivity_relative
        ) / 3
    ) |>
    dplyr::arrange(dplyr::desc(IIP))
}
