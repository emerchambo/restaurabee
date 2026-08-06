#' Calculate the Interaction-based Restoration Priority Index (IPR)
#'
#' Computes the Interaction-based Restoration Priority Index (IPR) for plant
#' species by integrating standardized ecological metrics derived from
#' plant-pollinator interaction networks.
#'
#' The IPR combines four ecological components:
#' \itemize{
#'   \item Relative interaction abundance;
#'   \item Relative occurrence frequency;
#'   \item Relative connectivity (number of pollinator partners);
#'   \item Species specialization, represented by the d' index.
#' }
#'
#' By default, all four components receive equal weights, although users may
#' specify alternative weighting schemes according to their ecological
#' hypotheses, restoration objectives, or management priorities.
#'
#' @param plant_metrics A data frame containing the columns:
#' \itemize{
#'   \item \code{abundance_relative}
#'   \item \code{frequency_relative}
#'   \item \code{connectivity_relative}
#'   \item \code{d_prime}
#' }
#'
#' @param weight_abundance Weight assigned to interaction abundance
#'   (default = 0.25).
#' @param weight_specialization Weight assigned to species specialization
#'   (d') (default = 0.25).
#' @param weight_connectivity Weight assigned to connectivity
#'   (default = 0.25).
#' @param weight_frequency Weight assigned to occurrence frequency
#'   (default = 0.25).
#'
#' @return A data frame containing standardized ecological metrics and the
#' final IPR score, ordered from highest to lowest restoration priority.
#'
#' @details
#' All ecological metrics are standardized using min-max scaling:
#'
#' \deqn{
#' x_{pad} = \frac{x - min(x)}{max(x) - min(x)}
#' }
#'
#' Species specialization is represented by the standardized d' index:
#'
#' \deqn{
#' D_i = d'_{pad,i}
#' }
#'
#' The final Interaction-based Restoration Priority Index is calculated as:
#'
#' \deqn{
#' IPR_i =
#' w_A A_i +
#' w_D D_i +
#' w_C C_i +
#' w_F F_i
#' }
#'
#' where \eqn{A_i} is standardized interaction abundance, \eqn{D_i} is
#' standardized species specialization, \eqn{C_i} is standardized
#' connectivity, and \eqn{F_i} is standardized occurrence frequency.
#'
#' The weights must satisfy:
#'
#' \deqn{
#' w_A + w_D + w_C + w_F = 1.
#' }
#'
#' @export
calculate_IPR <- function(
    plant_metrics,
    weight_abundance = 0.25,
    weight_specialization = 0.25,
    weight_connectivity = 0.25,
    weight_frequency = 0.25
) {

  weight_sum <-
    weight_abundance +
    weight_specialization +
    weight_connectivity +
    weight_frequency

  if (abs(weight_sum - 1) > 1e-8) {
    stop(
      "The four weights must sum to 1.",
      call. = FALSE
    )
  }

  scale_minmax <- function(x) {

    if (all(is.na(x))) {
      return(rep(NA_real_, length(x)))
    }

    x_min <- min(x, na.rm = TRUE)
    x_max <- max(x, na.rm = TRUE)

    if (x_max == x_min) {
      return(rep(0, length(x)))
    }

    (x - x_min) / (x_max - x_min)
  }

  plant_metrics |>
    dplyr::mutate(
      abundance_pad = scale_minmax(abundance_relative),
      d_prime_pad = scale_minmax(d_prime),
      connectivity_pad = scale_minmax(connectivity_relative),
      frequency_pad = scale_minmax(frequency_relative),

      IPR =
        weight_abundance * abundance_pad +
        weight_specialization * d_prime_pad +
        weight_connectivity * connectivity_pad +
        weight_frequency * frequency_pad
    ) |>
    dplyr::arrange(dplyr::desc(IPR))
}
