# Global variable declarations
#
# These variables are used within tidy evaluation expressions
# (e.g., dplyr::mutate(), dplyr::summarise(), dplyr::filter())
# to avoid "no visible binding for global variable" notes during
# R CMD check.

utils::globalVariables(c(

  # Input variables
  "plant_species",
  "bee_species",
  "sample_unit",

  # Interaction metrics
  "visits",
  "occurrences",
  "partners",
  "n",

  # Relative ecological metrics
  "abundance_relative",
  "frequency_relative",
  "connectivity_relative",

  # Interaction Importance Index
  "IIP",

  # Species specialization
  "d_prime",

  # Standardized ecological metrics
  "abundance_pad",
  "frequency_pad",
  "connectivity_pad",
  "d_prime_pad",

  # Interaction-based Restoration Priority Index
  "IPR",
  "relative_IPR",
  "cumulative_IPR",

  # Sensitivity analysis
  "IPR_equal",
  "IPR_abundance",
  "IPR_specialization",
  "IPR_connectivity",
  "IPR_frequency",

  "rank_equal",
  "rank_abundance",
  "rank_specialization",
  "rank_connectivity",
  "rank_frequency"

))
