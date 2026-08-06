
<!-- README.md is generated from README.Rmd. Please edit README.Rmd and then run devtools::build_readme(). -->

# restaurabee

<!-- badges: start -->
<!-- badges: end -->

## Overview

**restaurabee** is an R package for prioritizing plant species for
ecological restoration using plant–pollinator interaction networks.

The package provides a reproducible workflow that integrates interaction
abundance, occurrence frequency, network connectivity, and species
specialization to identify plant species with the greatest potential to
support the restoration of pollination interactions.

## Main features

- Validate plant–pollinator interaction datasets.
- Build quantitative interaction matrices.
- Calculate the **Interaction Importance Index (IIP)**.
- Estimate species specialization (**d’**).
- Calculate the **Interaction-based Restoration Priority Index (IPR)**.
- Select priority species based on cumulative restoration priority.
- Perform sensitivity analyses under alternative weighting schemes.
- Calculate ecological network metrics using the **bipartite** package.

## Installation

The development version can be installed from GitHub using:

``` r
# install.packages("remotes")
remotes::install_github("YOUR_GITHUB_USERNAME/restaurabee")
```

## Example

``` r
library(restaurabee)

interaction_data <- data.frame(
  plant_species = c(
    "Plant_A",
    "Plant_A",
    "Plant_B",
    "Plant_C"
  ),
  bee_species = c(
    "Bee_1",
    "Bee_2",
    "Bee_1",
    "Bee_3"
  ),
  sample_unit = c(
    "S1",
    "S1",
    "S2",
    "S3"
  )
)

result <- restaurabee(interaction_data)
#> Warning in fisherfit(web): all species singletons: alpha arbitrarily high
```

### Interaction Importance Index (IIP)

``` r
result$IIP
#>   plant_species visits occurrences partners abundance_relative
#> 1       Plant_A      2           1        2               0.50
#> 2       Plant_B      1           1        1               0.25
#> 3       Plant_C      1           1        1               0.25
#>   frequency_relative connectivity_relative       IIP
#> 1          0.3333333                  0.50 0.4444444
#> 2          0.3333333                  0.25 0.2777778
#> 3          0.3333333                  0.25 0.2777778
```

### Restoration prioritization (IPR)

``` r
result$prioritization
#>   plant_species visits occurrences partners abundance_relative
#> 1       Plant_A      2           1        2               0.50
#> 2       Plant_C      1           1        1               0.25
#> 3       Plant_B      1           1        1               0.25
#>   frequency_relative connectivity_relative       IIP d_prime abundance_pad
#> 1          0.3333333                  0.50 0.4444444       0             1
#> 2          0.3333333                  0.25 0.2777778       1             0
#> 3          0.3333333                  0.25 0.2777778       0             0
#>   d_prime_pad connectivity_pad frequency_pad  IPR
#> 1           0                1             0 0.50
#> 2           1                0             0 0.25
#> 3           0                0             0 0.00
```

### Selected priority species

``` r
result$selected_species
#>   plant_species visits occurrences partners abundance_relative
#> 1       Plant_A      2           1        2               0.50
#> 2       Plant_C      1           1        1               0.25
#>   frequency_relative connectivity_relative       IIP d_prime abundance_pad
#> 1          0.3333333                  0.50 0.4444444       0             1
#> 2          0.3333333                  0.25 0.2777778       1             0
#>   d_prime_pad connectivity_pad frequency_pad  IPR relative_IPR
#> 1           0                1             0 0.50    0.6666667
#> 2           1                0             0 0.25    0.3333333
#>   cumulative_IPR
#> 1      0.6666667
#> 2      1.0000000
```

### Sensitivity analysis

``` r
result$sensitivity$correlation_matrix
#>                     rank_equal rank_abundance rank_specialization
#> rank_equal           1.0000000      1.0000000           0.8660254
#> rank_abundance       1.0000000      1.0000000           0.8660254
#> rank_specialization  0.8660254      0.8660254           1.0000000
#> rank_connectivity    1.0000000      1.0000000           0.8660254
#> rank_frequency       1.0000000      1.0000000           0.8660254
#>                     rank_connectivity rank_frequency
#> rank_equal                  1.0000000      1.0000000
#> rank_abundance              1.0000000      1.0000000
#> rank_specialization         0.8660254      0.8660254
#> rank_connectivity           1.0000000      1.0000000
#> rank_frequency              1.0000000      1.0000000
```

## Workflow

    Interaction data
            │
            ▼
    check_data()
            │
            ▼
    build_interaction_matrix()
            │
            ├──────────────► Network metrics
            │
            ▼
    calculate_IIP()
            │
            ▼
    calculate_d_prime()
            │
            ▼
    calculate_IPR()
            │
            ▼
    select_species()
            │
            ▼
    sensitivity_analysis()

## Citation

If you use **restaurabee** in your research, please cite the package and
the corresponding methodological publication (to be added).

## License

MIT License.
