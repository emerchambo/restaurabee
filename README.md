
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
- Estimate plant species specialization (**d’**).
- Calculate the **Interaction-based Restoration Priority Index (IPR)**.
- Select priority species based on cumulative restoration priority.
- Perform sensitivity analyses under alternative weighting schemes.
- Calculate ecological network metrics using the **bipartite** package.

## Installation

The development version can be installed from GitHub using:

``` r
# install.packages("remotes")
remotes::install_github("emerchambo/restaurabee")
```

## Input data

The input must be a data frame in long format, with one row per recorded
plant–pollinator interaction and three required columns:

- `plant_species`: a consistent and unambiguous plant species
  identifier;
- `bee_species`: a consistent and unambiguous bee species identifier;
- `sample_unit`: a unique identifier for the sampling unit in which the
  interaction was recorded.

Repeated rows must not be removed when they represent separate
interaction records because they contribute to interaction abundance.

If subplot numbers are repeated among dates, sites, transects, or plots,
`sample_unit` should be constructed as a composite identifier. For
example:

``` r
interaction_data$sample_unit <- paste(
  interaction_data$date,
  interaction_data$site,
  interaction_data$transect,
  interaction_data$plot,
  interaction_data$subplot,
  sep = "_"
)
```

Species identifiers should not contain leading or trailing spaces,
hidden characters, or terminal punctuation. Full scientific names or
other unambiguous identifiers are recommended.

## Example

``` r
library(restaurabee)

interaction_data <- data.frame(
  plant_species = c(
    "Plant_A", "Plant_A", "Plant_A", "Plant_A", "Plant_A",
    "Plant_B", "Plant_B", "Plant_B",
    "Plant_C", "Plant_C", "Plant_C", "Plant_C", "Plant_C",
    "Plant_D", "Plant_D", "Plant_D", "Plant_D",
    "Plant_E", "Plant_E", "Plant_E", "Plant_E",
    "Plant_F", "Plant_F"
  ),
  bee_species = c(
    "Bee_1", "Bee_1", "Bee_2", "Bee_2", "Bee_3",
    "Bee_1", "Bee_1", "Bee_2",
    "Bee_2", "Bee_2", "Bee_2", "Bee_3", "Bee_4",
    "Bee_3", "Bee_4", "Bee_4", "Bee_5",
    "Bee_1", "Bee_4", "Bee_5", "Bee_5",
    "Bee_2", "Bee_5"
  ),
  sample_unit = c(
    "S1", "S2", "S2", "S3", "S4",
    "S1", "S5", "S5",
    "S2", "S3", "S6", "S6", "S7",
    "S4", "S7", "S8", "S8",
    "S5", "S9", "S9", "S10",
    "S6", "S10"
  )
)

result <- restaurabee(interaction_data)
```

### Interaction Importance Index (IIP)

``` r
result$IIP
#>   plant_species visits occurrences partners abundance_relative
#> 1       Plant_A      5           4        3         0.21739130
#> 2       Plant_C      5           4        3         0.21739130
#> 3       Plant_D      4           3        3         0.17391304
#> 4       Plant_E      4           3        3         0.17391304
#> 5       Plant_B      3           2        2         0.13043478
#> 6       Plant_F      2           2        2         0.08695652
#>   frequency_relative connectivity_relative       IIP
#> 1          0.2222222                0.1875 0.2090378
#> 2          0.2222222                0.1875 0.2090378
#> 3          0.1666667                0.1875 0.1760266
#> 4          0.1666667                0.1875 0.1760266
#> 5          0.1111111                0.1250 0.1221820
#> 6          0.1111111                0.1250 0.1076892
```

### Restoration prioritization (IPR)

``` r
result$prioritization
#>   plant_species visits occurrences partners abundance_relative
#> 1       Plant_C      5           4        3         0.21739130
#> 2       Plant_A      5           4        3         0.21739130
#> 3       Plant_D      4           3        3         0.17391304
#> 4       Plant_E      4           3        3         0.17391304
#> 5       Plant_B      3           2        2         0.13043478
#> 6       Plant_F      2           2        2         0.08695652
#>   frequency_relative connectivity_relative       IIP    d_prime abundance_pad
#> 1          0.2222222                0.1875 0.2090378 0.32313108     1.0000000
#> 2          0.2222222                0.1875 0.2090378 0.26793459     1.0000000
#> 3          0.1666667                0.1875 0.1760266 0.38823646     0.6666667
#> 4          0.1666667                0.1875 0.1760266 0.30751100     0.6666667
#> 5          0.1111111                0.1250 0.1221820 0.23537737     0.3333333
#> 6          0.1111111                0.1250 0.1076892 0.06276279     0.0000000
#>   d_prime_pad connectivity_pad frequency_pad       IPR
#> 1   0.7999673                1           1.0 0.9499918
#> 2   0.6303791                1           1.0 0.9075948
#> 3   1.0000000                1           0.5 0.7916667
#> 4   0.7519754                1           0.5 0.7296605
#> 5   0.5303488                0           0.0 0.2159205
#> 6   0.0000000                0           0.0 0.0000000
```

### Selected priority species

``` r
result$selected_species
#>   plant_species visits occurrences partners abundance_relative
#> 1       Plant_C      5           4        3          0.2173913
#> 2       Plant_A      5           4        3          0.2173913
#> 3       Plant_D      4           3        3          0.1739130
#> 4       Plant_E      4           3        3          0.1739130
#>   frequency_relative connectivity_relative       IIP   d_prime abundance_pad
#> 1          0.2222222                0.1875 0.2090378 0.3231311     1.0000000
#> 2          0.2222222                0.1875 0.2090378 0.2679346     1.0000000
#> 3          0.1666667                0.1875 0.1760266 0.3882365     0.6666667
#> 4          0.1666667                0.1875 0.1760266 0.3075110     0.6666667
#>   d_prime_pad connectivity_pad frequency_pad       IPR relative_IPR
#> 1   0.7999673                1           1.0 0.9499918    0.2642658
#> 2   0.6303791                1           1.0 0.9075948    0.2524719
#> 3   1.0000000                1           0.5 0.7916667    0.2202234
#> 4   0.7519754                1           0.5 0.7296605    0.2029747
#>   cumulative_IPR
#> 1      0.2642658
#> 2      0.5167377
#> 3      0.7369612
#> 4      0.9399359
```

### Sensitivity analysis

``` r
result$sensitivity$correlation_matrix
#>                     rank_equal rank_abundance rank_specialization
#> rank_equal                   1              1                   1
#> rank_abundance               1              1                   1
#> rank_specialization          1              1                   1
#> rank_connectivity            1              1                   1
#> rank_frequency               1              1                   1
#>                     rank_connectivity rank_frequency
#> rank_equal                          1              1
#> rank_abundance                      1              1
#> rank_specialization                 1              1
#> rank_connectivity                   1              1
#> rank_frequency                      1              1
```

## Workflow

The complete workflow is run with `restaurabee()`, which coordinates the
following steps:

``` text
Interaction data
        │
        ▼
Data validation
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
```

The individual calculation functions are also exported for users who
need greater control over the workflow.

## Interpretation

The IPR is a relative prioritization score and should not be interpreted
as an absolute measure of restoration success. Results depend on
sampling effort, taxonomic resolution, spatial and temporal coverage,
and data quality.

The prioritization should be considered alongside ecological
feasibility, species origin, establishment requirements, and other
restoration criteria.

## Citation

If you use **restaurabee** in your research, please cite the package:

``` r
citation("restaurabee")
```

A citation for the corresponding methodological publication will be
added when it becomes available.

## License

MIT License.
