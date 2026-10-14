
<!-- README.md is generated from README.Rmd. Please edit README.Rmd and then run devtools::build_readme(). -->

# restaurabee

<!-- badges: start -->

<!-- badges: end -->

## Overview

**restaurabee** is an R package for prioritizing plant species for
ecological restoration using plant–pollinator interaction networks.

The package combines interaction abundance, occurrence frequency,
connectivity and species specialization to rank plants. It also
evaluates the bee partners represented by selected plants and the
stability of selection under resampling.

## Main features

- Validate plant–pollinator interaction datasets.
- Build quantitative interaction matrices.
- Calculate the **Interaction Importance Index (IIP)**.
- Estimate species specialization (**d’**) for plants and bees.
- Calculate the **Interaction-based Restoration Priority Index (IPR)**.
- Set component weights, selection thresholds and treatment of boundary
  ties.
- Compare rankings under alternative weighting schemes.
- Calculate selected ecological network metrics using **bipartite**.
- Calculate the coverage of recorded bee species by selected plants.
- Evaluate plant selection stability by resampling sampling units.

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
- `sample_unit`: an identifier for the sampling unit in which the
  interaction was recorded.

Repeated rows must be retained when they represent separate interaction
records, because they contribute to interaction abundance.

The researcher defines the sampling unit according to the study design.
Records belonging to the same unit must share its identifier.

If identifiers are reused among dates, sites, transects or plots,
construct an identifier that distinguishes the units used in the
analysis. For example, when each subplot visit is treated as a sampling
unit:

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

Distinguishing visits does not establish that they are independent. For
bootstrap analyses, repeated visits to the same location can be kept
together using a separate grouping column.

Species identifiers should be checked for spelling differences,
whitespace and inconsistent punctuation. Full scientific names or other
unambiguous identifiers are recommended.

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
result$sensitivity$scenario_weights
#>                abundance specialization connectivity frequency
#> equal               0.25           0.25         0.25      0.25
#> abundance           0.40           0.20         0.20      0.20
#> specialization      0.20           0.40         0.20      0.20
#> connectivity        0.20           0.20         0.40      0.20
#> frequency           0.20           0.20         0.20      0.40
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

### Custom analysis settings

Weights must be named, non-negative and sum to one. The selection
threshold specifies the cumulative proportion of total IPR to retain.
Use `ties = "include"` to retain all plants tied with the last selected
plant.

``` r
custom_result <- restaurabee(
  interaction_data,
  weights = c(
    abundance = 0.20,
    specialization = 0.40,
    connectivity = 0.20,
    frequency = 0.20
  ),
  threshold = 0.70,
  ties = "include",
  network_indices = c("connectance", "H2")
)

custom_result$settings
#> $weights
#>      abundance specialization   connectivity      frequency 
#>            0.2            0.4            0.2            0.2 
#> 
#> $threshold
#> [1] 0.7
#> 
#> $ties
#> [1] "include"
#> 
#> $network_indices
#> [1] "connectance" "H2"         
#> 
#> $seed
#> NULL
```

### Bee coverage

Coverage is the proportion of recorded bee species that interact with at
least one selected plant. Each bee species is counted once.

``` r
if (!is.null(result$selected_species)) {
  coverage <- pollinator_coverage(
    plant_species = result$selected_species$plant_species,
    interaction_matrix = result$interaction_matrix
  )

  coverage
}
#> $coverage
#> [1] 1
#> 
#> $n_plants
#> [1] 4
#> 
#> $n_bees_total
#> [1] 5
#> 
#> $n_bees_covered
#> [1] 5
#> 
#> $covered
#> [1] "Bee_1" "Bee_2" "Bee_3" "Bee_4" "Bee_5"
#> 
#> $not_covered
#> character(0)
```

This describes partners recorded in the supplied network. It does not
estimate sampling completeness or predict the persistence of bee
populations after restoration.

### Bootstrap selection stability

`bootstrap_priority()` resamples units with replacement and recalculates
metrics and selection. It is run separately from `restaurabee()`.

``` r
bootstrap_result <- bootstrap_priority(
  interaction_data,
  n_boot = 1000,
  weights = result$settings$weights,
  threshold = result$settings$threshold,
  ties = result$settings$ties,
  seed = 123
)

bootstrap_result$diagnostics
bootstrap_result$summary
```

By default, records from each `sample_unit` are resampled together. Use
`resample_unit` to specify a grouping column when observations from the
same location should remain together.

Always inspect the diagnostics. Selection frequencies use only
replicates with defined selection. IPR and rank summaries additionally
require the plant to be present. Replicates with total IPR zero are
retained for inspection, with selection recorded as `NA`.

The reported intervals describe the bootstrap distributions. They are
not calibrated confidence intervals for true priorities.

## Workflow

`restaurabee()` validates the data, builds the interaction matrix,
calculates IIP and d’, combines the plant metrics, and calculates IPR.
It then selects plants, compares weighting scenarios and calculates the
requested network metrics.

`pollinator_coverage()` evaluates the bee partners represented by a
plant selection. `bootstrap_priority()` evaluates the stability of
prioritization under resampling. Both functions are called separately.

Individual calculation functions are also exported for users who need
greater control over the workflow.

## Interpretation

The IPR is a relative prioritization score and should not be interpreted
as an absolute measure of restoration success. Results depend on
sampling effort, taxonomic resolution, spatial and temporal coverage,
and data quality.

Sensitivity analysis evaluates changes in weights. Bootstrap analysis
evaluates changes induced by resampling the units represented in the
data. Neither procedure corrects unequal observation effort or recovers
sampling units that were not recorded.

The prioritization should be considered alongside ecological
feasibility, species origin, establishment requirements and other
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
