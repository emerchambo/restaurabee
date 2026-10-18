
<!-- README.md is generated from README.Rmd. Please edit README.Rmd and then run devtools::build_readme(). -->

# restaurabee

<!-- badges: start -->

<!-- badges: end -->

## Overview

**restaurabee** is an R package for ranking plant species using
plant–pollinator interaction records to inform ecological restoration.

The package combines interaction abundance, occurrence frequency,
connectivity and species specialization into a score for each plant. It
also describes the cumulative distribution of scores, associations
between components, changes under alternative weights, recorded bee
coverage and stability under resampling.

## Main features

- Validate plant–pollinator interaction datasets.
- Build quantitative interaction matrices.
- Calculate the **Interaction Importance Index (IIP)**.
- Estimate species specialization (**d’**) for plants and bees.
- Calculate the **Interaction-based Restoration Priority Index (IPR)**.
- Set component weights, cumulative IPR thresholds and boundary tie
  rules.
- Describe the cumulative IPR curve and set sizes at requested
  thresholds.
- Calculate Spearman correlations between IPR components.
- Examine changes in rankings and cumulative sets after omitting
  components.
- Compare rankings under alternative weighting schemes.
- Calculate selected ecological network metrics using **bipartite**.
- Calculate the coverage of recorded bee species by a set of plants.
- Evaluate plant selection stability and set size by resampling sampling
  units.

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

The output retains all plants and their component values. Higher IPR
values indicate higher positions under the supplied weights.

### Plants retained at the cumulative threshold

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

By default, this set reaches at least 80% of total IPR. The threshold is
adjustable and describes the distribution of scores. It is not an
estimate of bee coverage or a recommendation to plant only these
species.

### Cumulative IPR proportions

The diagnostics include the number of plants needed to reach 70%, 80%
and 90% of total IPR, together with the threshold supplied to
`restaurabee()`.

``` r
result$diagnostics$ipr_accumulation$thresholds
#>   threshold k achieved_cumulative_IPR boundary_IPR  status message
#> 1       0.7 3               0.7369612    0.7916667 defined        
#> 2       0.8 4               0.9399359    0.7296605 defined        
#> 3       0.9 4               0.9399359    0.7296605 defined
```

The complete curve retains all plants, including those with zero IPR:

``` r
result$diagnostics$ipr_accumulation$curve
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
#>   d_prime_pad connectivity_pad frequency_pad       IPR k relative_IPR
#> 1   0.7999673                1           1.0 0.9499918 1   0.26426581
#> 2   0.6303791                1           1.0 0.9075948 2   0.25247193
#> 3   1.0000000                1           0.5 0.7916667 3   0.22022341
#> 4   0.7519754                1           0.5 0.7296605 4   0.20297473
#> 5   0.5303488                0           0.0 0.2159205 5   0.06006412
#> 6   0.0000000                0           0.0 0.0000000 6   0.00000000
#>   cumulative_IPR
#> 1      0.2642658
#> 2      0.5167377
#> 3      0.7369612
#> 4      0.9399359
#> 5      1.0000000
#> 6      1.0000000
```

Use `ipr_accumulation()` directly to examine other thresholds:

``` r
accumulation <- ipr_accumulation(
  result$prioritization,
  thresholds = c(0.60, 0.75, 0.95),
  ties = result$settings$ties
)

accumulation$thresholds
#>   threshold k achieved_cumulative_IPR boundary_IPR  status message
#> 1      0.60 3               0.7369612    0.7916667 defined        
#> 2      0.75 4               0.9399359    0.7296605 defined        
#> 3      0.95 5               1.0000000    0.2159205 defined
```

When total IPR is zero, cumulative proportions and threshold sizes are
undefined. The function retains the curve and reports this condition.

### Associations between components

Spearman correlations describe how the four components vary together
across plants:

``` r
result$diagnostics$component_correlations$correlation_matrix
#>                abundance specialization connectivity frequency
#> abundance      1.0000000      0.6179144    0.8528029 0.9847319
#> specialization 0.6179144      1.0000000    0.8280787 0.5976143
#> connectivity   0.8528029      0.8280787    1.0000000 0.8660254
#> frequency      0.9847319      0.5976143    0.8660254 1.0000000
result$diagnostics$component_correlations$constant_components
#> character(0)
```

Positive associations can indicate that components reinforce similar
patterns in the data. Equal weights do not ensure independent
contributions from the four components.

Correlations involving a constant component are undefined and returned
as `NA`. This diagnostic does not change weights or remove components.

The same calculation can be run separately with
`component_correlations(result$prioritization)`.

### Component omission

Each omission scenario removes one component and rescales the remaining
weights to sum to one. Results are compared with the reference weights
used in the analysis.

``` r
omission <- result$diagnostics$component_omission

omission$scenario_weights
#>                        abundance specialization connectivity frequency
#> reference              0.2500000      0.2500000    0.2500000 0.2500000
#> without_abundance      0.0000000      0.3333333    0.3333333 0.3333333
#> without_specialization 0.3333333      0.0000000    0.3333333 0.3333333
#> without_connectivity   0.3333333      0.3333333    0.0000000 0.3333333
#> without_frequency      0.3333333      0.3333333    0.3333333 0.0000000

omission$summary[, c(
  "scenario",
  "spearman_vs_reference",
  "mean_absolute_rank_change",
  "n_selected",
  "jaccard",
  "status"
)]
#>                 scenario spearman_vs_reference mean_absolute_rank_change
#> 1              reference             1.0000000                 0.0000000
#> 2      without_abundance             1.0000000                 0.0000000
#> 3 without_specialization             0.9710083                 0.3333333
#> 4   without_connectivity             1.0000000                 0.0000000
#> 5      without_frequency             0.9428571                 0.3333333
#>   n_selected jaccard  status
#> 1          4       1 defined
#> 2          4       1 defined
#> 3          4       1 defined
#> 4          4       1 defined
#> 5          4       1 defined
```

The full summary also reports Kendall correlations, maximum rank changes
and plants added to or removed from the cumulative set.

Omitting a component changes both its contribution and the weights of
the remaining components. High rank correlations do not guarantee that
the same plants are retained at a cumulative threshold.

This diagnostic can also be run separately with `component_omission()`.
Supply the weights, threshold and tie rule explicitly when reproducing a
customized analysis.

### Sensitivity to alternative weights

Sensitivity analysis compares the reference ranking with weighting
scenarios that emphasize individual components:

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

These comparisons describe the tested weights. They do not measure
sampling uncertainty.

### Custom analysis settings

Weights must be named, non-negative and sum to one. The threshold
specifies the cumulative proportion of total IPR to retain. Use
`ties = "include"` to retain all plants tied with the last selected
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
#> 
#> $diagnostics
#> [1] TRUE
```

Ranking diagnostics are calculated by default. To skip them:

``` r
result_without_diagnostics <- restaurabee(
  interaction_data,
  diagnostics = FALSE
)

is.null(result_without_diagnostics$diagnostics)
#> [1] TRUE
```

Disabling these diagnostics does not change IIP, d’, IPR, the cumulative
selection, sensitivity analysis or network metrics.

### Bee coverage

Coverage is the proportion of recorded bee species that interact with at
least one plant in the supplied set. Each bee species is counted once.

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

### Bootstrap selection stability and set size

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

bootstrap_result$set_size$summary
bootstrap_result$set_size$frequencies
```

By default, records from each `sample_unit` are resampled together. Use
`resample_unit` to specify a grouping column when observations from the
same location should remain together.

Always inspect the diagnostics. Selection frequencies use only
replicates with defined selection. IPR and rank summaries additionally
require the plant to be present. Replicates with total IPR zero are
retained for inspection, with selection recorded as `NA`.

The `set_size` output describes the number of retained plants in each
replicate, its frequency distribution and summary statistics. Replicates
with calculation failures or undefined selection are excluded from these
statistics and counted separately. No additional resampling is
performed.

The reported intervals describe the bootstrap distributions. They are
not calibrated confidence intervals for true priorities.

## Workflow

`restaurabee()` validates the data, builds the interaction matrix,
calculates IIP and d’, combines the plant metrics, and calculates IPR.
It also calculates ranking diagnostics, selects plants at the requested
cumulative threshold, compares weighting scenarios and calculates the
requested network metrics.

The ranking diagnostics are stored in `result$diagnostics`:

- `ipr_accumulation`: cumulative scores and set sizes at requested
  thresholds;
- `component_correlations`: associations between the four IPR
  components;
- `component_omission`: changes after removing each component.

`print()` provides a brief overview. `summary()` also displays the
ranking diagnostics when available.

`pollinator_coverage()` evaluates the bee partners represented by a
plant set. `bootstrap_priority()` evaluates stability under resampling.
Both functions are called separately.

Individual calculation and diagnostic functions are also exported for
users who need greater control over the workflow.

## Interpretation

The IPR is a relative prioritization score and should not be interpreted
as an absolute measure of restoration success. Results depend on
sampling effort, taxonomic resolution, spatial and temporal coverage,
and data quality.

Interaction abundance counts recorded interactions. Occurrence frequency
describes their distribution among sampling units. Neither component
measures plant abundance or floral availability.

Component correlations help identify associated patterns. Component
omission and sensitivity analysis describe dependence on the chosen
weights. Bootstrap analysis evaluates changes induced by resampling the
units represented in the data.

These procedures do not correct unequal observation effort or recover
sampling units that were not recorded. Plants with few records or
recorded in few units require particular care when interpreting
specialization and ranking stability.

The ranking and cumulative sets should be considered alongside
ecological feasibility, species origin, establishment requirements and
other restoration criteria.

## Citation

If you use **restaurabee** in your research, please cite the package:

``` r
citation("restaurabee")
```

A citation for the corresponding methodological publication will be
added when it becomes available.

## License

MIT License.
