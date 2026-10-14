# restaurabee 0.2.1

## Bug fixes

- Adjusted d' values outside the interval from zero to one by no more
  than 1e-12 to account for numerical rounding.
- Retained errors for values outside this tolerance.
- Added tests for boundary adjustment and preservation of valid values.

# restaurabee 0.2.0

## New features

- Added `pollinator_coverage()` to report the recorded bee species
  represented by selected plants.
- Added `bootstrap_priority()` to evaluate plant selection stability
  by resampling sampling units or groups of units.
- Added custom weights, selection thresholds and boundary tie handling
  to `restaurabee()`.
- Added configurable network indices and an optional seed.
- Added analysis settings to the output and displayed them in
  `print()` and `summary()`.

## Improvements

- Strengthened input validation across calculation functions.
- Prevented species identifier conflicts when building interaction matrices.
- Clarified interaction, occurrence and partner counts.
- Improved handling of constant rankings and undefined correlations.
- Reported selection failures without returning all plants as selected.
- Added tests for calculation functions and workflow settings.

## Documentation and dependencies

- Updated the README and vignette with coverage and bootstrap examples,
  interpretation and limitations.
- Declared R >= 4.1.0.
- Removed the unused tidyr dependency and the unnecessary LazyData field.
