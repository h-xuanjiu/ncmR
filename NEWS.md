# ncmR 0.3.0 (2026-04-19)

## NEW FEATURES

-   feat: add Shiny application with Fit NCM and Plotting modules.

## CHANGES

-   `scatter_plot`: removed Unicode characters from axis labels for broader compatibility.

## REMOVED

-   `print.unicode_ggplot()` and `ggsave_unicode()`: no longer needed after removing Unicode dependency from plotting functions.


# ncmR 0.2.0 (2026-04-15)

## NEW FEATURES

-   Added function `scatter_plot`: NCM scatter plot with fitted curve and CI.
-   Added function `print.unicode_ggplot`: Enables Unicode display when printing plots.
-   Added function `ggsave_unicode`: Saves plots with full Unicode support.

# ncmR 0.1.0 (2026-04-01)

  * Initial CRAN submission.
  * Added `fit_ncm()` function for neutral community model fitting.
  * Added `summary()` methods for `NCM` and `NCM_group` objects.
  * Included example dataset `example_data`.
