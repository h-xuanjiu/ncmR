#' Launch the ncmR Shiny Application
#'
#' @description
#' Starts the interactive Shiny web application for fitting and visualizing
#' Neutral Community Models (NCM). The app provides a user-friendly interface
#' for uploading abundance data, fitting NCM models, and generating publication-quality
#' plots.
#'
#' @details
#' The Shiny app includes two main modules:
#' \itemize{
#'   \item \strong{Fit NCM}: Upload abundance data (samples as rows, species as columns)
#'     and optional group metadata to fit neutral community models. Supports grouped
#'     analyses and provides model parameters, fit statistics, and species-level predictions.
#'   \item \strong{Plotting}: Visualize fitting results or upload pre-computed NCM results
#'     to generate customizable scatter plots with fitted curves and confidence intervals.
#' }
#'
#' The app automatically detects sample IDs from the first column and handles large
#' datasets with column pagination. Results can be downloaded as ZIP archives containing
#' summary statistics and predictions.
#'
#' @return
#' This function does not return a value; it launches a Shiny application
#' interactively in the default web browser.
#'
#' @examples
#' \dontrun{
#' # Launch the app
#' ncmR::run_app()
#' }
#'
#' @seealso
#' \code{\link{fit_ncm}} for programmatic NCM fitting,
#' \code{\link{scatter_plot}} for generating NCM plots in R scripts
#'
#' @export
run_app <- function() {
  app_dir <- system.file("ncmShiny", package = "ncmR")
  if (app_dir == "") {
    stop("App not found. Re-install the package.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
