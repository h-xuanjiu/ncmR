#' Example OTU table and grouping information for NCM demonstration
#'
#' A simulated dataset containing an OTU table (ASV counts) and corresponding
#' group labels. The data are intended to illustrate the usage of the
#' \code{\link{fit_ncm}} function.
#'
#' @format A list with two components:
#' \describe{
#'   \item{otu}{A data frame with 15 rows (samples) and 2000 columns (ASVs).
#'              Row names are sample identifiers, column names are ASV identifiers.
#'              Values represent counts of each ASV in each sample.}
#'   \item{grp}{A data frame with 15 rows and 1 column named \code{group}.
#'              Row names correspond to the samples in \code{otu}. The \code{group}
#'              column contains character labels ("A", "A", ..., etc.) indicating
#'              group membership for each sample.}
#' }
#'
#' @source Simulated data for package examples.
"example_data"
