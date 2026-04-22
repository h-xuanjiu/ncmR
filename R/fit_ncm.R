utils::globalVariables("status")

#' Fit Neutral Community Model (NCM) with optional grouping and total pooling
#'
#' @param otu OTU table, rows = samples, columns = species (data.frame or matrix)
#' @param grp Optional data.frame with rownames = sample IDs, and one column specifying group membership.
#'        If NULL, all samples in otu are used as one group.
#' @param group_col Column name in grp that contains group labels (default "group").
#' @param groups Character vector of group names to analyze. If NULL and grp is provided,
#'        the function fits models for each group AND for all samples combined (total).
#'        If non-NULL, only those groups are analyzed (no total model).
#' @param simplify If TRUE and only one model is fitted, return the model list directly (not nested).
#' @param return_model Whether to return the nlsLM model object (default FALSE to save space).
#' @param ... Additional arguments passed to nlsLM (e.g., lower, upper, control).
#'
#' @return A list containing model results. If multiple models, a named list with keys:
#'         - "all" (if total model fitted) and group names. Each value is a list with:
#'           m, N, Nm, ci, rsqr, predictions, (model optional).
#' @export
#'
#' @examples
#' # Load example data
#' data(example_data)
#' otu <- example_data$otu
#' grp <- example_data$grp
#'
#' # 1. No grouping: fit the total model using all samples
#' res_total <- fit_ncm(otu)
#' print(paste("m =", round(res_total$m, 4)))
#' print(paste("R^2 =", round(res_total$rsqr, 4)))
#' head(res_total$predictions)
#'
#' # 2. With grouping: fit models for all groups + total
#' res_all <- fit_ncm(otu, grp)
#'
#' # 3. Only specific groups (no total model), returns a single model object
#' res_sub <- fit_ncm(otu, grp, groups = "A")
#' res_sub$status_summary
#'
fit_ncm <- function(otu, grp = NULL, group_col = "group", groups = NULL,
                    simplify = TRUE, return_model = FALSE, ...) {
  if (!requireNamespace("minpack.lm", quietly = TRUE)) {
    stop("Package 'minpack.lm' is required for nlsLM.")
  }
  if (!requireNamespace("Hmisc", quietly = TRUE)) {
    stop("Package 'Hmisc' is required for binconf.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required for data manipulation.")
  }

  if (!is.data.frame(otu)) {
    otu <- as.data.frame(otu, stringsAsFactors = FALSE)
  }

  if (nrow(otu) == 0) {
    stop("otu has no rows.")
  }

  fit_one <- function(spp_mat, group_name = NULL, ...) {
    spp_mat <- spp_mat[, colSums(spp_mat) > 0, drop = FALSE]
    if (ncol(spp_mat) == 0) {
      warning(
        "No species with positive abundance in ",
        ifelse(is.null(group_name), "merged data", paste0("group '", group_name, "'"))
      )
      return(NULL)
    }

    N <- mean(rowSums(spp_mat))
    p_mean <- colMeans(spp_mat)
    p <- p_mean / N
    freq <- colMeans(spp_mat > 0)

    data_tbl <- data.frame(
      species = names(p), p = as.numeric(p), freq = as.numeric(freq),
      stringsAsFactors = FALSE
    )
    data_tbl <- data_tbl[data_tbl$p > 0 & data_tbl$freq > 0, , drop = FALSE]
    data_tbl <- data_tbl[order(data_tbl$freq), , drop = FALSE]

    if (nrow(data_tbl) < 3) {
      warning(
        "Less than 3 species after filtering (", nrow(data_tbl), ") in ",
        ifelse(is.null(group_name), "merged data", paste0("group '", group_name, "'"))
      )
      return(NULL)
    }

    p_vec <- data_tbl$p
    freq_vec <- data_tbl$freq
    d <- 1 / N

    dots <- list(...)

    if (!"start" %in% names(dots)) {
      start_values <- list(m = c(0.01, 0.05, 0.1, 0.5))
    } else {
      start_values <- list(m = dots[["start"]][["m"]])
    }

    if (!"lower" %in% names(dots)) {
      lower <- c(m = 1e-6)
    } else {
      lower <- dots[["lower"]]
    }
    if (!"upper" %in% names(dots)) {
      upper <- c(m = 1)
    } else {
      upper <- dots[["upper"]]
    }

    fit <- NULL
    used_start <- NULL
    for (m_start in start_values[["m"]]) {
      used_start <- m_start
      fit <- tryCatch(
        minpack.lm::nlsLM(freq ~ stats::pbeta(d, N * m * p, N * m * (1 - p), lower.tail = FALSE),
          start = list(m = m_start),
          lower = lower, upper = upper,
          data = list(p = p_vec, freq = freq_vec, N = N, d = d),
          ...
        ),
        error = function(e) NULL
      )
      if (!is.null(fit)) break
    }

    if (is.null(fit)) {
      warning(
        "nlsLM failed for ",
        ifelse(is.null(group_name), "merged data", paste0("group '", group_name, "'")),
        " with start values: ", paste(start_values[["m"]], collapse = ", "),
        ". Check data or adjust parameters."
      )
      return(NULL)
    }

    m_est <- unname(stats::coef(fit)["m"])
    m_ci <- tryCatch(stats::confint(fit, "m", level = 0.95),
      error = function(e) c(NA_real_, NA_real_)
    )

    freq_pred <- stats::pbeta(d, N * m_est * p_vec, N * m_est * (1 - p_vec), lower.tail = FALSE)

    n_samples <- nrow(spp_mat)
    ci_df <- Hmisc::binconf(
      freq_pred * n_samples, n_samples,
      alpha = 0.05,
      method = "wilson", return.df = TRUE
    )

    ss_res <- sum((freq_vec - freq_pred)^2)
    ss_tot <- sum((freq_vec - mean(freq_vec))^2)
    rsqr <- 1 - ss_res / ss_tot

    pred_df <- data_tbl
    pred_df$freq.pred <- freq_pred
    pred_df$lower <- ci_df$Lower
    pred_df$upper <- ci_df$Upper
    pred_df <- pred_df[, c("species", "p", "freq", "freq.pred", "lower", "upper")]

    pred_df$status <- dplyr::case_when(
      pred_df$freq > pred_df$upper ~ "Above",
      pred_df$freq < pred_df$lower ~ "Below",
      TRUE ~ "Neutral"
    )

    status_counts <- dplyr::count(pred_df, status)
    status_counts$percentage <- status_counts$n / sum(status_counts$n) * 100

    Nm <- N * m_est

    res <- list(
      m = m_est,
      N = N,
      Nm = Nm,
      ci = m_ci,
      rsqr = rsqr,
      predictions = pred_df,
      status_summary = status_counts
    )
    if (return_model) res$model <- fit
    class(res) <- "NCM"
    return(res)
  }

  if (is.null(grp)) {
    message("Fitting NCM on all samples (no grouping).\n")
    return(fit_one(otu, group_name = NULL, ...))
  }

  if (!is.data.frame(grp)) {
    grp <- as.data.frame(grp, stringsAsFactors = FALSE)
  }
  if (!group_col %in% colnames(grp)) {
    stop("Column '", group_col, "' not found in grp.")
  }
  if (is.null(rownames(grp)) || any(rownames(grp) == "")) {
    stop("grp must have rownames that correspond to sample IDs in otu.")
  }

  common_samples <- intersect(rownames(otu), rownames(grp))
  if (length(common_samples) == 0) {
    stop("No matching sample IDs between otu and grp.")
  }
  otu <- otu[common_samples, , drop = FALSE]
  grp <- grp[common_samples, , drop = FALSE]

  all_groups <- unique(grp[[group_col]])

  if (is.null(groups)) {
    groups_to_use <- all_groups
    fit_total <- TRUE
  } else {
    groups_to_use <- intersect(groups, all_groups)
    if (length(groups_to_use) == 0) {
      stop("None of the specified groups are present in grp.")
    }
    fit_total <- FALSE
  }

  result_list <- list()

  if (fit_total) {
    message("Fitting NCM for all samples combined (total).\n")
    total_fit <- fit_one(otu, group_name = "all", ...)
    if (!is.null(total_fit)) {
      result_list[["all"]] <- total_fit
    } else {
      warning("Fitting failed for total combined samples.")
    }
  }

  for (g in groups_to_use) {
    samples_in_group <- rownames(grp)[grp[[group_col]] == g]
    spp_mat <- otu[samples_in_group, , drop = FALSE]
    message("Fitting NCM for group:", g, " (", length(samples_in_group), " samples)\n")
    fit_res <- fit_one(spp_mat, group_name = g, ...)
    if (!is.null(fit_res)) {
      result_list[[g]] <- fit_res
    } else {
      warning("Fitting failed for group: ", g)
    }
  }

  if (length(result_list) == 0) {
    stop("No successful fits for any group or total.")
  }

  if (simplify && length(result_list) == 1) {
    return(result_list[[1]])
  } else {
    class(result_list) <- "NCM_group"
    return(result_list)
  }
}


#' Summary method for NCM objects
#'
#' @param object An object of class "NCM" (single model).
#' @param ... Additional arguments (not used).
#'
#' @return An object of class "summary_ncm" containing key model statistics.
#' @export
summary.NCM <- function(object, ...) {
  stats <- list(
    m = round(object$m, 4),
    Nm = round(object$Nm, 0),
    rsqr = round(object$rsqr, 4),
    n_species = nrow(object$predictions),
    status_summary = object$status_summary
  )
  class(stats) <- "summary_ncm"
  stats
}


#' Print summary of NCM model
#'
#' @param x An object of class "summary_ncm".
#' @param ... Additional arguments (not used).
#' @return No return value, called for side effects. Prints a formatted table
#'   of NCM results to the console.
#' @export
print.summary_ncm <- function(x, ...) {
  cat("\nNeutral Community Model\n")
  cat(sprintf("  Migration rate (m): %.4f\n", x$m))
  cat(sprintf("  N * m: %d\n", x$Nm))
  cat(sprintf("  R^2: %.4f\n", x$rsqr))
  cat(sprintf("  Number of species: %d\n", x$n_species))
  cat("  Status counts:\n")
  print(x$status_summary, row.names = FALSE)
  invisible(x)
}

#' Summary method for NCM group results
#'
#' @param object An object of class "NCM_group" (list of NCM objects).
#' @param ... Additional arguments (not used).
#'
#' @return An object of class "summary_ncm_group" containing summary for each group.
#' @export
summary.NCM_group <- function(object, ...) {
  # Summarize each group
  summaries <- lapply(object, function(x) {
    if (inherits(x, "NCM")) {
      list(
        m = round(x$m, 4),
        Nm = round(x$Nm, 0),
        rsqr = round(x$rsqr, 4),
        n_species = nrow(x$predictions),
        status_summary = x$status_summary
      )
    } else {
      NULL
    }
  })
  # Remove any NULLs (if any group failed)
  summaries <- summaries[!sapply(summaries, is.null)]
  class(summaries) <- "summary_ncm_group"
  summaries
}

#' Print summary of NCM group results
#'
#' @param x An object of class "summary_ncm_group".
#' @param ... Additional arguments (not used).
#' @return No return value, called for side effects. Prints a formatted table
#'   of NCM group results to the console.
#' @export
print.summary_ncm_group <- function(x, ...) {
  cat("\nNeutral Community Model - Group Summary\n")
  cat(sprintf("%-15s %-10s %-10s %-10s %-10s\n", "Group", "m", "Nm", "R^2", "Species"))
  cat(rep("-", 60), "\n", sep = "")

  for (nm in names(x)) {
    grp <- x[[nm]]
    cat(sprintf(
      "%-15s %-10.4f %-10d %-10.4f %-10d\n",
      nm, grp$m, grp$Nm, grp$rsqr, grp$n_species
    ))
  }
  cat("\n")
  invisible(x)
}
