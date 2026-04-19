utils::globalVariables(c("freq", "freq.pred", "lower", "upper", "x", "y"))
#' @importFrom dplyr %>%
NULL
#' @importFrom DT DTOutput
NULL
#' @importFrom bslib bs_theme
NULL
#' @importFrom shinyjs useShinyjs
NULL
#' @importFrom zip zipr
NULL

#'
#' Scatter plot for NCM results and data frames
#'
#' @description
#' Generic function for creating scatter plots. Methods available for
#' NCM result objects and data frames.
#'
#' @param object An object to plot: NCM result or data.frame
#' @param ... Additional arguments passed to methods
#'
#' @returns A ggplot object
#' @export
#'
scatter_plot <- function(object, ...) {
  UseMethod("scatter_plot")
}

#' @describeIn scatter_plot Plot NCM result with fitted curve, confidence
#'   intervals, and parameter annotations. Supports full customization of
#'   visual elements.
#' @param point_alpha Alpha transparency for points (default: 0.8)
#' @param point_size Point size (default: 3)
#' @param point_colors Named vector of colors for Above/Below/Neutral status
#' @param fit_line_color Fitted line color (default: "#335399")
#' @param fit_line_type Line type for fitted curve (default: "solid")
#' @param fit_line_size Line width for fitted curve (default: 1)
#' @param ci_line_color Confidence interval line color (default: "#335399")
#' @param ci_line_type Confidence interval line type (default: "dashed")
#' @param ci_line_size Confidence interval line width (default: 1)
#' @param axis_title_x_text X-axis title text
#' @param axis_title_y_text Y-axis title text
#' @param axis_title_x_size X-axis title font size (default: 25)
#' @param axis_title_y_size Y-axis title font size (default: 25)
#' @param axis_text_x_size X-axis tick label font size (default: 20)
#' @param axis_text_y_size Y-axis tick label font size (default: 20)
#' @param legend_title_text Legend title text (default: NA)
#' @param legend_size Legend text size (default: 6)
#' @param legend_position Legend position as NPC coordinates c(x, y) (default: c(0.80, 0.40))
#' @param legend_hjust Legend horizontal justification (default: 0)
#' @param legend_vjust Legend vertical justification (default: 1)
#' @param fit_para_size Fitting parameter text size (default: 6)
#' @param fit_para_position Parameter annotation position as NPC coordinates c(x, y) (default: c(0.02, 0.98))
#' @param fit_para_hjust Parameter horizontal justification (default: 0)
#' @param fit_para_vjust Parameter vertical justification (default: 1)
#' @param font_family Font family (default: "sans")
#' @exportS3Method
#'
#' @examples
#' # Load example data
#' data(example_data)
#' otu <- example_data$otu
#'
#' # fit the total model using all samples
#' res_total <- fit_ncm(otu)
#'
#' # plot
#' scatter_plot(res_total)
#'
scatter_plot.NCM <- function(
    object,
    point_alpha = 0.8,
    point_size = 3,
    point_colors = c("Above" = "#ED7D70", "Below" = "#2B889B", "Neutral" = "#B57FAF"),
    fit_line_color = "#335399",
    fit_line_type = "solid",
    fit_line_size = 1,
    ci_line_color = "#335399",
    ci_line_type = "dashed",
    ci_line_size = 1,
    axis_title_x_text = "Mean relative abundance (log10)",
    axis_title_y_text = "Frequency of occupancy",
    axis_title_x_size = 25,
    axis_title_y_size = 25,
    axis_text_x_size = 20,
    axis_text_y_size = 20,
    legend_title_text = NA,
    legend_size = 6,
    legend_position = c(0.80, 0.40),
    legend_hjust = 0,
    legend_vjust = 1,
    fit_para_size = 6,
    fit_para_position = c(0.02, 0.98),
    fit_para_hjust = 0,
    fit_para_vjust = 1,
    font_family = "sans",
    ...) {
  expected_status <- c("Above", "Below", "Neutral")

  if (length(point_colors) < 3) {
    stop("point_colors must have exactly 3 colors for Above, Below, Neutral",
      call. = FALSE
    )
  } else if (length(point_colors) > 3) {
    warning("point_colors has more than 3 colors, using first 3 only",
      call. = FALSE
    )
    point_colors <- point_colors[1:3]
  }

  if (is.null(names(point_colors))) {
    names(point_colors) <- expected_status
  } else {
    if (!setequal(names(point_colors), expected_status)) {
      missing_names <- setdiff(expected_status, names(point_colors))
      extra_names <- setdiff(names(point_colors), expected_status)

      msg <- "point_colors names must be Above, Below, Neutral"
      if (length(missing_names) > 0) {
        msg <- paste0(msg, "\n  Missing: ", paste(missing_names, collapse = ", "))
      }
      if (length(extra_names) > 0) {
        msg <- paste0(msg, "\n  Extra: ", paste(extra_names, collapse = ", "))
      }
      stop(msg, call. = FALSE)
    }

    if (!identical(names(point_colors), expected_status)) {
      point_colors <- point_colors[expected_status]
    }
  }

  if (length(legend_position) != 2) {
    stop("legend_position must be length 2 (x, y)", call. = FALSE)
  }
  if (length(fit_para_position) != 2) {
    stop("fit_para_position must be length 2 (x, y)", call. = FALSE)
  }

  if (any(legend_position < 0 | legend_position > 1)) {
    warning("legend_position values outside [0,1] may place legend outside plot")
  }
  if (any(fit_para_position < 0 | fit_para_position > 1)) {
    warning("fit_para_position values outside [0,1] may place text outside plot")
  }

  make_label <- function(list) {
    paste0(
      "<span style='white-space:pre'><i>R</i><sup>2</sup>&#12288; = ", sprintf("%.4f", list$rsqr), "</span><br>",
      "<span style='white-space:pre'>Nm&#12288; = ", sprintf("%.0f", list$Nm), "</span><br>",
      "<span style='white-space:pre'>m&#12288; = ", sprintf("%.4f", list$m), "</span>"
    )
  }

  plotdata <- object$predictions %>%
    dplyr::mutate(status = dplyr::case_when(
      freq > upper ~ "Above",
      freq < lower ~ "Below",
      TRUE ~ "Neutral"
    ))

  make_legend_html <- function(data, title, point_colors) {
    perc <- prop.table(table(data)) * 100
    if (is.na(title)) title <- ""

    line_span <- function(color, label, pct) {
      paste0(
        "<span style='color:", color, ";'>",
        label, "  ", sprintf("%.2f", pct), "%",
        "</span>"
      )
    }

    paste0(
      "<p style='margin:0; white-space:pre;'>",
      "<b style='font-size:25px;'>", title, "</b><br>",
      line_span(point_colors["Neutral"], "Neutral", perc["Neutral"]), "<br>",
      line_span(point_colors["Below"], "Below", perc["Below"]), "<br>",
      line_span(point_colors["Above"], "Above", perc["Above"]),
      "</p>"
    )
  }


  p <- ggplot2::ggplot(plotdata, ggplot2::aes(x = log10(p), y = freq)) +
    ggplot2::geom_point(ggplot2::aes(colour = status), alpha = point_alpha, size = point_size) +
    ggplot2::geom_line(ggplot2::aes(y = freq.pred), color = fit_line_color, linewidth = fit_line_size, linetype = fit_line_type) +
    ggplot2::geom_line(ggplot2::aes(y = lower), color = ci_line_color, linetype = ci_line_type, linewidth = ci_line_size) +
    ggplot2::geom_line(ggplot2::aes(y = upper), color = ci_line_color, linetype = ci_line_type, linewidth = ci_line_size) +
    ggplot2::labs(
      x = axis_title_x_text,
      y = axis_title_y_text
    ) +
    ggplot2::scale_color_manual(values = point_colors) +
    ggplot2::theme_bw(base_rect_size = 1) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major = ggplot2::element_line(colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(size = axis_title_y_size, family = font_family, colour = "black", vjust = 0.5, hjust = 0.5, margin = ggplot2::margin(r = 15)),
      axis.title.x = ggplot2::element_text(size = axis_title_x_size, family = font_family, colour = "black", vjust = 0.5, hjust = 0.5, margin = ggplot2::margin(t = 15)),
      axis.text.x = ggplot2::element_text(size = axis_text_x_size, family = font_family, colour = "black", vjust = 0.5, hjust = 0.5, margin = ggplot2::margin(t = 8)),
      axis.text.y = ggplot2::element_text(size = axis_text_y_size, family = font_family, colour = "black", vjust = 0.5, hjust = 1, margin = ggplot2::margin(r = 8)),
      panel.border = ggplot2::element_rect(linewidth = 1),
      plot.margin = ggplot2::unit(c(1, 1, 1, 1), "lines"),
      axis.ticks = ggplot2::element_line(linewidth = 0.75),
      axis.ticks.length = ggplot2::unit(-0.10, "cm")
    )
  npc_to_data <- function(plot, npc) {
    gb <- ggplot2::ggplot_build(plot)

    x_range <- gb$layout$panel_scales_x[[1]]$range$range
    y_range <- gb$layout$panel_scales_y[[1]]$range$range

    c(
      x = x_range[1] + npc[1] * diff(x_range),
      y = y_range[1] + npc[2] * diff(y_range)
    )
  }

  fit_para_position_data <- npc_to_data(p, fit_para_position)
  legend_position_data <- npc_to_data(p, legend_position)

  p <- p +
    ggtext::geom_richtext(
      data = data.frame(x = fit_para_position_data[1], y = fit_para_position_data[2]),
      ggplot2::aes(x = x, y = y, label = make_label(object)),
      hjust = fit_para_hjust, vjust = fit_para_vjust,
      size = fit_para_size, family = font_family,
      fill = NA, label.color = NA
    ) +
    ggtext::geom_richtext(
      data = data.frame(x = legend_position_data[1], y = legend_position_data[2]),
      ggplot2::aes(x = x, y = y, label = make_legend_html(plotdata$status, legend_title_text, point_colors)),
      hjust = legend_hjust, vjust = legend_vjust,
      size = legend_size, family = font_family,
      fill = NA, label.color = NA
    )
  class(p) <- c("unicode_ggplot", class(p))
  return(p)
}


#' @describeIn scatter_plot Plot data.frame by converting to NCM object.
#'   Requires manual specification of model parameters (rsqr, Nm, m) and
#'   optional column name mapping.
#' @param rsqr R-squared value from NCM fit
#' @param Nm Nm parameter from NCM fit
#' @param m m parameter from NCM fit
#' @param map Named vector for column name mapping (e.g., c(p = "abundance"))
#' @exportS3Method
#' @examples
#' # Load example data
#' data(example_data)
#' otu <- example_data$otu
#'
#' # fit the total model using all samples
#' res_total <- fit_ncm(otu)
#'
#' df <- res_total$predictions
#'
#' # Plot with manual parameters
#' scatter_plot(df, rsqr = 0.95, Nm = 500, m = 0.5678)
scatter_plot.data.frame <- function(
    object,
    rsqr,
    Nm,
    m,
    map,
    point_alpha = 0.8,
    point_size = 3,
    point_colors = c("Above" = "#ED7D70", "Below" = "#2B889B", "Neutral" = "#B57FAF"),
    fit_line_color = "#335399",
    fit_line_type = "solid",
    fit_line_size = 1,
    ci_line_color = "#335399",
    ci_line_type = "dashed",
    ci_line_size = 1,
    axis_title_x_text = "Mean relative abundance (log10)",
    axis_title_y_text = "Frequency of occupancy",
    axis_title_x_size = 25,
    axis_title_y_size = 25,
    axis_text_x_size = 20,
    axis_text_y_size = 20,
    legend_title_text = NA,
    legend_size = 6,
    legend_position = c(0.80, 0.40),
    legend_hjust = 0,
    legend_vjust = 1,
    fit_para_size = 6,
    fit_para_position = c(0.02, 0.98),
    fit_para_hjust = 0,
    fit_para_vjust = 1,
    font_family = "sans",
    ...) {
  if (!all(c("p", "freq", "freq.pred", "lower", "upper") %in% colnames(object))) {
    missing <- setdiff(c("p", "freq", "freq.pred", "lower", "upper"), colnames(object))
    not_in_map <- setdiff(missing, names(map))

    if (length(not_in_map) > 0) {
      stop("Missing columns not in map: ", paste(not_in_map, collapse = ", "))
    }

    names(object)[match(map[missing], names(object))] <- missing
  }

  expected_status <- c("Above", "Below", "Neutral")

  if (length(point_colors) < 3) {
    stop("point_colors must have exactly 3 colors for Above, Below, Neutral",
      call. = FALSE
    )
  } else if (length(point_colors) > 3) {
    warning("point_colors has more than 3 colors, using first 3 only",
      call. = FALSE
    )
    point_colors <- point_colors[1:3]
  }

  if (is.null(names(point_colors))) {
    names(point_colors) <- expected_status
  } else {
    if (!setequal(names(point_colors), expected_status)) {
      missing_names <- setdiff(expected_status, names(point_colors))
      extra_names <- setdiff(names(point_colors), expected_status)

      msg <- "point_colors names must be Above, Below, Neutral"
      if (length(missing_names) > 0) {
        msg <- paste0(msg, "\n  Missing: ", paste(missing_names, collapse = ", "))
      }
      if (length(extra_names) > 0) {
        msg <- paste0(msg, "\n  Extra: ", paste(extra_names, collapse = ", "))
      }
      stop(msg, call. = FALSE)
    }

    if (!identical(names(point_colors), expected_status)) {
      point_colors <- point_colors[expected_status]
    }
  }

  if (length(legend_position) != 2) {
    stop("legend_position must be length 2 (x, y)", call. = FALSE)
  }
  if (length(fit_para_position) != 2) {
    stop("fit_para_position must be length 2 (x, y)", call. = FALSE)
  }

  if (any(legend_position < 0 | legend_position > 1)) {
    warning("legend_position values outside [0,1] may place legend outside plot")
  }
  if (any(fit_para_position < 0 | fit_para_position > 1)) {
    warning("fit_para_position values outside [0,1] may place text outside plot")
  }

  make_label <- function(list) {
    paste0(
      "<span style='white-space:pre'><i>R</i><sup>2</sup>&#12288; = ", sprintf("%.4f", rsqr), "</span><br>",
      "<span style='white-space:pre'>Nm&#12288; = ", sprintf("%.0f", Nm), "</span><br>",
      "<span style='white-space:pre'>m&#12288; = ", sprintf("%.4f", m), "</span>"
    )
  }

  plotdata <- object %>%
    dplyr::mutate(status = dplyr::case_when(
      freq > upper ~ "Above",
      freq < lower ~ "Below",
      TRUE ~ "Neutral"
    ))

  make_legend_html <- function(data, title, point_colors) {
    perc <- prop.table(table(data)) * 100
    if (is.na(title)) title <- ""

    line_span <- function(color, label, pct) {
      paste0(
        "<span style='color:", color, ";'>",
        label, "  ", sprintf("%.2f", pct), "%",
        "</span>"
      )
    }

    paste0(
      "<p style='margin:0; white-space:pre;'>",
      "<b style='font-size:25px;'>", title, "</b><br>",
      line_span(point_colors["Neutral"], "Neutral", perc["Neutral"]), "<br>",
      line_span(point_colors["Below"], "Below", perc["Below"]), "<br>",
      line_span(point_colors["Above"], "Above", perc["Above"]),
      "</p>"
    )
  }


  p <- ggplot2::ggplot(plotdata, ggplot2::aes(x = log10(p), y = freq)) +
    ggplot2::geom_point(ggplot2::aes(colour = status), alpha = point_alpha, size = point_size) +
    ggplot2::geom_line(ggplot2::aes(y = freq.pred), color = fit_line_color, linewidth = fit_line_size, linetype = fit_line_type) +
    ggplot2::geom_line(ggplot2::aes(y = lower), color = ci_line_color, linetype = ci_line_type, linewidth = ci_line_size) +
    ggplot2::geom_line(ggplot2::aes(y = upper), color = ci_line_color, linetype = ci_line_type, linewidth = ci_line_size) +
    ggplot2::labs(
      x = axis_title_x_text,
      y = axis_title_y_text
    ) +
    ggplot2::scale_color_manual(values = point_colors) +
    ggplot2::theme_bw(base_rect_size = 1) +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.major = ggplot2::element_line(colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(size = axis_title_y_size, family = font_family, colour = "black", vjust = 0.5, hjust = 0.5, margin = ggplot2::margin(r = 15)),
      axis.title.x = ggplot2::element_text(size = axis_title_x_size, family = font_family, colour = "black", vjust = 0.5, hjust = 0.5, margin = ggplot2::margin(t = 15)),
      axis.text.x = ggplot2::element_text(size = axis_text_x_size, family = font_family, colour = "black", vjust = 0.5, hjust = 0.5, margin = ggplot2::margin(t = 8)),
      axis.text.y = ggplot2::element_text(size = axis_text_y_size, family = font_family, colour = "black", vjust = 0.5, hjust = 1, margin = ggplot2::margin(r = 8)),
      panel.border = ggplot2::element_rect(linewidth = 1),
      plot.margin = ggplot2::unit(c(1, 1, 1, 1), "lines"),
      axis.ticks = ggplot2::element_line(linewidth = 0.75),
      axis.ticks.length = ggplot2::unit(-0.10, "cm")
    )
  npc_to_data <- function(plot, npc) {
    gb <- ggplot2::ggplot_build(plot)

    x_range <- gb$layout$panel_scales_x[[1]]$range$range
    y_range <- gb$layout$panel_scales_y[[1]]$range$range

    c(
      x = x_range[1] + npc[1] * diff(x_range),
      y = y_range[1] + npc[2] * diff(y_range)
    )
  }

  fit_para_position_data <- npc_to_data(p, fit_para_position)
  legend_position_data <- npc_to_data(p, legend_position)

  p <- p +
    ggtext::geom_richtext(
      data = data.frame(x = fit_para_position_data[1], y = fit_para_position_data[2]),
      ggplot2::aes(x = x, y = y, label = make_label(object)),
      hjust = fit_para_hjust, vjust = fit_para_vjust,
      size = fit_para_size, family = font_family,
      fill = NA, label.color = NA
    ) +
    ggtext::geom_richtext(
      data = data.frame(x = legend_position_data[1], y = legend_position_data[2]),
      ggplot2::aes(x = x, y = y, label = make_legend_html(plotdata$status, legend_title_text, point_colors)),
      hjust = legend_hjust, vjust = legend_vjust,
      size = legend_size, family = font_family,
      fill = NA, label.color = NA
    )
  class(p) <- c("unicode_ggplot", class(p))
  return(p)
}

#' @describeIn scatter_plot Default method for unsupported types.
#' @exportS3Method
scatter_plot.default <- function(object, ...) {
  # Error for unsupported input types
  stop("Unsupported input type: ", paste(class(object), collapse = "/"), call. = FALSE)
}


