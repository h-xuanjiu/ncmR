#' @importFrom zip zipr
NULL

# server.R
function(input, output, session) {

  plot_dl_dir <- file.path(tempdir(), "plot_downloads")
  dir.create(plot_dl_dir, showWarnings = FALSE, recursive = TRUE)

  # 添加资源路由（用正斜杠，Shiny 内部会处理）
  shiny::addResourcePath("plot_dls", plot_dl_dir)

  # server 函数开头，清理旧文件
  old_files <- list.files(plot_dl_dir, full.names = TRUE, pattern = "^plot_")
  if (length(old_files) > 0) {
    file_age <- as.numeric(difftime(Sys.time(), file.info(old_files)$mtime, units = "hours"))
    unlink(old_files[file_age > 1])  # 删 1 小时前的
  }

  # 存储数据
  data_vals <- shiny::reactiveValues(
    abundance = NULL,
    group = NULL
  )

  # 辅助函数：读取文件并智能处理行名
  read_data_file <- function(file, sep, header) {
    df <- utils::read.table(file$datapath,
      header = header,
      sep = sep,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      quote = "\""
    )
    if (header && ncol(df) > 0) {
      first_col <- df[[1]]
      if (length(unique(first_col)) == nrow(df)) {
        rownames(df) <- as.character(first_col)
        df <- df[, -1, drop = FALSE]
      }
    }
    df <- as.data.frame(lapply(df, function(x) as.numeric(as.character(x))))
    return(df)
  }

  # 读取分组文件（不强制转换数值）
  read_group_file <- function(file, sep, header) {
    df <- utils::read.table(file$datapath,
      header = header,
      sep = sep,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      quote = "\""
    )
    if (header && ncol(df) > 0) {
      first_col <- df[[1]]
      if (length(unique(first_col)) == nrow(df)) {
        rownames(df) <- as.character(first_col)
        df <- df[, -1, drop = FALSE]
      }
    }
    return(df)
  }

  # 丰度文件上传
  shiny::observeEvent(input$ncm_file, {
    req(input$ncm_file)
    tryCatch(
      {
        df <- read_data_file(input$ncm_file, input$sep, input$header)
        data_vals$abundance <- df
        shiny::showNotification("Abundance data loaded successfully.", type = "message")
      },
      error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  # 分组文件上传
  shiny::observeEvent(input$group_file, {
    req(input$group_file)
    tryCatch(
      {
        df <- read_group_file(input$group_file, input$sep_group, input$header_group)
        data_vals$group <- df
        shiny::showNotification("Group data loaded successfully.", type = "message")
      },
      error = function(e) {
        shiny::showNotification(paste("Error:", e$message), type = "error")
      }
    )
  })

  # 示例数据加载
  shiny::observeEvent(input$example_data, {
    if (requireNamespace("ncmR", quietly = TRUE)) {
      utils::data("example_data", package = "ncmR", envir = environment())
      if (exists("example_data") && is.list(example_data) && all(c("otu", "grp") %in% names(example_data))) {
        data_vals$abundance <- example_data$otu
        data_vals$group <- example_data$grp
        shiny::showNotification("Example data (OTU + group) loaded.", type = "message")
      } else {
        shiny::showNotification("Example data not found in package.", type = "error")
      }
    } else {
      shiny::showNotification("Package ncmR not installed.", type = "error")
    }
  })

  # 动态生成导航栏
  output$data_tabs <- shiny::renderUI({
    choices <- c()
    if (!is.null(data_vals$abundance)) choices <- c(choices, "Abundance Data (OTU)" = "abundance")
    if (!is.null(data_vals$group)) choices <- c(choices, "Group Data" = "group")
    if (length(choices) == 0) {
      return(shiny::helpText("No data loaded. Please upload abundance data or click 'Load Example Data'."))
    }
    shiny::radioButtons("preview_tab", "Select dataset to preview:",
      choices = choices, inline = TRUE
    )
  })

  # ========== 新增：列分页逻辑（每次最多显示 50 列） ==========
  col_page_start <- shiny::reactiveVal(1) # 当前起始列索引（从1开始）
  page_size <- 50 # 每页显示的列数

  # 生成列分页 UI（仅在丰度数据且列数超过 page_size 时显示）
  output$col_page_ui <- shiny::renderUI({
    req(input$preview_tab == "abundance", data_vals$abundance)
    total_cols <- ncol(data_vals$abundance)
    if (total_cols <= page_size) {
      return(NULL)
    }

    current_start <- col_page_start()
    total_pages <- ceiling(total_cols / page_size)
    current_page <- ceiling(current_start / page_size)

    shiny::div(
      style = "margin-bottom: 10px;",
      shiny::actionButton("col_prev", "← Previous 50 columns",
        disabled = (current_page == 1)
      ),
      shiny::span(paste(
        "  Columns", current_start, "to",
        min(current_start + page_size - 1, total_cols),
        "of", total_cols, "  "
      )),
      shiny::actionButton("col_next", "Next 50 columns →",
        disabled = (current_page == total_pages)
      )
    )
  })

  # 上一组按钮
  shiny::observeEvent(input$col_prev, {
    current <- col_page_start()
    new_start <- max(1, current - page_size)
    col_page_start(new_start)
  })

  # 下一组按钮
  shiny::observeEvent(input$col_next, {
    current <- col_page_start()
    total_cols <- ncol(data_vals$abundance)
    new_start <- current + page_size
    if (new_start <= total_cols) col_page_start(new_start)
  })

  # 当丰度数据变化时（重新上传或加载示例），重置列分页起始
  shiny::observeEvent(data_vals$abundance, {
    col_page_start(1)
  })

  # 修改：预览表格，使用列分页（不再使用列选择器）
  output$data_preview <- DT::renderDT({
    req(input$preview_tab)
    df <- switch(input$preview_tab,
      "abundance" = data_vals$abundance,
      "group" = data_vals$group
    )
    req(df)

    # 如果是丰度数据且列数超过 page_size，则只显示当前页的列子集
    if (input$preview_tab == "abundance" && ncol(df) > page_size) {
      start <- col_page_start()
      end <- min(start + page_size - 1, ncol(df))
      df <- df[, start:end, drop = FALSE]
    }

    # 将行名作为第一列显示
    df_display <- cbind(SampleID = rownames(df), df)
    DT::datatable(df_display,
      options = list(pageLength = 10, scrollX = TRUE, scrollY = "400px"),
      style = "bootstrap5",
      rownames = FALSE
    )
  })

  # 根据分组文件动态更新 groups 多选框选项
  shiny::observe({
    req(data_vals$group, input$group_col)
    if (input$group_col %in% colnames(data_vals$group)) {
      group_vals <- unique(data_vals$group[[input$group_col]])
      shiny::updateSelectizeInput(session, "groups", choices = group_vals, selected = NULL)
    } else {
      shiny::updateSelectizeInput(session, "groups", choices = NULL)
    }
  })

  # ========== NCM 拟合 ==========
  # 存储拟合结果
  data_vals$ncm_result <- NULL

  shiny::observeEvent(input$run_ncm, {
    req(data_vals$abundance)

    otu <- data_vals$abundance
    grp <- data_vals$group
    group_col <- input$group_col
    groups <- if (is.null(input$groups) || length(input$groups) == 0) NULL else input$groups

    shiny::showNotification("Fitting NCM model(s)...", type = "message", duration = NULL, id = "fit_msg")

    tryCatch(
      {
        res <- ncmR::fit_ncm(
          otu = otu,
          grp = grp,
          group_col = group_col,
          groups = groups,
          simplify = FALSE,
          return_model = FALSE
        )

        # 存储结果
        data_vals$ncm_result <- res

        shiny::removeNotification(id = "fit_msg")
        shiny::showNotification("Fitting completed.", type = "message")

        session$sendCustomMessage(type = "collapse_data_preview", message = list())
      },
      error = function(e) {
        shiny::removeNotification(id = "fit_msg")
        shiny::showNotification(paste("Fitting failed:", e$message), type = "error")
        data_vals$ncm_result <- NULL
      }
    )
  })


  shiny::observeEvent(data_vals$ncm_result, {
    res <- data_vals$ncm_result
    req(res)

    # 判断是否为分组结果（NCM_group）
    is_grouped <- inherits(res, "NCM_group") || (is.list(res) && !any(c("m", "N", "predictions") %in% names(res)))

    if (is_grouped) {
      # 分组情况：显示组选择器
      group_names <- names(res)
      output$group_selector <- shiny::renderUI({
        shiny::radioButtons("selected_group", "Select group:",
          choices = group_names,
          selected = group_names[1],
          inline = TRUE
        )
      })
    } else {
      # 未分组情况：不显示组选择器
      output$group_selector <- shiny::renderUI({
        NULL
      })
    }

    # 定义一个反应式表达式，返回当前要展示的模型对象
    current_model <- shiny::reactive({
      if (is_grouped) {
        req(input$selected_group)
        res[[input$selected_group]]
      } else {
        res
      }
    })

    # 参数摘要
    output$param_summary <- shiny::renderUI({
      cur <- current_model()
      req(cur)
      m <- round(cur$m, 4)
      N <- round(cur$N, 0)
      Nm <- round(cur$Nm, 0)
      rsqr <- round(cur$rsqr, 3)
      ci_lower <- round(cur$ci[1], 4)
      ci_upper <- round(cur$ci[2], 4)

      shiny::tags$div(
        style = "display: flex; flex-wrap: wrap; gap: 20px; margin-bottom: 10px;",
        shiny::tags$div(shiny::tags$strong("m:"), " ", m),
        shiny::tags$div(shiny::tags$strong("N:"), " ", N),
        shiny::tags$div(shiny::tags$strong("Nm:"), " ", Nm),
        shiny::tags$div(shiny::tags$strong("R²:"), " ", rsqr),
        shiny::tags$div(shiny::tags$strong("CI 95%:"), " [", ci_lower, ", ", ci_upper, "]")
      )
    })

    # 状态汇总表
    output$status_summary_table <- shiny::renderTable(
      {
        cur <- current_model()
        req(cur$status_summary)
        cur$status_summary
      },
      striped = TRUE,
      bordered = FALSE,
      spacing = "xs",
      width = "auto"
    )

    # 预测结果表
    output$predictions_table <- DT::renderDT({
      cur <- current_model()
      req(cur$predictions)
      df <- cur$predictions
      colnames(df) <- c("Species", "p", "Observed Frequency", "Predicted Frequency", "Lower CI", "Upper CI", "Status")
      DT::datatable(df,
        options = list(pageLength = 10, scrollX = TRUE),
        style = "bootstrap5",
        rownames = FALSE
      )
    })
  })

  # 当拟合结果为空时（比如拟合失败或未运行），清空输出
  shiny::observeEvent(data_vals$ncm_result,
    {
      if (is.null(data_vals$ncm_result)) {
        output$group_selector <- shiny::renderUI({
          NULL
        })
        output$param_summary <- shiny::renderUI({
          NULL
        })
        output$status_summary_table <- shiny::renderTable({
          NULL
        })
        output$predictions_table <- DT::renderDT({
          NULL
        })
      }
    },
    ignoreNULL = FALSE
  )

  # 清空所有数据
  shiny::observeEvent(input$clear_data, {
    # 重置文件输入控件
    shinyjs::reset("ncm_file")
    shinyjs::reset("group_file")

    # 清空 reactive 存储的数据
    data_vals$abundance <- NULL
    data_vals$group <- NULL
    data_vals$ncm_result <- NULL

    # 清空分组多选框的选项
    shiny::updateSelectizeInput(session, "groups", choices = NULL, selected = NULL)

    # 清空所有结果输出（可选，因为 data_vals$ncm_result 为 NULL 时会自动触发清空）
    output$group_selector <- shiny::renderUI({
      NULL
    })
    output$param_summary <- shiny::renderUI({
      NULL
    })
    output$status_summary_table <- shiny::renderTable({
      NULL
    })
    output$predictions_table <- DT::renderDT({
      NULL
    })

    # 显示通知
    shiny::showNotification("All data cleared. You can upload new data.", type = "message")
  })

  # 下载结果（打包为 ZIP，分组时每个组一个子文件夹）
  output$download_ncm <- shiny::downloadHandler(
    filename = function() {
      if (!is.null(data_vals$ncm_result)) {
        res <- data_vals$ncm_result
        is_grouped <- inherits(res, "NCM_group") || (is.list(res) && !any(c("m", "N", "predictions") %in% names(res)))
        if (is_grouped) {
          paste0("ncm_results_all_groups_", Sys.Date(), ".zip")
        } else {
          paste0("ncm_results_", Sys.Date(), ".zip")
        }
      } else {
        "ncm_results.zip"
      }
    },
    content = function(file) {
      on.exit({
        session$sendCustomMessage(type = "hideNcmLoading", message = list())
      })
      res <- data_vals$ncm_result
      req(res)

      is_grouped <- inherits(res, "NCM_group") || (is.list(res) && !any(c("m", "N", "predictions") %in% names(res)))

      # 创建临时根目录
      temp_root <- tempdir()
      zip_dir <- tempfile(pattern = "ncm_export_", tmpdir = temp_root)
      dir.create(zip_dir, showWarnings = FALSE)

      old_wd <- getwd()
      on.exit({
        setwd(old_wd)
        unlink(zip_dir, recursive = TRUE)
      }, add = TRUE)

      if (is_grouped) {
        # 分组情况：遍历每个组
        group_names <- names(res)
        for (grp in group_names) {
          cur <- res[[grp]]
          if (is.null(cur)) next

          safe_grp <- gsub("[^A-Za-z0-9_.-]", "_", grp)
          group_folder <- paste0("Group_", safe_grp)
          group_dir <- file.path(zip_dir, group_folder)
          dir.create(group_dir, showWarnings = FALSE, recursive = TRUE)

          # 1. 生成 summary.txt
          summary_file <- file.path(group_dir, "summary.txt")
          m <- round(cur$m, 4)
          N <- round(cur$N, 0)
          Nm <- round(cur$Nm, 0)
          rsqr <- round(cur$rsqr, 3)
          ci_lower <- round(cur$ci[1], 4)
          ci_upper <- round(cur$ci[2], 4)

          summary_lines <- c(
            paste0("Neutral Community Model Results for Group: ", grp),
            paste0("Date: ", Sys.Date()),
            "",
            "=== Model Parameters ===",
            paste0("m  = ", m),
            paste0("N  = ", N),
            paste0("Nm = ", Nm),
            paste0("R² = ", rsqr),
            paste0("95% CI for m: [", ci_lower, ", ", ci_upper, "]"),
            "",
            "=== Status Summary ==="
          )
          if (!is.null(cur$status_summary)) {
            sm <- cur$status_summary
            col_widths <- c(10, 10, 12)
            header <- sprintf("%-*s %-*s %-*s", col_widths[1], "Status", col_widths[2], "Count", col_widths[3], "Percentage(%)")
            separator <- paste(rep("-", nchar(header)), collapse = "")
            summary_lines <- c(summary_lines, header, separator)
            for (i in 1:nrow(sm)) {
              line <- sprintf(
                "%-*s %-*d %-*.2f",
                col_widths[1], sm$status[i],
                col_widths[2], sm$n[i],
                col_widths[3], sm$percentage[i]
              )
              summary_lines <- c(summary_lines, line)
            }
          } else {
            summary_lines <- c(summary_lines, "No status summary available.")
          }
          writeLines(summary_lines, summary_file)

          # 2. 生成 predictions.csv
          pred_file <- file.path(group_dir, "predictions.csv")
          if (!is.null(cur$predictions)) {
            utils::write.csv(cur$predictions, pred_file, row.names = FALSE)
          } else {
            writeLines("No predictions available.", pred_file)
          }
        }
      } else {
        # 未分组情况：单个模型，文件放在根目录
        cur <- res
        summary_file <- file.path(zip_dir, "summary.txt")
        pred_file <- file.path(zip_dir, "predictions.csv")

        m <- round(cur$m, 4)
        N <- round(cur$N, 0)
        Nm <- round(cur$Nm, 0)
        rsqr <- round(cur$rsqr, 3)
        ci_lower <- round(cur$ci[1], 4)
        ci_upper <- round(cur$ci[2], 4)

        summary_lines <- c(
          "Neutral Community Model Results",
          paste0("Date: ", Sys.Date()),
          "",
          "=== Model Parameters ===",
          paste0("m  = ", m),
          paste0("N  = ", N),
          paste0("Nm = ", Nm),
          paste0("R² = ", rsqr),
          paste0("95% CI for m: [", ci_lower, ", ", ci_upper, "]"),
          "",
          "=== Status Summary ==="
        )
        if (!is.null(cur$status_summary)) {
          sm <- cur$status_summary
          col_widths <- c(10, 10, 12)
          header <- sprintf("%-*s %-*s %-*s", col_widths[1], "Status", col_widths[2], "Count", col_widths[3], "Percentage(%)")
          separator <- paste(rep("-", nchar(header)), collapse = "")
          summary_lines <- c(summary_lines, header, separator)
          for (i in 1:nrow(sm)) {
            line <- sprintf(
              "%-*s %-*d %-*.2f",
              col_widths[1], sm$status[i],
              col_widths[2], sm$n[i],
              col_widths[3], sm$percentage[i]
            )
            summary_lines <- c(summary_lines, line)
          }
        } else {
          summary_lines <- c(summary_lines, "No status summary available.")
        }
        writeLines(summary_lines, summary_file)
        if (!is.null(cur$predictions)) {
          utils::write.csv(cur$predictions, pred_file, row.names = FALSE)
        } else {
          writeLines("No predictions available.", pred_file)
        }
      }

      setwd(zip_dir)
      # 列出顶层所有文件和目录
      all_items <- list.files(zip_dir, all.files = FALSE, recursive = FALSE, include.dirs = TRUE)
      if (length(all_items) == 0) {
        writeLines("No results to download.", "README.txt")
        all_items <- "README.txt"
      }
      # 使用 mirror 模式，递归打包，保留目录结构
      zip::zipr(file,
        files = all_items, recurse = TRUE,
        include_directories = TRUE, mode = "mirror"
      )
    }
  )

  # 控制 Plotting 页面两个卡片的显隐
  shiny::observeEvent(input$plot_data_source, {
    if (input$plot_data_source == "from_ncm") {
      shinyjs::show("fitting_card")
      shinyjs::hide("upload_card")
    } else {
      shinyjs::hide("fitting_card")
      shinyjs::show("upload_card")
    }
  })
  # 初始状态：默认选中 from_ncm，显示 fitting_card，隐藏 upload_card
  shinyjs::hide("upload_card")
  shinyjs::show("fitting_card")

  # 当用户上传文件时，动态生成列映射下拉框
  shiny::observeEvent(input$plot_file, {
    req(input$plot_file)
    df <- tryCatch({
      utils::read.table(input$plot_file$datapath,
                        header = input$plot_header,
                        sep = input$plot_sep,
                        stringsAsFactors = FALSE)
    }, error = function(e) NULL)
    req(df)
    col_names <- colnames(df)

    output$column_mapping_ui <- shiny::renderUI({
      shiny::tagList(
        shiny::selectInput("map_p", "Map to 'p'",
                           choices = c("None" = "", col_names), selected = ""),
        shiny::selectInput("map_freq", "Map to 'freq'",
                           choices = c("None" = "", col_names), selected = ""),
        shiny::selectInput("map_freq_pred", "Map to 'freq.pred'",
                           choices = c("None" = "", col_names), selected = ""),
        shiny::selectInput("map_lower", "Map to 'lower'",
                           choices = c("None" = "", col_names), selected = ""),
        shiny::selectInput("map_upper", "Map to 'upper'",
                           choices = c("None" = "", col_names), selected = "")
      )
    })
  })

  # 上传数据预览表格
  output$uploaded_data_preview <- DT::renderDT({
    req(input$plot_file, input$plot_header, input$plot_sep)
    df <- utils::read.table(input$plot_file$datapath,
                            header = input$plot_header,
                            sep = input$plot_sep,
                            stringsAsFactors = FALSE)
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE),
                  style = "bootstrap5", rownames = FALSE)
  })


  # ==================== Plotting 页面：从拟合结果展示卡片 ====================
  # 组选择器（如果有分组）
  output$plot_group_selector <- shiny::renderUI({
    if (is.null(data_vals$ncm_result)) return(NULL)
    res <- data_vals$ncm_result
    is_grouped <- inherits(res, "NCM_group") || (is.list(res) && !any(c("m", "N", "predictions") %in% names(res)))
    if (is_grouped) {
      group_names <- names(res)
      shiny::radioButtons("plot_selected_group", "Select group:",
                          choices = group_names,
                          selected = group_names[1],
                          inline = TRUE)
    } else {
      NULL
    }
  })

  # 当前选中的模型（用于卡片内展示）
  plot_current_model <- shiny::reactive({
    if (is.null(data_vals$ncm_result)) return(NULL)
    res <- data_vals$ncm_result
    is_grouped <- inherits(res, "NCM_group") || (is.list(res) && !any(c("m", "N", "predictions") %in% names(res)))
    if (is_grouped) {
      req(input$plot_selected_group)
      res[[input$plot_selected_group]]
    } else {
      res
    }
  })

  # 参数摘要（如果无结果则显示提示）
  output$plot_param_summary <- shiny::renderUI({
    if (is.null(data_vals$ncm_result)) {
      return(shiny::tags$div(
        style = "padding: 20px; text-align: center;",
        shiny::tags$p("No NCM fitting results available. Please go to the 'Fit NCM' page and run the analysis first.",
                      style = "color: #856404; background-color: #fff3cd; padding: 10px; border-radius: 5px;")
      ))
    }
    cur <- plot_current_model()
    req(cur)
    m <- round(cur$m, 4)
    N <- round(cur$N, 0)
    Nm <- round(cur$Nm, 0)
    rsqr <- round(cur$rsqr, 3)
    ci_lower <- round(cur$ci[1], 4)
    ci_upper <- round(cur$ci[2], 4)
    shiny::tags$div(
      style = "display: flex; flex-wrap: wrap; gap: 20px; margin-bottom: 10px;",
      shiny::tags$div(shiny::tags$strong("m:"), " ", m),
      shiny::tags$div(shiny::tags$strong("N:"), " ", N),
      shiny::tags$div(shiny::tags$strong("Nm:"), " ", Nm),
      shiny::tags$div(shiny::tags$strong("R²:"), " ", rsqr),
      shiny::tags$div(shiny::tags$strong("CI 95%:"), " [", ci_lower, ", ", ci_upper, "]")
    )
  })

  # 状态汇总表
  output$plot_status_summary_table <- shiny::renderTable({
    if (is.null(data_vals$ncm_result)) return(NULL)
    cur <- plot_current_model()
    req(cur$status_summary)
    cur$status_summary
  }, striped = TRUE, bordered = FALSE, spacing = "xs", width = "auto")

  # 预测表
  output$plot_predictions_table <- DT::renderDT({
    if (is.null(data_vals$ncm_result)) return(NULL)
    cur <- plot_current_model()
    req(cur$predictions)
    df <- cur$predictions
    colnames(df) <- c("Species", "p", "Observed Frequency", "Predicted Frequency", "Lower CI", "Upper CI", "Status")
    DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE),
                  style = "bootstrap5", rownames = FALSE)
  })


  # 获取所有绘图参数（公共样式）
  plot_style_args <- shiny::reactive({
    list(
      point_alpha = input$point_alpha,
      point_size = input$point_size,
      point_colors = c(Above = input$point_color_above,
                       Below = input$point_color_below,
                       Neutral = input$point_color_neutral),
      fit_line_color = input$fit_line_color,
      fit_line_type = input$fit_line_type,
      fit_line_size = input$fit_line_size,
      ci_line_color = input$ci_line_color,
      ci_line_type = input$ci_line_type,
      ci_line_size = input$ci_line_size,
      axis_title_x_text = input$axis_title_x_text,
      axis_title_y_text = input$axis_title_y_text,
      axis_title_x_size = input$axis_title_x_size,
      axis_title_y_size = input$axis_title_y_size,
      axis_text_x_size = input$axis_text_x_size,
      axis_text_y_size = input$axis_text_y_size,
      legend_title_text = if (is.na(input$legend_title_text)) NULL else input$legend_title_text,
      legend_size = input$legend_size,
      legend_position = c(input$legend_position_x, input$legend_position_y),
      legend_hjust = input$legend_hjust,
      legend_vjust = input$legend_vjust,
      fit_para_size = input$fit_para_size,
      fit_para_position = c(input$fit_para_position_x, input$fit_para_position_y),
      fit_para_hjust = input$fit_para_hjust,
      fit_para_vjust = input$fit_para_vjust,
      font_family = input$font_family
    )
  })


  # 获取上传数据的独特参数（列映射 + 模型参数）
  upload_data_args <- shiny::reactive({
    map <- list()
    if (!is.null(input$map_p) && input$map_p != "") map$p <- input$map_p
    if (!is.null(input$map_freq) && input$map_freq != "") map$freq <- input$map_freq
    if (!is.null(input$map_freq_pred) && input$map_freq_pred != "") map$freq.pred <- input$map_freq_pred
    if (!is.null(input$map_lower) && input$map_lower != "") map$lower <- input$map_lower
    if (!is.null(input$map_upper) && input$map_upper != "") map$upper <- input$map_upper

    list(
      map = map,
      rsqr = if (!is.na(input$rsqr)) input$rsqr else NULL,
      Nm = if (!is.na(input$Nm)) input$Nm else NULL,
      m = if (!is.na(input$m)) input$m else NULL
    )
  })

  plot_obj <- shiny::eventReactive(input$make_plot, {

    # 折叠数据预览面板
    bslib::accordion_panel_close(id = "plot_data_accordion",
                                 values = c("fitting_card", "upload_card"))

    source <- input$plot_data_source
    p <- NULL  # 初始化绘图对象

    if (source == "upload_own") {
      shiny::req(input$plot_file)
      tryCatch({
        df <- utils::read.table(input$plot_file$datapath,
                                header = input$plot_header,
                                sep = input$plot_sep,
                                stringsAsFactors = FALSE)
        upload_args <- upload_data_args()
        style_args <- plot_style_args()
        args <- list(object = df,
                     rsqr = upload_args$rsqr,
                     Nm = upload_args$Nm,
                     m = upload_args$m)
        if (length(upload_args$map) > 0) {
          args$map <- upload_args$map
        }
        args <- c(args, style_args)
        p <- do.call(ncmR::scatter_plot, args)

        # 成功通知
        shiny::showNotification("Plot generated successfully!", type = "message")

      }, error = function(e) {
        shiny::showNotification(paste("Uploaded data plotting failed:", e$message), type = "error", duration = 10)
        p <<- NULL
      })

    } else if (source == "from_ncm") {
      shiny::req(data_vals$ncm_result)
      tryCatch({
        res <- data_vals$ncm_result
        is_grouped <- inherits(res, "NCM_group") || (is.list(res) && !any(c("m", "N", "predictions") %in% names(res)))
        if (is_grouped) {
          shiny::req(input$plot_selected_group)
          cur <- res[[input$plot_selected_group]]
        } else {
          cur <- res
        }
        shiny::req(cur)
        style_args <- plot_style_args()
        p <- do.call(ncmR::scatter_plot, c(list(object = cur), style_args))

        # 成功通知
        shiny::showNotification("Plot generated successfully!", type = "message")

      }, error = function(e) {
        shiny::showNotification(paste("Fitted result plotting failed:", e$message), type = "error", duration = 10)
        p <<- NULL
      })
    }

    return(p)
  })

  output$ncm_plot <- shiny::renderPlot({
    p <- plot_obj()
    if (is.null(p)) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0, 0, "Plot generation failed. Check the error notification.", col = "red", cex = 1.2)
    } else {
      p
    }
  })


  # === 模态框：设置参数 ===
  shiny::observeEvent(input$download_plot_btn, {
    p <- plot_obj()
    if (is.null(p)) {
      shiny::showNotification("No plot to download.", type = "error")
      return()
    }

    shiny::showModal(shiny::modalDialog(
      title = "Download Plot",
      size = "m",
      easyClose = TRUE,

      shiny::textInput("dl_filename", "File name", value = paste0("plot_", Sys.Date())),
      shiny::selectInput("dl_format", "Format",
                         choices = c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg",
                                     "TIFF" = "tiff"),
                         selected = "pdf"),
      shiny::numericInput("dl_width", "Width (inches)", value = 10, min = 1, step = 0.5),
      shiny::numericInput("dl_height", "Height (inches)", value = 8, min = 1, step = 0.5),
      shiny::conditionalPanel(
        condition = "input.dl_format != 'pdf'",
        shiny::numericInput("dl_dpi", "DPI", value = 300, min = 72, max = 2000, step = 50)
      ),

      footer = shiny::tagList(
        shiny::modalButton("Cancel"),
        shiny::tags$button("Download", class = "btn btn-primary",
                           onclick = "Shiny.setInputValue('do_plot_download', Math.random())")
      )
    ))
  })

  shiny::observeEvent(input$do_plot_download, {
    shiny::removeModal()

    p <- plot_obj()
    shiny::req(p)

    fmt <- input$dl_format
    fname <- input$dl_filename
    w <- input$dl_width
    h <- input$dl_height
    raw_dpi <- if (!is.null(input$dl_dpi)) as.numeric(input$dl_dpi) else 300
    dpi <- min(max(raw_dpi, 72), 2000)

    safe_name <- gsub("[^A-Za-z0-9_.-]", "_", fname)
    if (!grepl(paste0("\\.", fmt, "$"), safe_name)) {
      safe_name <- paste0(safe_name, ".", fmt)
    }

    # 唯一文件名，防止多用户冲突
    temp_file <- tempfile(pattern = "plot_", tmpdir = plot_dl_dir, fileext = paste0(".", fmt))

    tryCatch({
      if (fmt == "pdf") {
        ggplot2::ggsave(temp_file, plot = p, width = w, height = h, device = cairo_pdf)
      } else {
        ggplot2::ggsave(temp_file, plot = p, width = w, height = h, dpi = dpi, device = fmt)
      }

      session$sendCustomMessage("download_plot_url", list(
        url = paste0("plot_dls/", basename(temp_file)),  # 确认前缀是 plot_dls
        filename = safe_name
      ))

      # 300 秒后自动清理临时文件
      later::later(function() {
        if (file.exists(temp_file)) unlink(temp_file)
      }, delay = 300)

    }, error = function(e) {
      session$sendCustomMessage(type = "hidePlotLoading", message = list())
      shiny::showNotification(paste("Error saving plot:", e$message), type = "error")
    })
  })

  # MIME 类型辅助函数
  mime_type <- function(fmt) {
    switch(fmt,
           pdf = "application/pdf",
           png = "image/png",
           jpeg = "image/jpeg",
           tiff = "image/tiff",
           "application/octet-stream"
    )
  }

}
