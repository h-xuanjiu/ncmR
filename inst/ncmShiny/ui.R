#' @importFrom DT DTOutput
NULL
#' @importFrom bslib bs_theme
NULL
#' @importFrom shinyjs useShinyjs
NULL


# 简单有效的 CSS：将导航菜单推到右侧
navbar_css <- "
.navbar .navbar-collapse {
  justify-content: flex-end;
}
.navbar-nav {
  flex-direction: row;
}
.navbar-nav .nav-item {
  margin-left: 0.75rem;
  margin-right: 0.75rem;
}
"

shiny::tagList(
  shinyjs::useShinyjs(),
  shiny::tags$head(
    shiny::tags$style(shiny::HTML(navbar_css))
  ),
  shiny::tags$script(HTML("
  Shiny.addCustomMessageHandler('collapse_data_preview', function(msg) {
    var btn = document.querySelector('.accordion-button');
    if (btn && btn.getAttribute('aria-expanded') === 'true') {
      btn.click();
    }
  });
   Shiny.addCustomMessageHandler('download_plot', function(msg) {
      // 创建临时链接并点击
      var link = document.createElement('a');
      link.href = msg.data;
      link.download = msg.filename;
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
    });
")),
  shiny::navbarPage(
    title = "Neutral Community Model (NCM) Tool",
    theme = bslib::bs_theme(bootswatch = "flatly", primary = "#2c3e50"),
    id = "main_nav",

    # ========== 页面1: Fit NCM ==========
    shiny::tabPanel(
      title = "Fit NCM",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = "Data Input",
          width = 400, # 在这里控制侧边栏宽度
          # 丰度数据（必须）
          shiny::fileInput("ncm_file", "Abundance Data (CSV/TSV) *",
            accept = c(".csv", ".tsv", ".txt")
          ),
          shiny::selectInput("sep", "Separator (abundance)",
            choices = c("Comma" = ",", "Tab" = "\t", "Semicolon" = ";")
          ),
          shiny::checkboxInput("header", "First row as column names (abundance)", value = TRUE),
          shiny::hr(),

          # 分组数据（可选，独立分隔符）
          shiny::fileInput("group_file", "Group Data (Optional, CSV/TSV)",
            accept = c(".csv", ".tsv", ".txt")
          ),
          shiny::selectInput("sep_group", "Separator (group)",
            choices = c("Comma" = ",", "Tab" = "\t", "Semicolon" = ";")
          ),
          shiny::checkboxInput("header_group", "First row as column names (group)", value = TRUE),
          shiny::helpText("Group file should contain sample IDs and group assignments."),
          shiny::hr(),

          # 示例数据按钮（同时加载丰度+分组）
          shiny::actionButton("example_data", "Load Example Data",
            icon = shiny::icon("table"), class = "btn-info"
          ),
          shiny::actionButton("clear_data", "Clear All Data",
            icon = shiny::icon("trash"),
            class = "btn-danger"
          ),
          shiny::hr(),

          # 折叠的参数设置面板
          bslib::accordion(
            open = FALSE,
            bslib::accordion_panel(
              title = "Model Parameters (optional)",
              icon = shiny::icon("sliders-h"),

              # 分组相关设置
              shiny::textInput("group_col", "Group column name in group file", value = "group"),
              shiny::selectizeInput("groups", "Select groups to analyze (leave empty for all)",
                choices = NULL, multiple = TRUE,
                options = list(placeholder = "All groups (including total)")
              )
            )
          ),
          # 运行按钮
          # 旋转动画 CSS（如果页面已有可省略，但为保证独立最好加上）
          tags$style(HTML("
    @keyframes spin {
      0% { transform: rotate(0deg); }
      100% { transform: rotate(360deg); }
    }
  ")),

          # ========== Fit NCM 按钮的 loading 遮罩 ==========
          tags$div(
            id = "run_ncm_loading_mask",
            style = "
      display: none;
      position: fixed;
      top: 0; left: 0;
      width: 100%; height: 100%;
      background: rgba(0,0,0,0.5);
      z-index: 99999;
      justify-content: center;
      align-items: center;
      flex-direction: column;
    ",
            tags$div(
              style = "
        width: 50px; height: 50px;
        border: 4px solid #f3f3f3;
        border-top: 4px solid #3498db;
        border-radius: 50%;
        animation: spin 1s linear infinite;
      "
            ),
            tags$p(
              "Fitting NCM model, please wait...",
              style = "color: white; margin-top: 15px; font-size: 16px;"
            )
          ),

          # 按钮：添加自定义 class "run-ncm-btn" 方便 JS 定位
          shiny::actionButton("run_ncm", "Fit NCM",
            class = "btn-primary run-ncm-btn",
            icon = icon("play")
          ),

          # JS：点击按钮显示遮罩，并监听 Shiny 消息隐藏遮罩
          tags$script(HTML("
    // 点击按钮时显示遮罩
    document.addEventListener('click', function(e) {
      if (e.target.closest('.run-ncm-btn')) {
        document.getElementById('run_ncm_loading_mask').style.display = 'flex';
      }
    });

    // Shiny 服务端通知隐藏遮罩
    Shiny.addCustomMessageHandler('hideRunNcmLoading', function(msg) {
      document.getElementById('run_ncm_loading_mask').style.display = 'none';
    });
  "))
        ),
        bslib::accordion(
          bslib::accordion_panel(
            title = "Data Preview",
            open = TRUE,
            shiny::uiOutput("data_tabs"),
            shiny::uiOutput("col_page_ui"),
            DT::DTOutput("data_preview")
          )
        ),
        bslib::card(
          bslib::card_header("NCM Fitting Results"),
          shiny::uiOutput("group_selector"),
          shiny::hr(),
          shiny::uiOutput("param_summary"),
          shiny::hr(),
          bslib::card(
            bslib::card_header("Status Summary"),
            shiny::tableOutput("status_summary_table")
          ),
          bslib::card(
            bslib::card_header("Predictions (species-level)"),
            DT::DTOutput("predictions_table")
          ),
          # 全屏 loading 遮罩（初始隐藏）
          tags$div(
            id = "ncm_loading_mask",
            style = "
    display: none;
    position: fixed;
    top: 0; left: 0;
    width: 100%; height: 100%;
    background: rgba(0,0,0,0.5);
    z-index: 99999;
    justify-content: center;
    align-items: center;
    flex-direction: column;
  ",
            tags$div(
              style = "
      width: 50px; height: 50px;
      border: 4px solid #f3f3f3;
      border-top: 4px solid #3498db;
      border-radius: 50%;
      animation: spin 1s linear infinite;
    "
            ),
            tags$p(
              "Preparing download...",
              style = "color: white; margin-top: 15px; font-size: 16px;"
            )
          ),

          # 旋转动画 CSS
          tags$style(HTML("
  @keyframes spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }
")),

          # 下载按钮（加上自定义 class）
          shiny::downloadButton("download_ncm", "Download Results", class = "ncm-dl-btn"),

          # 监听点击和接收隐藏消息的 JS
          tags$script(HTML("
  // 点击下载按钮时显示 loading
  document.addEventListener('click', function(e) {
    if (e.target.closest('.ncm-dl-btn')) {
      document.getElementById('ncm_loading_mask').style.display = 'flex';
    }
  });

  // Shiny 服务端通知隐藏 loading
  Shiny.addCustomMessageHandler('hideNcmLoading', function(msg) {
    document.getElementById('ncm_loading_mask').style.display = 'none';
  });
"))
        )
      )
    ),

    # ========== 页面2: Plotting ==========
    shiny::tabPanel(
      title = "Plotting",
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          title = "Plot Options",
          width = 400,

          # 数据源选择
          shiny::radioButtons("plot_data_source", "Data source",
            choices = c(
              "From NCM fitting result" = "from_ncm",
              "Upload data" = "upload_own"
            )
          ),

          # 上传数据时的控件
          shiny::conditionalPanel(
            condition = "input.plot_data_source == 'upload_own'",
            shiny::fileInput("plot_file", "Upload your NCM result file (CSV)",
              accept = c(".csv", ".tsv", ".txt")
            ),
            shiny::selectInput("plot_sep", "Separator",
              choices = c("Comma" = ",", "Tab" = "\t", "Semicolon" = ";")
            ),
            shiny::checkboxInput("plot_header", "First row as column names", value = TRUE),
            shiny::hr(),
            shiny::h5("Column mapping"),
            shiny::uiOutput("column_mapping_ui"),
            shiny::hr(),
            shiny::h5("Model parameters"),
            # 修改：支持小数点后5位
            shiny::numericInput("rsqr", "R²", value = NA, step = 0.00001),
            shiny::numericInput("Nm", "Nm", value = NA, step = 0.1),
            shiny::numericInput("m", "m", value = NA, step = 0.00001)
          ),
          shiny::hr(),

          # 可折叠的绘图样式控制面板
          bslib::accordion(
            open = FALSE,
            bslib::accordion_panel(
              title = "Plot Aesthetics (optional)",
              icon = shiny::icon("palette"),

              # ---- 点样式 ----
              shiny::h5("Point settings"),
              shiny::sliderInput("point_alpha", "Point transparency", min = 0, max = 1, value = 0.8, step = 0.05),
              shiny::sliderInput("point_size", "Point size", min = 1, max = 10, value = 3, step = 0.5),
              shiny::fluidRow(
                shiny::column(4, colourpicker::colourInput("point_color_above", "Above color", value = "#ED7D70")),
                shiny::column(4, colourpicker::colourInput("point_color_below", "Below color", value = "#2B889B")),
                shiny::column(4, colourpicker::colourInput("point_color_neutral", "Neutral color", value = "#B57FAF"))
              ),

              # ---- 拟合曲线样式 ----
              shiny::h5("Fit curve settings"),
              colourpicker::colourInput("fit_line_color", "Fit line color", value = "#335399"),
              shiny::selectInput("fit_line_type", "Fit line type",
                choices = c("solid" = "solid", "dashed" = "dashed", "dotted" = "dotted", "dotdash" = "dotdash"),
                selected = "solid"
              ),
              shiny::sliderInput("fit_line_size", "Fit line width", min = 0.5, max = 3, value = 1, step = 0.1),

              # ---- 置信区间曲线样式 ----
              shiny::h5("Confidence interval settings"),
              colourpicker::colourInput("ci_line_color", "CI line color", value = "#335399"),
              shiny::selectInput("ci_line_type", "CI line type",
                choices = c("solid" = "solid", "dashed" = "dashed", "dotted" = "dotted", "dotdash" = "dotdash"),
                selected = "dashed"
              ),
              shiny::sliderInput("ci_line_size", "CI line width", min = 0.5, max = 3, value = 1, step = 0.1),

              # ---- 坐标轴标题与文字 ----
              shiny::h5("Axis titles and text"),
              shiny::textInput("axis_title_x_text", "X-axis title", value = "Mean relative abundance (log10)"),
              shiny::textInput("axis_title_y_text", "Y-axis title", value = "Frequency of occupancy"),
              shiny::sliderInput("axis_title_x_size", "X-axis title size", min = 10, max = 40, value = 25, step = 1),
              shiny::sliderInput("axis_title_y_size", "Y-axis title size", min = 10, max = 40, value = 25, step = 1),
              shiny::sliderInput("axis_text_x_size", "X-axis text size", min = 8, max = 30, value = 20, step = 1),
              shiny::sliderInput("axis_text_y_size", "Y-axis text size", min = 8, max = 30, value = 20, step = 1),

              # ---- 图例样式 ----
              shiny::h5("Legend settings"),
              shiny::textInput("legend_title_text", "Legend title", value = NA),
              shiny::sliderInput("legend_size", "Legend key size", min = 2, max = 15, value = 6, step = 1),
              shiny::fluidRow(
                shiny::column(6, shiny::sliderInput("legend_position_x", "Legend position X", min = 0, max = 1, value = 0.8, step = 0.01)),
                shiny::column(6, shiny::sliderInput("legend_position_y", "Legend position Y", min = 0, max = 1, value = 0.4, step = 0.01))
              ),
              shiny::sliderInput("legend_hjust", "Legend horizontal adjustment", min = 0, max = 1, value = 0, step = 0.05),
              shiny::sliderInput("legend_vjust", "Legend vertical adjustment", min = 0, max = 1, value = 1, step = 0.05),

              # ---- 拟合参数标签样式 ----
              shiny::h5("Fit parameter label settings"),
              shiny::sliderInput("fit_para_size", "Parameter label size", min = 4, max = 15, value = 6, step = 1),
              shiny::fluidRow(
                shiny::column(6, shiny::sliderInput("fit_para_position_x", "Label position X", min = 0, max = 1, value = 0.02, step = 0.01)),
                shiny::column(6, shiny::sliderInput("fit_para_position_y", "Label position Y", min = 0, max = 1, value = 0.98, step = 0.01))
              ),
              shiny::sliderInput("fit_para_hjust", "Label horizontal adjustment", min = 0, max = 1, value = 0, step = 0.05),
              shiny::sliderInput("fit_para_vjust", "Label vertical adjustment", min = 0, max = 1, value = 1, step = 0.05),

              # ---- 字体 ----
              shiny::h5("Font family"),
              shiny::selectInput("font_family", "Font family",
                choices = c(
                  "sans" = "sans", "serif" = "serif", "mono" = "mono",
                  "Arial" = "Arial", "Times New Roman" = "Times New Roman"
                ),
                selected = "sans"
              )
            )
          ),
          shiny::hr(),
          shiny::actionButton("make_plot", "Generate Plot", class = "btn-success", icon = shiny::icon("chart-line"))
        ),
        bslib::accordion(
          id = "plot_data_accordion", # 用于控制折叠
          open = c("fitting_card", "upload_card"), # 默认都打开

          bslib::accordion_panel(
            title = "NCM Fitting Results",
            value = "fitting_card", # 用于标识
            icon = shiny::icon("table"),
            shiny::uiOutput("plot_group_selector"),
            shiny::hr(),
            shiny::uiOutput("plot_param_summary"),
            shiny::hr(),
            bslib::card(
              bslib::card_header("Status Summary"),
              shiny::tableOutput("plot_status_summary_table")
            ),
            bslib::card(
              bslib::card_header("Predictions (species-level)"),
              DT::DTOutput("plot_predictions_table")
            )
          ),
          bslib::accordion_panel(
            title = "Uploaded Data Preview",
            value = "upload_card", # 用于标识
            icon = shiny::icon("upload"),
            DT::DTOutput("uploaded_data_preview")
          )
        ),
        bslib::card(
          bslib::card_header("NCM Plot"),
          shiny::plotOutput("ncm_plot", height = "600px"),
          # Plot 下载 loading 遮罩（复用之前的样式，独立 ID 防止冲突）
          tags$div(
            id = "plot_loading_mask",
            style = "
    display: none;
    position: fixed;
    top: 0; left: 0;
    width: 100%; height: 100%;
    background: rgba(0,0,0,0.5);
    z-index: 99999;
    justify-content: center;
    align-items: center;
    flex-direction: column;
  ",
            tags$div(
              style = "
      width: 50px; height: 50px;
      border: 4px solid #f3f3f3;
      border-top: 4px solid #e74c3c;
      border-radius: 50%;
      animation: spin 1s linear infinite;
    "
            ),
            tags$p(
              "Preparing plot...",
              style = "color: white; margin-top: 15px; font-size: 16px;"
            )
          ),

          # 如果之前没有加过旋转动画 CSS，这里需要加（加过了就不用重复）
          tags$style(HTML("
  @keyframes spin {
    0% { transform: rotate(0deg); }
    100% { transform: rotate(360deg); }
  }
")),

          # 下载按钮加自定义 class
          shiny::actionButton("download_plot_btn", "Download Plot", icon = shiny::icon("download"), class = "plot-dl-btn"),

          # Plot 下载的 JS 监听
          tags$script(HTML("
  // 点击 Plot Modal 的 Download 按钮时显示 loading
  document.addEventListener('click', function(e) {
    var btn = e.target.closest('.modal-footer .btn-primary');
    if (!btn) return;
    var modalTitle = document.querySelector('.modal-title');
    if (modalTitle && modalTitle.textContent.trim() === 'Download Plot' && btn.textContent.trim() === 'Download') {
      document.getElementById('plot_loading_mask').style.display = 'flex';
    }
  });

  // 隐藏 loading（出错时调用）
  Shiny.addCustomMessageHandler('hidePlotLoading', function(msg) {
    document.getElementById('plot_loading_mask').style.display = 'none';
  });

  // URL 方式下载：点击链接后立即隐藏 loading（因为浏览器接管了）
  Shiny.addCustomMessageHandler('download_plot_url', function(msg) {
    var link = document.createElement('a');
    link.href = msg.url;
    link.download = msg.filename;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);

    // 触发下载后立刻隐藏 loading
    document.getElementById('plot_loading_mask').style.display = 'none';
  });
")),
          style = "height: 680px; max-height: 680px; overflow: hidden; flex-shrink: 0;"
        )
      )
    ),

    # ========== 页面3: Help ==========
    shiny::tabPanel(
      title = "Help",
      bslib::card(
        bslib::card_header("User Guide"),
        shiny::markdown('
## How to Use This Tool

### 1. Fit NCM (Neutral Community Model)

#### Data Input

**Abundance Data (Required)**
- Format: CSV or TSV file
- Structure: **Samples as rows, Species as columns**
- First column should be sample IDs (rownames), or the first column will be automatically detected as sample IDs if all values are unique
- The remaining columns contain species abundance/count data (numeric values only)
- Supported separators: Comma, Tab, or Semicolon

**Group Data (Optional)**
- Format: CSV or TSV file mapping sample IDs to group assignments
- Structure: **Samples as rows**
- Must contain a column with sample IDs (matching the abundance data) and a column with group labels
- Use separate separator settings if your group file uses a different delimiter than the abundance file

#### Quick Start
1. Click **Load Example Data** to test the tool with built-in demo data (includes both abundance and group information)
2. Or upload your own files using the file input buttons
3. Adjust model parameters in the collapsible **Model Parameters** panel if needed:
   - Specify the group column name if different from default "group"
   - Select specific groups to analyze (leave empty to analyze all groups plus total)
4. Click **Fit NCM** to run the model fitting
5. Results include:
   - Estimated parameters: **Nm** (metacommunity size), **m** (immigration rate), **N** (total individuals)
   - **R²** (coefficient of determination)
   - 95% confidence intervals for m
   - Species-level predictions with classification (Above/Below/Neutral expectations)

#### Data Preview Features
- Toggle between Abundance Data and Group Data tabs
- For large datasets (&gt;50 columns), use pagination buttons to navigate through species columns
- Data table supports horizontal and vertical scrolling

---

### 2. Plotting

#### Data Source Options

**Option A: Use Fitting Results**
- Automatically uses the NCM results from the "Fit NCM" tab
- Select from available groups if multiple groups were analyzed
- Displays fitted parameters and species-level predictions

**Option B: Upload Your Own Results**
- Upload a pre-computed NCM result file (CSV/TSV format)
- Required columns for plotting:
  - `p`: Mean relative abundance (x-axis)
  - `freq`: Observed occurrence frequency (y-axis)
  - `freq.pred`: Predicted occurrence frequency (fitted curve)
  - `lower`: Lower confidence interval bound
  - `upper`: Upper confidence interval bound
- Map your column names to these standard names using the dropdown menus
- Manually input model parameters (R², Nm, m) if not present in your file

#### Plot Customization

**Point Appearance**
- Transparency (alpha): 0-1
- Size: 1-10
- Colors: Customize for Above-prediction, Below-prediction, and Neutral species

**Fitted Curve**
- Line color, type (solid/dashed/dotted), and width

**Confidence Interval Bounds**
- Line color, type, and width for upper/lower CI curves

**Axis Labels**
- Custom X and Y axis titles
- Adjustable title and tick label sizes (8-40 points)

**Legend**
- Custom title
- Key size and position (X/Y coordinates from 0-1)
- Horizontal and vertical justification

**Parameter Label**
- Display position for R², Nm, m values on the plot
- Font size and alignment adjustments

**Font Family**
- Choose from sans, serif, mono, Arial, or Times New Roman

---

### 3. Data Format Examples

#### Abundance File (samples as rows, species as columns)

| SampleID | Species_A | Species_B | Species_C | Species_D |
|----------|-----------|-----------|-----------|-----------|
| Sample1  | 45        | 12        | 7         | 0         |
| Sample2  | 10        | 20        | 15        | 3         |
| Sample3  | 25        | 8         | 30        | 12        |
| Sample4  | 5         | 15        | 2         | 28        |

**Notes:**
- First column: Sample identifiers (will be used as rownames)
- Columns 2-N: Species abundance counts or relative abundances
- All values should be numeric (zeros allowed)

#### Group File (optional, samples as rows)

| SampleID | Group | Location |
|----------|-------|----------|
| Sample1  | A     | Site1    |
| Sample2  | A     | Site1    |
| Sample3  | B     | Site2    |
| Sample4  | B     | Site2    |

**Notes:**
- First column: Sample identifiers (must match abundance file)
- At least one column containing group assignments
- Additional metadata columns are allowed but ignored

---

### 4. Results Interpretation

#### Model Parameters
- **m**: Immigration rate (0-1). Higher values indicate stronger dispersal limitation.
- **N**: Total number of individuals in the local community.
- **Nm**: Metacommunity size (N × m). Represents the effective number of immigrants.
- **R²**: Goodness of fit. Values closer to 1 indicate better fit to neutral theory.
- **95% CI**: Confidence interval for the immigration rate estimate.

#### Species Classification
- **Above**: Species observed more frequently than predicted by neutral theory (selected/advantageous)
- **Below**: Species observed less frequently than predicted (disadvantageous or dispersal limited)
- **Neutral**: Species consistent with neutral theory predictions (within 95% CI)

#### Output Files (Download)
When you click **Download Results**, you receive a ZIP file containing:
- `summary.txt`: Model parameters, fit statistics, and status summary
- `predictions.csv`: Species-level predictions with classifications

For grouped analyses, each group has its own subfolder (e.g., `Group_A/`, `Group_B/`, `Group_total/`).

---


### 5. Citation

If you use this tool in your research, please cite:

> He Y (2026). ncmR: Fit Neutral Community Model to Microbiome or Ecological Data. R package, https://github.com/h-xuanjiu/ncmR

For the Neutral Community Model theory, please cite:

> Sloan, W.T., et al. (2006). Quantifying the roles of immigration and chance in shaping prokaryote community structure. *Environmental Microbiology*, 8(4), 732-740.

---

### 6. Contact & Support

For bug reports, feature requests, or questions about the NCM method, please contact heyuxuan0525@outlook.com or visit https://github.com/h-xuanjiu/ncmR.
        ')
      )
    )
  )
)
