####################################################################
## RoBDEMAT Visualizer
## Upload, edit, and visualize risk of bias assessments made with
## RoBDEMAT (pre-clinical / in vitro dental materials research).
##
## Reference: Delgado AHS et al. RoBDEMAT: A risk of bias tool and
## guideline to support reporting of pre-clinical dental materials
## research and assessment of systematic reviews. J Dent.
## 2022;127:104350. https://doi.org/10.1016/j.jdent.2022.104350
##
## This app does NOT perform the assessment -- it visualizes
## judgements the user has already made (uploaded, then editable
## in-app via dropdowns), in the style of robvis.
####################################################################

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(magrittr)
library(patchwork)
library(readxl)
library(openxlsx)
library(colourpicker)
library(rhandsontable)

# ------------------------------------------------------------------
# 1. RoBDEMAT item metadata + defaults
# ------------------------------------------------------------------

judgement_levels <- c(
  "Sufficiently reported / adequate",
  "Insufficiently reported",
  "Not reported / not adequate",
  "Not applicable"
)

# plotting-character (pch) codes for the symbol drawn inside each
# circle -- these are true ASCII plotting glyphs (drawn the same way
# base R draws pch 43/45/120), so they render dead-centre on the
# point, unlike geom_text() labels which are centred on font metrics
# and drift depending on the character. "Not applicable" gets no
# symbol (NA = nothing drawn).
judgement_pch <- c(
  "Sufficiently reported / adequate" = 43,   # "+"
  "Insufficiently reported"          = 45,   # "-"
  "Not reported / not adequate"      = 120,  # "x"
  "Not applicable"                   = NA
)

default_colors <- c(
  "Sufficiently reported / adequate" = "#2E7D32",
  "Insufficiently reported"          = "#F9A825",
  "Not reported / not adequate"      = "#C62828",
  "Not applicable"                   = "#9E9E9E"
)

# colour-blind friendly palette (Okabe-Ito derived: blue / orange /
# vermillion / grey), safe for the common red-green deficiencies
cb_colors <- c(
  "Sufficiently reported / adequate" = "#0072B2",
  "Insufficiently reported"          = "#E69F00",
  "Not reported / not adequate"      = "#D55E00",
  "Not applicable"                   = "#999999"
)

# short codes accepted when parsing an UPLOADED file (dropdown
# editing in-app restricts to the canonical text directly, so this
# is only needed for the initial CSV/Excel parse)
judgement_aliases <- c(
  "sufficiently reported / adequate" = "Sufficiently reported / adequate",
  "sufficiently reported"            = "Sufficiently reported / adequate",
  "adequate"                          = "Sufficiently reported / adequate",
  "s"                                 = "Sufficiently reported / adequate",
  "insufficiently reported"          = "Insufficiently reported",
  "i"                                 = "Insufficiently reported",
  "not reported / not adequate"     = "Not reported / not adequate",
  "not reported"                    = "Not reported / not adequate",
  "not adequate"                    = "Not reported / not adequate",
  "n"                                 = "Not reported / not adequate",
  "not applicable"                   = "Not applicable",
  "na"                                = "Not applicable",
  "n/a"                               = "Not applicable"
)

domains <- data.frame(
  domain = c("D1", "D2", "D3", "D4"),
  domain_title = c(
    "D1: Bias in planning and allocation",
    "D2: Bias in sample/specimen preparation",
    "D3: Bias in outcome assessment",
    "D4: Bias in data treatment and outcome reporting"
  ),
  stringsAsFactors = FALSE
)

items <- data.frame(
  id = c("1.1", "1.2", "1.3", "2.1", "2.2", "3.1", "3.2", "4.1", "4.2"),
  domain = c("D1", "D1", "D1", "D2", "D2", "D3", "D3", "D4", "D4"),
  label = c(
    "Control group",
    "Randomization of samples",
    "Sample size rationale and reporting",
    "Standardization of samples/materials",
    "Identical experimental conditions",
    "Testing procedures & outcomes",
    "Blinding of test operator",
    "Statistical analysis",
    "Reporting of study outcomes"
  ),
  stringsAsFactors = FALSE
)
item_ids <- items$id

TL_DEFAULTS <- list(width = 9, height = 6, dpi = 300)
ROW_UNIT_IN <- 2     # row-height inputs are relative units: 1 unit = 2 inches (0.2 -> 0.4 in)
ITEM_COL_IN <- 0.6   # fixed physical width (inches) of each item column in the traffic light table
SM_DEFAULTS <- list(width = 10, height = 5, dpi = 300)

# footnote lines: each domain title on its own line, followed by one
# indented line per item. Returned as a character vector (one
# element per line) so the caller can position each line explicitly.
build_footnote_lines <- function() {
  lines <- character(0)
  for (d in domains$domain) {
    dom_items <- items[items$domain == d, ]
    lines <- c(lines, domains$domain_title[domains$domain == d],
               paste0("   ", dom_items$id, " ", dom_items$label))
  }
  lines
}

# ------------------------------------------------------------------
# 2. Helpers
# ------------------------------------------------------------------

normalize_judgement <- function(x) {
  x_clean <- tolower(trimws(as.character(x)))
  out <- unname(judgement_aliases[x_clean])
  ifelse(is.na(out) & !is.na(x_clean) & x_clean != "", NA_character_, out)
}

read_uploaded <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls")) {
    df <- as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE)
    df[] <- lapply(df, as.character)
  } else {
    df <- read.csv(path, stringsAsFactors = FALSE, check.names = TRUE, colClasses = "character",
                   na.strings = "")   # keep the text "NA" (= Not applicable); only truly empty cells are missing
  }
  df
}

# One fixed example template: 8 studies with judgements written as the
# short codes S / I / N / NA (the same every time it is downloaded).
make_template <- function() {
  codes <- rbind(
    c("S", "S", "I", "S", "I", "S", "S", "I", "S"),
    c("I", "S", "S", "I", "N", "S", "S", "I", "NA"),
    c("I", "S", "NA", "S", "N", "S", "S", "S", "S"),
    c("N", "S", "I", "I", "S", "I", "S", "S", "S"),
    c("I", "I", "S", "I", "I", "S", "N", "I", "S"),
    c("I", "I", "N", "N", "S", "NA", "S", "I", "N"),
    c("S", "I", "S", "I", "N", "I", "N", "S", "I"),
    c("I", "I", "I", "N", "NA", "I", "I", "S", "N")
  )
  df <- data.frame(Study = paste("Study", seq_len(nrow(codes))), codes, stringsAsFactors = FALSE)
  colnames(df) <- c("Study", item_ids)
  df
}

# builds the editable table: free-text Study column, dropdown-only
# (select from list) for every RoBDEMAT item column
build_hot_table <- function(df) {
  ht <- rhandsontable(df, rowHeaders = NULL, contextMenu = TRUE, stretchH = "all") %>%
    hot_col("Study", type = "text", width = 220, className = "htMiddle")
  for (id in item_ids) {
    ht <- ht %>% hot_col(id, type = "dropdown", source = c("", judgement_levels),
                         strict = TRUE, allowInvalid = FALSE, width = 90, className = "htMiddle htCenter")
  }
  ht
}

# numericInput's min/max only warn in the browser -- typed values outside
# the range still reach the server -- so clamp them explicitly.
clamp_num <- function(x, lo, hi, default) {
  if (is.null(x) || length(x) == 0 || is.na(x)) x <- default
  max(lo, min(hi, x))
}

# Width (inches) of the widest string in `txt` at font size `fs` (pt),
# measured on a throwaway PDF device with metric-compatible fonts so
# right-aligned layout is accurate; falls back to a per-character
# estimate if measuring fails. The previously active device is restored.
measure_text_in <- function(txt, fs, family = "sans") {
  fallback <- max(nchar(txt)) * 0.52 * fs / 72
  old <- grDevices::dev.cur()
  out <- tryCatch({
    grDevices::pdf(NULL, family = switch(family, serif = "Times", mono = "Courier", "Helvetica"))
    graphics::par(ps = fs)
    max(graphics::strwidth(txt, units = "inches"))
  }, error = function(e) fallback)
  try(grDevices::dev.off(), silent = TRUE)
  if (old > 1) try(grDevices::dev.set(old), silent = TRUE)
  if (!is.finite(out) || out <= 0) fallback else out
}

# "Sources of bias" table for the About tab: the domain cell is merged
# (rowspan) across all of that domain's items.
bias_table <- function() {
  rows <- unlist(lapply(domains$domain, function(d) {
    it <- items[items$domain == d, ]
    lapply(seq_len(nrow(it)), function(k) {
      tags$tr(
        if (k == 1) tags$td(rowspan = nrow(it), style = "vertical-align: middle;",
                            domains$domain_title[domains$domain == d]),
        tags$td(it$id[k]),
        tags$td(it$label[k])
      )
    })
  }), recursive = FALSE)
  tags$table(
    class = "table table-bordered", style = "width: 100%;",
    tags$thead(tags$tr(tags$th("Domain"), tags$th("Item"), tags$th("Description"))),
    tags$tbody(rows)
  )
}

# Citation for the tool (cited by its web address until a paper is published;
# update url / year / version here and the Home tab text, .ris and .nbib all follow).
CITATION <- list(
  author_short = "Yuen JJX",
  author_full  = "Yuen, Jonathan Jun Xian",
  title = "RoBDEMAT Visualizer: a web tool for producing risk-of-bias assessment figures for pre-clinical dental materials research",
  year = "2026",
  version = "1.0",
  url = "https://jonathanyuen.shinyapps.io/RoBDEMATvis/"
)
citation_text <- function() {
  with(CITATION, paste0(author_short, ". ", title, " [computer software]. Version ", version, ". ",
                        year, ". ", url))
}
citation_ris <- function() {
  with(CITATION, c(
    "TY  - COMP",
    paste0("AU  - ", author_full),
    paste0("TI  - ", title),
    paste0("PY  - ", year),
    paste0("Y1  - ", year),
    paste0("ET  - ", version),
    paste0("UR  - ", url),
    "ER  - "
  ))
}
citation_nbib <- function() {
  with(CITATION, c(
    paste0("TI  - ", title),
    paste0("FAU - ", author_full),
    paste0("AU  - ", author_short),
    paste0("DP  - ", year),
    paste0("SO  - RoBDEMAT Visualizer (web application), version ", version, ". ", year, ". ", url),
    paste0("LA  - eng")
  ))
}

font_choices <- c("Sans (default)" = "sans", "Serif" = "serif", "Monospace" = "mono")
category_keys <- c("suff", "insuff", "notrep", "na")

# 4 colourInput/textInput pairs used to customize categories, plus
# "Default" and "Colour-blind friendly" quick-set buttons
# Bootstrap tooltip wrapper (styled, appears on hover; initialised once in the UI header)
tip <- function(text, ...) {
  tags$div(`data-toggle` = "tooltip", `data-placement` = "top", `data-container` = "body", title = text, ...)
}

category_controls <- function(prefix) {
  tagList(
    tags$style(HTML("
      .judg-row { display: flex; align-items: center; gap: 6px; margin-bottom: 6px; }
      .judg-row .form-group { margin-bottom: 0; }
      .judg-row .checkbox { margin: 0; min-height: 0; }
      .judg-row .judg-check { flex: 0 0 auto; }
      .judg-row .judg-colour { flex: 0 0 auto; width: 90px; }
      .judg-row .judg-text { flex: 1 1 auto; min-width: 0; }
    ")),
    tip("Default also restores the original legend text and re-ticks every judgement",
        selectInput(paste0(prefix, "_palette"), "Colour palette",
                    choices = c("Default" = "default", "Colour-blind friendly" = "cb"),
                    selected = "default", selectize = FALSE)
    ),
    helpText("Untick a judgement to hide it from the legend. You can change its colour using the colour picker (click the box) or by typing a hex code, and change the legend text by editing it."),
    lapply(seq_along(judgement_levels), function(i) {
      lvl <- judgement_levels[i]
      key <- category_keys[i]
      # one compact row per judgement: [show in legend] [colour] [legend text]
      tags$div(
        class = "judg-row",
        tags$div(class = "judg-check",
                 checkboxInput(paste0(prefix, "_show_", key), label = NULL, value = TRUE)),
        tags$div(class = "judg-colour",
                 colourInput(paste0(prefix, "_color_", key), label = NULL,
                             value = default_colors[[lvl]], showColour = "both", width = "100%")),
        tags$div(class = "judg-text",
                 textInput(paste0(prefix, "_label_", key), label = NULL, value = lvl, width = "100%"))
      )
    })
  )
}

get_colors <- function(input, prefix) {
  setNames(sapply(category_keys, function(k) input[[paste0(prefix, "_color_", k)]]), judgement_levels)
}

# which judgements are shown in the legend (named logical, TRUE if untouched)
get_show <- function(input, prefix) {
  setNames(vapply(category_keys, function(k) {
    v <- input[[paste0(prefix, "_show_", k)]]
    if (is.null(v)) TRUE else isTRUE(v)
  }, logical(1)), judgement_levels)
}

get_labels <- function(input, prefix) {
  setNames(sapply(category_keys, function(k) input[[paste0(prefix, "_label_", k)]]), judgement_levels)
}

export_controls <- function(prefix, defaults, auto_toggle = FALSE) {
  size_inputs <- tagList(
    numericInput(paste0(prefix, "_width"), "Width (in)", value = defaults$width, min = 2, max = 40),
    numericInput(paste0(prefix, "_height"), "Height (in)", value = defaults$height, min = 2, max = 60),
    actionButton(paste0(prefix, "_reset_export"), "Default size/DPI"),
    tags$br(), tags$br()
  )
  tagList(
    if (auto_toggle) {
      tagList(
        checkboxInput(paste0(prefix, "_manual"), "Adjust manually", value = FALSE),
        textOutput(paste0(prefix, "_export_size")),
        helpText("By default the export is sized automatically to fit the whole figure. Tick the box to set the width and height yourself."),
        conditionalPanel(paste0("input.", prefix, "_manual"), size_inputs)
      )
    } else size_inputs,
    numericInput(paste0(prefix, "_dpi"), "DPI", value = defaults$dpi, min = 72, max = 600),
    if (auto_toggle) actionButton(paste0(prefix, "_reset_dpi"), "Default DPI"),
    if (auto_toggle) tags$br(),
    if (auto_toggle) tags$br(),
    radioButtons(paste0(prefix, "_format"), "Format", choices = c("PNG" = "png", "TIFF" = "tiff"), inline = TRUE),
    downloadButton(paste0("download_", prefix), "Download plot")
  )
}

# ------------------------------------------------------------------
# 3. UI
# ------------------------------------------------------------------

ui <- navbarPage(
  title = "RoBDEMAT Visualizer",
  id = "nav",
  header = tags$script(HTML("$(function() { $('body').tooltip({selector: '[data-toggle=\"tooltip\"]', container: 'body'}); });")),
  
  # ---- Home ----
  tabPanel("Home", value = "home",
           fluidPage(
             tags$div(
               style = "max-width: 1150px; margin: 40px auto 30px auto;",
               tags$div(
                 style = "text-align: center;",
                 h1("RoBDEMAT Visualizer"),
                 p(style = "font-size: 18px; color: #555;",
                   "Enables the production of high-quality Risk of Bias Tool for Pre-clinical Dental Materials Research (RoBDEMAT) assessment figures."),
                 tags$br(),
                 actionButton("start_btn", "Start", class = "btn-primary btn-lg")
               ),
               tags$hr(style = "margin-top: 35px;"),
               
               fluidRow(
                 # ---- left: how to use ----
                 column(6,
                        h3("How to use the RoBDEMAT Visualizer"),
                        
                        h4("1. Download the Excel template"),
                        p("Click \u201cDownload Excel template\u201d and save the template to your computer."),
                        downloadButton("download_template_xlsx_home", "Download Excel template"),
                        
                        h4("2. Enter your assessment data"),
                        p("Open the Excel file and replace the example studies with your own study names and RoBDEMAT assessments."),
                        tags$ul(
                          tags$li("Enter one study per row."),
                          tags$li("Enter the appropriate judgement (S, I, N, or NA) for each RoBDEMAT item."),
                          tags$li("Keep the column headings and format unchanged.")
                        ),
                        
                        h4("3. Upload your completed Excel file"),
                        p("Save your completed file, then click \u201cStart\u201d and upload the Excel file."),
                        
                        h4("4. Generate your RoBDEMAT figure"),
                        p("The visualizer will use your data to create a publication-ready RoBDEMAT assessment figure.")
                 ),
                 # ---- right: example of the final figure ----
                 column(6,
                        h3("Example figure"),
                        if (file.exists(file.path("www", "example_figure.png"))) {
                          tags$img(src = "example_figure.png", alt = "Example RoBDEMAT traffic light figure",
                                   style = "width: 100%; height: auto; border: 1px solid #ddd; border-radius: 4px; padding: 6px; background: #fff;")
                        } else {
                          tags$div(
                            style = "border: 2px dashed #ccc; color: #888; padding: 60px 20px; text-align: center;",
                            "Example figure not found. Save an image as www/example_figure.png next to app.R."
                          )
                        }
                 )
               ),
               
               tags$hr(style = "margin-top: 30px;"),
               
               h3("Cite the RoBDEMAT Visualizer"),
               p("If the RoBDEMAT Visualizer was useful for your research, please consider citing the tool in your publication."),
               tags$div(
                 style = "background: #f5f5f5; border-left: 4px solid #ccc; padding: 10px 14px; margin-bottom: 12px;",
                 citation_text()
               ),
               downloadButton("download_cite_nbib", "Download citation (.nbib)"),
               downloadButton("download_cite_ris", "Download citation (.ris)")
             )
           )
  ),
  
  # ---- Upload & edit ----
  tabPanel("Upload & edit data", value = "upload",
           sidebarLayout(
             sidebarPanel(
               width = 4,
               h4("1. Get the template (optional)"),
               downloadButton("download_template_xlsx", "Template (Excel)"),
               tags$hr(),
               h4("2. Upload your judgements"),
               fileInput("upload", "Excel file", accept = c(".csv", ".xlsx", ".xls")),
               tags$hr(),
               h4("3. Edit in-app"),
               actionButton("add_row", "Add study"),
               tags$div(
                 class = "help-block",
                 p("Your uploaded studies will appear on the right. You can make changes directly in the table:"),
                 tags$ul(
                   style = "padding-left: 18px;",
                   tags$li(strong("Study names:"), " Click a study name to edit it."),
                   tags$li(strong("Judgements:"), " Click any assessment cell and choose from the four judgement options."),
                   tags$li(strong("Add a study:"), " Click \u201cAdd study\u201d to add a new row."),
                   tags$li(strong("Edit or remove rows:"), " Right-click a row for options to insert or remove it.")
                 )
               ),
               tags$hr(),
               uiOutput("validation_summary")
             ),
             mainPanel(
               width = 8,
               h4("Judgements (editable)"),
               uiOutput("data_status"),
               rHandsontableOutput("edit_table")
             )
           )
  ),
  
  # ---- Traffic light plot ----
  tabPanel("Traffic light plot",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               h4("Text"),
               selectInput("tl_font", "Font", choices = font_choices),
               numericInput("tl_font_size", "Font size (pt, 8-16)", value = 10, min = 8, max = 16, step = 1),
               tags$hr(),
               h4("Circles"),
               numericInput("tl_circle_size", "Circle size", value = 8, min = 2, max = 20, step = 0.5),
               numericInput("tl_row_height", "Study row height (0-1.5)", value = 0.2, min = 0, max = 1.5, step = 0.05),
               numericInput("tl_header_height", "Header row height (0-1.5)", value = 0.1, min = 0, max = 1.5, step = 0.05),
               tags$hr(),
               h4("Judgement colours & legend text"),
               category_controls("tl"),
               tags$hr(),
               h4("Export"),
               export_controls("tl", TL_DEFAULTS, auto_toggle = TRUE)
             ),
             mainPanel(
               width = 9,
               plotOutput("traffic_plot")
             )
           )
  ),
  
  # ---- Summary plot ----
  tabPanel("Summary plot",
           sidebarLayout(
             sidebarPanel(
               width = 3,
               h4("Text"),
               selectInput("sm_font", "Font", choices = font_choices),
               numericInput("sm_font_size", "Font size (pt, 8-16)", value = 10, min = 8, max = 16, step = 1),
               numericInput("sm_bar_text_size", "In-bar count label size (pt, 8-16)", value = 11, min = 8, max = 16, step = 0.5),
               tags$hr(),
               h4("Judgement colours & legend text"),
               category_controls("sm"),
               tags$hr(),
               h4("Export"),
               export_controls("sm", SM_DEFAULTS, auto_toggle = TRUE)
             ),
             mainPanel(
               width = 9,
               h4("Domain / item summary plot"),
               helpText("Proportion of studies in each judgement category, per item."),
               plotOutput("summary_plot")
             )
           )
  ),
  
  # ---- About ----
  tabPanel("About",
           fluidPage(
             h3("About this tool"),
             p("This app visualizes risk-of-bias judgements made using ", strong("RoBDEMAT"), ", a tool for pre-clinical (in vitro) dental materials research developed by Delgado et al. (2022)."),
             p("Reference: Delgado AHS, Sauro S, Lima AF, et al. RoBDEMAT: A risk of bias tool and guideline to support reporting of pre-clinical dental materials research and assessment of systematic reviews. ",
               em("J Dent."), " 2022;127:104350. ",
               a("https://doi.org/10.1016/j.jdent.2022.104350", href = "https://doi.org/10.1016/j.jdent.2022.104350", target = "_blank")),
             p("This tool was inspired by ",
               a("robvis", href = "https://github.com/mcguinlu/robvis", target = "_blank"),
               " (McGuinness & Higgins), an R package and web app for visualizing risk-of-bias assessments."),
             p("This tool was developed by Jonathan Jun Xian, Yuen (DDS)."),
             h4("Sources of bias"),
             bias_table()
           )
  )
)

# ------------------------------------------------------------------
# 4. Server
# ------------------------------------------------------------------

server <- function(input, output, session) {
  
  dataStore <- reactiveVal(NULL)
  # bumped whenever we want to force the editable table to re-render
  # from dataStore() (new upload, added row). Ordinary cell edits do
  # NOT bump this -- they only update dataStore() -- so the widget
  # isn't rebuilt (and the reactive graph doesn't loop) every time
  # someone edits a cell.
  render_trigger <- reactiveVal(0)
  
  # ---- Parse upload ----
  raw_data <- reactive({
    req(input$upload)
    df <- read_uploaded(input$upload$datapath, input$upload$name)
    req(ncol(df) >= 2)
    colnames(df)[1] <- "Study"
    df
  })
  
  processed <- reactive({
    df <- raw_data()
    avail <- colnames(df)[-1]
    matched <- sapply(item_ids, function(id) {
      candidates <- c(id, paste0("X", id), gsub("\\.", "_", id), paste0("Item_", id))
      hit <- avail[tolower(avail) %in% tolower(candidates)]
      if (length(hit) >= 1) hit[1] else NA_character_
    })
    found_ids <- item_ids[!is.na(matched)]
    missing_ids <- item_ids[is.na(matched)]
    
    out <- data.frame(Study = as.character(df$Study), stringsAsFactors = FALSE)
    unmatched_values <- character(0)
    for (id in found_ids) {
      col <- matched[[id]]
      vals <- df[[col]]
      norm <- normalize_judgement(vals)
      bad <- vals[is.na(norm) & !is.na(vals) & trimws(vals) != ""]
      if (length(bad) > 0) unmatched_values <- c(unmatched_values, unique(as.character(bad)))
      out[[id]] <- norm
    }
    for (id in missing_ids) out[[id]] <- NA_character_
    out <- out[, c("Study", item_ids), drop = FALSE]
    
    list(data = out, missing_cols = missing_ids, unmatched_values = unique(unmatched_values),
         n_studies = nrow(out))
  })
  
  observeEvent(input$upload, {
    dataStore(processed()$data)
    render_trigger(isolate(render_trigger()) + 1)
  })
  
  output$validation_summary <- renderUI({
    req(input$upload)
    p <- processed()
    msgs <- list()
    if (length(p$missing_cols) > 0) {
      msgs <- c(msgs, list(tags$p(style = "color:#C62828;",
                                  paste0("Columns not found for item(s): ", paste(p$missing_cols, collapse = ", "), "."))))
    }
    if (length(p$unmatched_values) > 0) {
      msgs <- c(msgs, list(tags$p(style = "color:#F9A825;",
                                  paste0("Unrecognized values left blank: ", paste(head(p$unmatched_values, 8), collapse = ", "),
                                         if (length(p$unmatched_values) > 8) ", ..." else ""))))
    }
    if (length(msgs) == 0) {
      tags$p(style = "color:#2E7D32;", paste0("Loaded ", p$n_studies, " studies, all items recognized."))
    } else {
      tagList(tags$p(strong(paste0("Loaded ", p$n_studies, " studies."))), msgs)
    }
  })
  
  # ---- Editable table (rhandsontable, dropdown-only judgement cells) ----
  output$edit_table <- renderRHandsontable({
    render_trigger()
    df <- isolate(dataStore())
    req(!is.null(df))
    build_hot_table(df)
  })
  
  observeEvent(input$edit_table, {
    df <- hot_to_r(input$edit_table)
    req(!is.null(df))
    # dropdown's blank option ("") represents "not yet judged" -> NA
    for (id in item_ids) {
      df[[id]][df[[id]] == ""] <- NA_character_
    }
    dataStore(df)
  }, ignoreInit = TRUE)
  
  observeEvent(input$add_row, {
    df <- dataStore()
    req(!is.null(df))
    new_row <- as.data.frame(t(rep(NA_character_, ncol(df))), stringsAsFactors = FALSE)
    colnames(new_row) <- colnames(df)
    new_row$Study <- paste0("New study ", nrow(df) + 1)
    dataStore(rbind(df, new_row))
    render_trigger(isolate(render_trigger()) + 1)
  })
  
  # ---- Reset / colour-blind buttons (traffic light plot) ----
  setup_color_buttons <- function(prefix) {
    observeEvent(input[[paste0(prefix, "_palette")]], {
      choice <- input[[paste0(prefix, "_palette")]]
      req(choice %in% c("default", "cb"))
      for (i in seq_along(judgement_levels)) {
        lvl <- judgement_levels[i]; k <- category_keys[i]
        updateColourInput(session, paste0(prefix, "_color_", k),
                          value = if (choice == "cb") cb_colors[[lvl]] else default_colors[[lvl]])
        if (choice == "default") {
          updateTextInput(session, paste0(prefix, "_label_", k), value = lvl)
          updateCheckboxInput(session, paste0(prefix, "_show_", k), value = TRUE)
        }
      }
    }, ignoreInit = TRUE)
  }
  setup_color_buttons("tl")
  setup_color_buttons("sm")
  
  observeEvent(input$tl_reset_export, {
    updateNumericInput(session, "tl_width", value = TL_DEFAULTS$width)
    updateNumericInput(session, "tl_height", value = TL_DEFAULTS$height)
    updateNumericInput(session, "tl_dpi", value = TL_DEFAULTS$dpi)
  })
  observeEvent(input$sm_reset_export, {
    updateNumericInput(session, "sm_width", value = SM_DEFAULTS$width)
    updateNumericInput(session, "sm_height", value = SM_DEFAULTS$height)
    updateNumericInput(session, "sm_dpi", value = SM_DEFAULTS$dpi)
  })
  
  # ---- Long-format data shared by both plots ----
  # ---- Completeness check: every study needs a name and all 9 judgements.
  # Plots are only generated when this passes.
  data_check <- reactive({
    df <- dataStore()
    if (is.null(df) || nrow(df) == 0) {
      return(list(ok = FALSE, empty = TRUE, details = character(0),
                  msg = "No data yet. Upload a file (or download the template) on the 'Upload & edit data' tab."))
    }
    blank_study <- is.na(df$Study) | trimws(as.character(df$Study)) == ""
    mm <- matrix(
      vapply(item_ids, function(id) is.na(df[[id]]) | trimws(as.character(df[[id]])) == "",
             logical(nrow(df))),
      nrow = nrow(df)
    )
    details <- character(0)
    if (any(blank_study)) {
      details <- c(details, paste0("Row ", paste(which(blank_study), collapse = ", "), ": study name is blank"))
    }
    for (i in which(rowSums(mm) > 0)) {
      who <- if (blank_study[i]) paste0("Row ", i) else as.character(df$Study[i])
      details <- c(details, paste0(who, ": ", paste(item_ids[mm[i, ]], collapse = ", ")))
    }
    n_blank <- sum(mm)
    ok <- length(details) == 0
    list(
      ok = ok, empty = FALSE, details = details, n_blank = n_blank,
      msg = if (ok) "" else paste0(
        "Some judgements are blank (", n_blank, " missing). ",
        "Please key in ALL judgements on the 'Upload & edit data' tab before the plots can be generated."
      )
    )
  })
  
  output$data_status <- renderUI({
    chk <- data_check()
    if (isTRUE(chk$empty)) return(NULL)
    if (chk$ok) {
      dup <- unique(dataStore()$Study[duplicated(dataStore()$Study)])
      return(tags$div(class = "alert alert-success", style = "padding: 8px 12px;",
                      paste0("All judgements are filled in (", nrow(dataStore()), " studies). The plots are ready."),
                      if (length(dup) > 0) tags$div(
                        paste0("Note: duplicate study names (", paste(dup, collapse = ", "),
                               ") are each shown as their own row."))))
    }
    shown <- head(chk$details, 10)
    tags$div(
      class = "alert alert-danger",
      tags$strong("Error: blank judgements found."),
      tags$p("Please key in all judgements using the dropdowns in the table below. ",
             "The plots will not be generated until every cell is filled."),
      tags$ul(lapply(shown, tags$li)),
      if (length(chk$details) > length(shown)) tags$p(paste0("...and ", length(chk$details) - length(shown), " more."))
    )
  })
  
  long_data <- reactive({
    chk <- data_check()
    validate(need(chk$ok, chk$msg))
    df <- dataStore()
    df %>%
      mutate(row_id = row_number()) %>%   # key rows by position, not by (possibly duplicated) study name
      pivot_longer(cols = all_of(item_ids), names_to = "id", values_to = "judgement") %>%
      left_join(items, by = "id") %>%
      mutate(
        judgement_display = ifelse(is.na(judgement), "Not reported / not adequate", judgement),
        judgement_display = factor(judgement_display, levels = judgement_levels)
      )
  })
  
  # ================================================================
  # Traffic light plot (custom table-style grid built from geoms)
  # ================================================================
  
  traffic_layout <- reactive({
    tl_fs <- clamp_num(input$tl_font_size, 8, 16, 10)
    tl_hh <- clamp_num(input$tl_header_height, 0, 1.5, 0.1)
    tl_rh <- clamp_num(input$tl_row_height, 0, 1.5, 0.2)
    df <- long_data()
    # one table row per row of the data, even if two studies share a name
    studies <- df %>% distinct(row_id, Study) %>% arrange(row_id) %>% pull(Study)
    n <- length(studies)
    
    # ---- Horizontal geometry, in INCHES (1 x-unit = 1 inch) ----
    # Item columns have a fixed physical width (ITEM_COL_IN). The Study
    # column takes whatever export width is left over (but never less
    # than the longest study name needs), so widening the exported image
    # widens only the Study column.
    item_w <- ITEM_COL_IN
    left_pad <- 0.3      # room for the rotated "Study" label
    right_pad <- 0.3
    min_study_w <- measure_text_in(studies, tl_fs, input$tl_font) + 0.3
    
    # Export width: automatic (smallest width that fits the table AND the
    # footer, measured from the actual text) unless "Adjust manually" is
    # ticked, in which case the typed width is used.
    manual <- isTRUE(input$tl_manual)
    foot_fs0 <- max(6, tl_fs - 2)
    fn_w <- measure_text_in(build_footnote_lines(), foot_fs0, input$tl_font)
    show_lv0 <- judgement_levels[get_show(input, "tl")]
    leg_lab_w <- if (length(show_lv0)) measure_text_in(unname(get_labels(input, "tl")[show_lv0]), foot_fs0, input$tl_font) else 0
    leg_title_w <- if (length(show_lv0)) measure_text_in("Judgements:", foot_fs0, input$tl_font) else 0
    leg_block_w <- if (length(show_lv0)) max(leg_title_w, leg_lab_w + 0.13 + 0.75 * 5 * .pt / 72 / 2) else 0
    table_min_panel <- left_pad + min_study_w + length(item_ids) * item_w + right_pad
    footer_min_panel <- left_pad + fn_w + 0.35 + leg_block_w + right_pad
    auto_width_in <- max(table_min_panel, footer_min_panel) + 0.42
    export_w_in <- if (manual) clamp_num(input$tl_width, 2, 40, 9) else auto_width_in
    panel_w_in <- max(3, export_w_in - 0.42)
    study_col_width <- max(min_study_w, panel_w_in - left_pad - right_pad - length(item_ids) * item_w)
    x_lo <- -left_pad
    table_width <- study_col_width + length(item_ids) * item_w
    x_hi <- table_width + right_pad
    
    x_study <- study_col_width / 2
    item_x <- setNames(study_col_width + (seq_along(item_ids) - 0.5) * item_w, item_ids)
    # y-coordinates are stored already flipped (negative = further
    # down the page), and plotted on a NORMAL (non-reversed) y-axis.
    # This avoids scale_y_reverse()'s expand-direction ambiguity,
    # which was the actual cause of the mismatched top/bottom padding
    # seen before: header row = 0, study rows = -1..-n going down,
    # footer content further negative still, domains title = +1 (above header).
    # Row geometry: header row centred at y = 0, study rows stacked
    # below it. Heights are user-adjustable so the table can be made
    # as compact as wanted.
    header_h <- tl_hh * ROW_UNIT_IN   # y-units are inches
    row_h <- tl_rh * ROW_UNIT_IN
    row_y <- c(0, -(header_h / 2 + (seq_len(n) - 0.5) * row_h))
    row_hgt <- c(header_h, rep(row_h, n))
    study_y <- setNames(row_y[-1], studies)
    
    df <- df %>% mutate(
      x = item_x[id],
      y = row_y[-1][row_id],
      symbol_pch = judgement_pch[as.character(judgement_display)]
    )
    symbol_df <- df %>% filter(!is.na(symbol_pch))
    
    grid_cells <- bind_rows(
      data.frame(x = x_study, y = row_y[-1], width = study_col_width, height = row_hgt[-1], fill = "#D9D9D9"),
      expand.grid(x = item_x, r = seq_along(row_y)) %>%
        mutate(y = row_y[r], height = row_hgt[r], width = item_w,
               fill = ifelse(r == 1, "#D9D9D9", "white")) %>%
        select(x, y, width, height, fill)
    )
    
    header_labels <- data.frame(x = item_x, y = 0, label = names(item_x))
    study_labels <- data.frame(x = 0.1, y = study_y, label = names(study_y))
    # spanning title over the domain columns, and "Study" rotated
    # outside the table to the left, matching a conventional table look
    # Domain row (D1-D4) sitting on top of the item-number header row;
    # each domain cell spans the item columns that belong to it.
    domain_h <- header_h
    domain_y <- header_h / 2 + domain_h / 2
    domain_cells <- do.call(rbind, lapply(domains$domain, function(d) {
      xs <- item_x[items$id[items$domain == d]]
      data.frame(x = mean(xs), y = domain_y, width = length(xs) * item_w, height = domain_h, label = d)
    }))
    domains_title <- data.frame(x = study_col_width + length(item_ids) * item_w / 2,
                                y = header_h / 2 + domain_h, label = "RoBDEMAT domains")
    study_side_label <- data.frame(x = 0, y = -(header_h / 2 + n * row_h / 2), label = "Study")
    
    colors <- get_colors(input, "tl")
    labels <- get_labels(input, "tl")
    
    y_hi <- header_h / 2 + domain_h
    y_lo <- -(header_h / 2 + n * row_h)
    if (y_hi - y_lo < 0.05) y_hi <- y_lo + 0.05
    main_in <- y_hi - y_lo                 # table height, inches
    top_pt <- tl_fs * 1.7                  # room above the table for the title
    bottom_pt <- 11                        # gap between table and footer
    
    # ---- Table plot (no legend / footnote of its own) ----
    main_plot <- ggplot() +
      geom_tile(data = grid_cells, aes(x = x, y = y, width = width, height = height, fill = fill),
                color = "grey60", linewidth = 0.3) +
      scale_fill_identity() +
      geom_tile(data = domain_cells, aes(x = x, y = y, width = width, height = height),
                fill = "#D9D9D9", color = "grey60", linewidth = 0.3) +
      geom_text(data = domain_cells, aes(x = x, y = y, label = label),
                fontface = "bold", family = input$tl_font, size = tl_fs / .pt) +
      geom_text(data = domains_title, aes(x = x, y = y, label = label),
                vjust = -0.35, fontface = "bold", family = input$tl_font, size = tl_fs / .pt) +
      geom_text(data = study_side_label, aes(x = x, y = y, label = label),
                angle = 90, vjust = -0.35, fontface = "bold", family = input$tl_font, size = tl_fs / .pt) +
      geom_text(data = header_labels, aes(x = x, y = y, label = label),
                fontface = "bold", family = input$tl_font, size = tl_fs / .pt) +
      geom_text(data = study_labels, aes(x = x, y = y, label = label),
                hjust = 0, family = input$tl_font, size = tl_fs / .pt) +
      geom_point(data = df, aes(x = x, y = y, color = judgement_display),
                 size = input$tl_circle_size) +
      geom_point(data = symbol_df, aes(x = x, y = y, shape = symbol_pch),
                 color = "black", size = input$tl_circle_size * 0.45) +
      scale_shape_identity() +
      scale_color_manual(values = colors, breaks = judgement_levels, drop = FALSE, guide = "none") +
      scale_y_continuous(breaks = NULL, limits = c(y_lo, y_hi), expand = expansion(0)) +
      coord_cartesian(clip = "off") +
      scale_x_continuous(breaks = NULL, limits = c(x_lo, x_hi), expand = expansion(0)) +
      labs(x = NULL, y = NULL) +
      theme_void(base_family = input$tl_font, base_size = tl_fs) +
      theme(plot.margin = margin(top_pt, 20, bottom_pt, 10))
    
    # ---- Footer: footnote on the left, legend as a column on the
    # right (like a conventional risk-of-bias figure). It is its own
    # small ggplot stacked under the table with patchwork, so its
    # size can never disturb the table's axis padding. The legend
    # keys are hand-drawn from the same circle + symbol layers as the
    # table (ggplot legend keys can't composite two layers).
    footnote_lines_text <- build_footnote_lines()
    n_foot_lines <- length(footnote_lines_text)
    foot_fs <- max(6, tl_fs - 2)
    line_h_in <- foot_fs * 1.5 / 72                        # one footnote line, inches
    leg_size <- 5                                          # legend circles: fixed size, independent of the table
    diam_in <- 0.75 * leg_size * .pt / 72                  # legend circle diameter, inches
    spacing_lines <- max(1.4, diam_in * 1.3 / line_h_in)   # legend row pitch, in lines
    show_lv <- judgement_levels[get_show(input, "tl")]   # judgements shown in the legend
    foot_lines <- max(n_foot_lines + 1, 1 + length(show_lv) * spacing_lines) + 0.6   # +1: "Domains:" header row
    footer_in <- foot_lines * line_h_in + 0.1
    
    # Table and footer share the SAME explicit x-range, so x = 0 is
    # the table's left border and x = table_width its right border in
    # both plots. Footnote text starts at x = 0; the legend is packed
    # so its right edge lands on x = table_width.
    units_per_in <- (x_hi - x_lo) / panel_w_in
    legend_labels <- unname(labels[show_lv])
    label_w_u <- if (length(show_lv)) measure_text_in(legend_labels, foot_fs, input$tl_font) * units_per_in else 0
    text_x <- table_width - label_w_u
    circle_x <- text_x - 0.13 * units_per_in
    
    # "Domains:" header on the same row as the legend's "Judgements:" title;
    # the footnote lines start one row below it.
    footnote_header <- data.frame(x = 0, y = -0.5, label = "Domains:")
    footnote_df <- data.frame(x = 0, y = -(seq_len(n_foot_lines) + 0.5), label = footnote_lines_text)
    legend_items <- data.frame(
      key = show_lv,
      color = unname(colors[show_lv]),
      pch = unname(judgement_pch[show_lv]),
      label = legend_labels,
      circle_x = rep(circle_x, length(show_lv)),
      text_x = rep(text_x, length(show_lv)),
      y = -(0.5 + spacing_lines * seq_along(show_lv)),
      stringsAsFactors = FALSE
    )
    legend_symbols <- legend_items %>% filter(!is.na(pch))
    legend_title <- data.frame(x = circle_x - (diam_in / 2) * units_per_in, y = -0.5, label = "Judgements:")[seq_len(length(show_lv) > 0), , drop = FALSE]
    
    footer_plot <- ggplot() +
      geom_text(data = footnote_header, aes(x = x, y = y, label = label),
                hjust = 0, fontface = "bold", family = input$tl_font, size = foot_fs / .pt) +
      geom_text(data = footnote_df, aes(x = x, y = y, label = label),
                hjust = 0, family = input$tl_font, size = foot_fs / .pt) +
      geom_text(data = legend_title, aes(x = x, y = y, label = label),
                hjust = 0, fontface = "bold", family = input$tl_font, size = foot_fs / .pt) +
      geom_point(data = legend_items, aes(x = circle_x, y = y, colour = I(color)),
                 size = leg_size) +
      geom_point(data = legend_symbols, aes(x = circle_x, y = y, shape = pch),
                 color = "black", size = leg_size * 0.45) +
      geom_text(data = legend_items, aes(x = text_x, y = y, label = label),
                hjust = 0, family = input$tl_font, size = foot_fs / .pt) +
      scale_shape_identity() +
      scale_x_continuous(limits = c(x_lo, x_hi), expand = expansion(0)) +
      scale_y_continuous(limits = c(-foot_lines, 0), expand = expansion(0)) +
      labs(x = NULL, y = NULL) +
      theme_void(base_family = input$tl_font, base_size = tl_fs) +
      theme(plot.margin = margin(0, 20, 4, 10))
    
    # Fixed physical heights (inches) for table and footer; a null-height
    # spacer underneath soaks up any extra export height, so rows never
    # stretch when the export height changes.
    final_plot <- (main_plot / footer_plot / patchwork::plot_spacer()) +
      patchwork::plot_layout(heights = grid::unit(c(main_in, footer_in, 1), c("in", "in", "null")))
    total_in <- main_in + (top_pt + bottom_pt) / 72 + footer_in + 4 / 72 + 0.05
    export_h_in <- if (manual) clamp_num(input$tl_height, 2, 60, 6) else total_in
    list(plot = final_plot, total_in = total_in, width_in = export_w_in, height_in = export_h_in)
  })
  
  traffic_plot_obj <- reactive({ traffic_layout()$plot })
  
  # Preview is rendered at the export size (96 dpi), so what you see
  # matches the exported proportions.
  output$traffic_plot <- renderPlot({
    traffic_plot_obj()
  },
  width = function() tryCatch(round(traffic_layout()$width_in * 96), error = function(e) 800),
  height = function() tryCatch(round(traffic_layout()$height_in * 96), error = function(e) 400),
  res = 96)
  
  output$tl_export_size <- renderText({
    l <- tryCatch(traffic_layout(), error = function(e) NULL)
    req(!is.null(l))
    sprintf("Export size: %.1f x %.1f in%s", l$width_in, l$height_in,
            if (isTRUE(input$tl_manual)) " (manual)" else " (auto)")
  })
  
  observeEvent(input$tl_reset_dpi, {
    updateNumericInput(session, "tl_dpi", value = TL_DEFAULTS$dpi)
  })
  
  output$download_tl <- downloadHandler(
    filename = function() paste0("robdemat_traffic_light.", input$tl_format),
    content = function(file) {
      chk <- data_check()
      if (!chk$ok) {
        showNotification(chk$msg, type = "error", duration = 8)
        req(chk$ok)
      }
      l <- traffic_layout()
      ggsave(file, plot = l$plot, width = l$width_in, height = l$height_in,
             dpi = input$tl_dpi, device = input$tl_format, bg = "white")
    }
  )
  
  # ================================================================
  # Summary plot
  # ================================================================
  
  summary_plot_obj <- reactive({
    sm_fs <- clamp_num(input$sm_font_size, 8, 16, 10)
    sm_bs <- clamp_num(input$sm_bar_text_size, 8, 16, 11)
    df <- long_data() %>%
      mutate(
        item_label = paste0(id, "\n", label),
        item_label = factor(item_label, levels = rev(unique(item_label[order(match(id, item_ids))])))
      )
    
    stack_order <- rev(judgement_levels)  # ggplot's default stack order, bottom to top pre-flip
    summary_df <- df %>%
      count(item_label, judgement_display) %>%
      group_by(item_label) %>%
      mutate(pct = n / sum(n) * 100) %>%
      ungroup() %>%
      mutate(judgement_display = factor(judgement_display, levels = judgement_levels)) %>%
      arrange(item_label, factor(judgement_display, levels = stack_order)) %>%
      group_by(item_label) %>%
      mutate(
        ymax = cumsum(pct),
        ymin = ymax - pct,
        y_mid = (ymin + ymax) / 2,
        bar_label = ifelse(n > 0, as.character(n), "")
      ) %>%
      ungroup()
    
    colors <- get_colors(input, "sm")
    labels <- get_labels(input, "sm")
    sm_show_lv <- judgement_levels[get_show(input, "sm")]   # judgements shown in the legend
    
    ggplot(summary_df, aes(x = item_label, y = pct, fill = judgement_display)) +
      geom_col(position = "stack", width = 0.7) +
      geom_text(aes(x = item_label, y = y_mid, label = bar_label), inherit.aes = FALSE,
                color = "black", size = sm_bs / .pt, family = input$sm_font) +
      scale_fill_manual(values = colors, breaks = sm_show_lv,
                        labels = labels[sm_show_lv], drop = FALSE, name = NULL) +
      labs(x = NULL, y = "% of studies", title = "RoBDEMAT summary plot") +
      theme_minimal(base_family = input$sm_font, base_size = sm_fs) +
      theme(
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = sm_fs),
        axis.title.x = element_text(margin = margin(t = 12)),   # gap between the axis numbers and "% of studies"
        legend.position = "bottom",
        legend.text = element_text(size = sm_fs),
        panel.grid.major.x = element_blank()
      ) +
      coord_flip()
  })
  
  # Export size for the summary plot: automatic unless "Adjust manually"
  # is ticked. Auto width is the default width (10 in) or wider if the
  # axis labels / legend need more room (measured from the actual text);
  # auto height fits the 9 bars plus title, axis and legend.
  summary_size <- reactive({
    sm_fs <- clamp_num(input$sm_font_size, 8, 16, 10)
    manual <- isTRUE(input$sm_manual)
    leg_labs <- unname(get_labels(input, "sm")[judgement_levels[get_show(input, "sm")]])
    leg_w <- sum(vapply(leg_labs, function(l) measure_text_in(l, sm_fs, input$sm_font), numeric(1))) +
      length(leg_labs) * 0.7
    axis_lines <- unlist(strsplit(paste0(items$id, "\n", items$label), "\n"))
    axis_w <- measure_text_in(axis_lines, sm_fs, input$sm_font) + 0.4
    auto_w <- max(SM_DEFAULTS$width, axis_w + 5, leg_w + 0.6)
    pitch <- max(0.5, 2 * sm_fs * 1.25 / 72 + 0.1)          # inches per bar (2-line labels)
    auto_h <- length(item_ids) * pitch + 1.4 + 2 * sm_fs / 72
    list(
      width_in = if (manual) clamp_num(input$sm_width, 2, 40, SM_DEFAULTS$width) else auto_w,
      height_in = if (manual) clamp_num(input$sm_height, 2, 60, SM_DEFAULTS$height) else auto_h,
      manual = manual
    )
  })
  
  # Preview rendered at the export size (96 dpi) so it matches the export.
  output$summary_plot <- renderPlot({
    summary_plot_obj()
  },
  width = function() tryCatch(round(summary_size()$width_in * 96), error = function(e) 800),
  height = function() tryCatch(round(summary_size()$height_in * 96), error = function(e) 500),
  res = 96)
  
  output$sm_export_size <- renderText({
    z <- summary_size()
    sprintf("Export size: %.1f x %.1f in%s", z$width_in, z$height_in,
            if (z$manual) " (manual)" else " (auto)")
  })
  
  observeEvent(input$sm_reset_dpi, {
    updateNumericInput(session, "sm_dpi", value = SM_DEFAULTS$dpi)
  })
  
  output$download_sm <- downloadHandler(
    filename = function() paste0("robdemat_summary_plot.", input$sm_format),
    content = function(file) {
      chk <- data_check()
      if (!chk$ok) {
        showNotification(chk$msg, type = "error", duration = 8)
        req(chk$ok)
      }
      z <- summary_size()
      ggsave(file, plot = summary_plot_obj(), width = z$width_in, height = z$height_in,
             dpi = input$sm_dpi, device = input$sm_format, bg = "white")
    }
  )
  
  # ---- Templates ----
  observeEvent(input$start_btn, {
    updateNavbarPage(session, "nav", selected = "upload")
  })
  
  template_download <- function() downloadHandler(
    filename = function() "robdemat_template.xlsx",
    content = function(file) write.xlsx(make_template(), file)
  )
  output$download_template_xlsx_home <- template_download()
  
  output$download_cite_ris <- downloadHandler(
    filename = function() "robdemat_visualizer_citation.ris",
    content = function(file) writeLines(citation_ris(), file, useBytes = TRUE)
  )
  output$download_cite_nbib <- downloadHandler(
    filename = function() "robdemat_visualizer_citation.nbib",
    content = function(file) writeLines(citation_nbib(), file, useBytes = TRUE)
  )
  output$download_template_xlsx <- template_download()
  
  # ---- About tab ----
  
}

shinyApp(ui, server)