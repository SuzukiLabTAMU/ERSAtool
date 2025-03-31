# DATE: 3/31/2025 ####
# INSTALL REQUIRED PACKAGES ####

install_if_missing <- function(cran_packages, bioconductor_packages = NULL) {
  for (pkg in cran_packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
  if (!is.null(bioconductor_packages)) {
    if (!requireNamespace("BiocManager", quietly = TRUE)) {
      install.packages("BiocManager")
    }
    for (pkg in bioconductor_packages) {
      if (!require(pkg, character.only = TRUE)) {
        BiocManager::install(pkg, ask = FALSE, update = TRUE)
        library(pkg, character.only = TRUE)
      }
    }
  }
}

# LOADING PACKAGES ####

required_cran_packages <- c("shiny", "shinyFiles", "DT", "data.table", "bs4Dash", "shinycssloaders", "rmarkdown", "ggplot2", "ggrepel", "pheatmap", "readxl", "writexl", "RColorBrewer", "dplyr", "kableExtra", "shinyAce")
required_bioc_packages <- c("GEOquery", "DESeq2", "ComplexHeatmap", "org.Hs.eg.db", "org.Mm.eg.db", "EnhancedVolcano", "clusterProfiler", "DOSE", "enrichplot", "AnnotationDbi")
install_if_missing(required_cran_packages, required_bioc_packages)

library(shiny)
library(shinyFiles)
library(DT)
library(data.table)
library(rmarkdown)
library(ggplot2)
library(tinytex)
library(ggrepel)
library(pheatmap)
library(readxl)
library(writexl)
library(RColorBrewer)
library(dplyr)
library(kableExtra)
library(shinyAce)
library(bs4Dash)
library(shinycssloaders)

library(GEOquery)
library(DESeq2)
library(ComplexHeatmap)
library(org.Hs.eg.db)
library(EnhancedVolcano)
library(clusterProfiler)
library(DOSE)
library(enrichplot)
library(AnnotationDbi)

# USER INTERFACE ####

ui <- dashboardPage(
  
  # HEADER ----
  
  dashboardHeader(title = tagList(
    tags$img(src = "Suzukilab_logo_cropped.png", height = "82px", style = "margin-right: 10px; image-rendering: crisp-edges; display: block;"), 
    "RNA Analysis Pipeline"
  )),
  
  # SIDEBAR ----
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Student Info", tabName = "student_info", icon = icon("user-graduate")),
      menuItem("Objective & Findings", tabName = "text_inputs", icon = icon("edit")),
      menuItem("Metadata", tabName = "metadata", icon = icon("database")),
      menuItem("Raw Counts", tabName = "raw_counts", icon = icon("table")),
      menuItem("Box Plots", tabName = "boxplots", icon = icon("chart-bar")),
      menuItem("PCA & Heatmap", tabName = "pca_heatmap", icon = icon("th")),
      menuItem("DEG Analysis", tabName = "deg", icon = icon("flask")),
      menuItem("GO Analysis", tabName = "go_analysis", icon = icon("dna")),
      menuItem("GSEA", tabName = "gsea", icon = icon("bar-chart")),
      menuItem("Download Report", tabName = "report", icon = icon("file-download"))
    )
  ),
  
  # BODY ----
  
  dashboardBody(
    
    tags$head(
      tags$style(HTML("
        #selected_column .radio label {
          font-weight: normal !important;
        }
      "))
    ),
    
    tabItems(
      
      # 📌 STUDENT INFO ----
      
      tabItem(tabName = "student_info",
              fluidRow(
                box(title = "Student Information", width = 12, solidHeader = TRUE, status = "info",
                    style = "border-radius: 10px; box-shadow: 2px 2px 10px rgba(0,0,0,0.1); padding: 15px;",
                    
                    tags$p("📌 This section is mandatory to generate the final report.",
                           style = "font-weight: bold; color: #d9534f; font-size: 16px; margin-bottom: 20px;"),
                    
                    textInput("first_name", "First Name:"),
                    textInput("last_name", "Last Name:"),
                    textInput("uin", "UIN:")
                )
              )
      ),
      
      # 📌 TEXT INPUTS ----
      
      tabItem(tabName = "text_inputs",
              fluidRow(
                box(title = "Study Information", width = 12, solidHeader = TRUE, status = "primary",
                    textInput("report_title", "Report Title", value = "Gene Expression Analysis"),
                    textAreaInput("objective_input", "Objective:", rows = 4),
                    textAreaInput("findings_input", "Findings from the Original Article:", rows = 4),
                    textAreaInput("abstract_input", "Abstract:", rows = 4)
                )
              )
      ),
      
      # 📌 METADATA ----
      
      tabItem(tabName = "metadata",
              fluidRow(
                
                box(title = "Select Species", width = 12, solidHeader = TRUE, status = "primary",
                    style = "border-radius: 10px; box-shadow: 2px 2px 10px rgba(0,0,0,0.1); padding: 15px;",
                    selectInput(
                      inputId = "species",
                      label = "Select Species:",
                      choices = c("Human" = "org.Hs.eg.db", "Mouse" = "org.Mm.eg.db"),
                      selected = "org.Hs.eg.db"
                    )
                ),
                
                box(title = "Important Instructions", width = 12, solidHeader = TRUE, status = "warning",
                    style = "border-radius: 10px; box-shadow: 2px 2px 10px rgba(0,0,0,0.1); padding: 15px;",
                    tags$p("⚠️ Please ensure that your metadata file follows these rules:", 
                           style = "font-weight: bold; color: #d9534f; font-size: 16px;"),
                    tags$ul(
                      tags$li("Your grouping or condition column should be named 'Condition'."),
                      tags$li("If you have multiple conditions, name them as 'Condition_1', 'Condition_2', etc."),
                      tags$li("Make sure column names do not contain special characters or spaces."),
                      tags$li("Use '.csv' or '.xlsx' file formats for best compatibility.")
                    )
                ),
                
                box(title = "Metadata Source", width = 12, solidHeader = TRUE, status = "primary",
                    style = "border-radius: 10px; box-shadow: 2px 2px 10px rgba(0,0,0,0.1); padding: 15px;",
                    
                    radioButtons("metadata_option", "Choose Metadata Source:",
                                 choices = c("Upload Metadata File" = "upload", "Enter GEO ID" = "geo"),selected = "upload"),
                    
                    conditionalPanel(
                      condition = "input.metadata_option == 'upload'",
                      fileInput("upload_metadata", "Upload Metadata File (.csv, .xlsx)", buttonLabel = "Browse...", placeholder = "No file selected")
                    ),
                    
                    conditionalPanel(
                      condition = "input.metadata_option == 'geo'",
                      textInput("geo_id", "Enter GEO ID:", placeholder = "e.g., GSE269016"),
                      actionButton("load_geo", "Load Metadata", class = "btn-primary")
                    ),
                    
                    tags$br(),
                    verbatimTextOutput("metadata_status")
                ),
                
                box(title = "Preview Metadata Table", width = 12, solidHeader = TRUE, status = "primary",
                    style = "border-radius: 10px; box-shadow: 2px 2px 10px rgba(0,0,0,0.1); padding: 15px;",
                    
                    div(style = "overflow-x: auto; max-width: 100%;",
                        DTOutput("metadata_table")
                    ),
                    
                    tags$br(),
                    downloadButton("download_metadata", "Download Metadata", class = "btn-primary btn-lg")
                ),
                
                box(title = "Modify Metadata Table", width = 12, solidHeader = TRUE, status = "primary",
                    style = "border-radius: 10px; box-shadow: 2px 2px 10px rgba(0,0,0,0.1); padding: 20px;",
                    
                    div(
                      h4("Row Operations", style = "color: #333; margin-bottom: 10px;"),
                      actionButton("add_row", "Add Row", icon = icon("plus"), class = "btn-success", style = "margin-right: 10px;"),
                      actionButton("delete_row", "Delete Selected Row", icon = icon("trash"), class = "btn-danger")
                    ),
                    tags$hr(),
                    
                    div(
                      h4("Column Operations", style = "color: #333; margin-bottom: 10px;"),
                      textInput("new_column_name", "New Column Name:", width = "60%"),
                      actionButton("add_column", "Add Column", icon = icon("plus"), class = "btn-success"),
                      tags$br(), tags$br(),
                      
                      selectizeInput("delete_column", "Select Column(s) to Delete:", choices = NULL, multiple = TRUE, width = "60%"),
                      actionButton("delete_column_btn", "Delete Column", icon = icon("trash"), class = "btn-danger")
                    ),
                    tags$hr(),
                    
                    div(
                      h4("Rename Column", style = "color: #333; margin-bottom: 10px;"),
                      selectInput("rename_column", "Select Column to Rename:", choices = NULL, width = "60%"),
                      textInput("new_column_name_rename", "Enter New Column Name:", width = "60%"),
                      actionButton("rename_column_btn", "Rename Column", icon = icon("edit"), class = "btn-warning")
                    )
                )
              )
      ),
      
      # 📌 RAW COUNTS ----
      
      tabItem(tabName = "raw_counts",
              fluidRow(
                box(title = "Upload Data", width = 12, solidHeader = TRUE, status = "success",
                    
                    radioButtons("file_type", "Select File Type:",
                                 choices = list("Raw Counts Matrix (.csv, .txt, .xlsx, .gz)" = "raw",
                                                "STAR .out.tab Files" = "out_tab"),
                                 selected = "raw", inline = FALSE),
                    
                    tags$hr(),
                    
                    conditionalPanel(
                      condition = "input.file_type == 'raw'",
                      fileInput("raw_counts", "Upload Raw Counts Matrix",
                                multiple = FALSE, accept = c(".csv", ".txt", ".xlsx", ".gz"))
                    ),

                    conditionalPanel(
                      condition = "input.file_type == 'out_tab'",
                      tagList(
                        div(
                          style = "font-weight: normal;",
                          radioButtons("selected_column", "Select Count Column:",
                            choices = list("Column V2 (Unstranded)" = "V2",
                                           "Column V3 (Stranded (first Strand))" = "V3",
                                           "Column V4 (Stranded (Second Strand))" = "V4"),
                            selected = "V4", inline = FALSE)
                        )
                      ),
                      fileInput("out_tab_files", "Upload Multiple .out.tab Files",
                                multiple = TRUE, accept = c(".out.tab", ".tab")),
                    ),
                    
                    tags$hr(),
                    
                    div(style = "overflow-x: auto; max-height: 500px;",
                        dataTableOutput("raw_preview") %>% withSpinner(color = "#28a745")
                    )
                )
              )
      ),
    
      # 📌 BOX PLOTS ----
      
      tabItem(tabName = "boxplots",
              fluidRow(
                
                box(title = "Pre-Normalized Box Plot", width = 6, solidHeader = TRUE, status = "info",
                    plotOutput("box_plot", height = "500px") %>% withSpinner(color = "#17a2b8"), 
                    downloadButton("download_boxplot", "Download Box Plot", class = "btn-info btn-sm"),
                    tags$hr(),
                    
                    actionButton("toggle_boxplot_code", "Show/Hide Boxplot Code", class = "btn-secondary btn-sm"),
                    conditionalPanel(
                      condition = "input.toggle_boxplot_code % 2 == 1",
                      pre(tags$code(uiOutput("boxplot_code")))
                    ),
                    
                    tags$br(),
                    textAreaInput("boxplot_text", "Interpretation:", rows = 3, width = "100%")
                ),
                
                box(title = "Normalized Box Plot", width = 6, solidHeader = TRUE, status = "primary",
                    plotOutput("box_plot_norm", height = "500px") %>% withSpinner(color = "#007bff"),  
                    downloadButton("download_boxplot_norm", "Download Normalized Box Plot", class = "btn-primary btn-sm"),
                    tags$hr(),
                    
                    actionButton("toggle_boxplot_code_norm", "Show/Hide Boxplot Code", class = "btn-secondary btn-sm"),
                    conditionalPanel(
                      condition = "input.toggle_boxplot_code_norm % 2 == 1",
                      pre(tags$code(uiOutput("boxplot_code_norm"))) 
                    ),
                    
                    tags$br(),
                    textAreaInput("boxplot_text_norm", "Interpretation:", rows = 3, width = "100%")
                )
              )
      ),
      
      # 📌 PCA & HEATMAP ----
      
      tabItem(tabName = "pca_heatmap",
              fluidRow(
                
                box(title = "PCA Plot", width = 6, solidHeader = TRUE, status = "warning",
                    plotOutput("pca_plot") %>% withSpinner(color = "#ffc107"),
                    downloadButton("download_pca_plot", "Download PCA Plot", class = "btn-warning btn-sm"),
                    tags$hr(),
                    
                    actionButton("toggle_pca_code", "Show/Hide PCA Code", class = "btn-secondary btn-sm"),
                    conditionalPanel(
                      condition = "input.toggle_pca_code % 2 == 1",
                      pre(tags$code(uiOutput("pca_code")))
                    ),
                    
                    tags$br(),
                    textAreaInput("pcaplot_text", "PCA Interpretation:", rows = 3, width = "100%")
                ),
                
                box(title = "Heatmap", width = 6, solidHeader = TRUE, status = "info",
                    plotOutput("heatmap_plot") %>% withSpinner(color = "#17a2b8"),
                    downloadButton("download_heatmap", "Download Heatmap", class = "btn-info btn-sm"),
                    tags$hr(),
                    
                    actionButton("toggle_heatmap_code", "Show/Hide Heatmap Code", class = "btn-secondary btn-sm"),
                    conditionalPanel(
                      condition = "input.toggle_heatmap_code % 2 == 1",
                      pre(tags$code(uiOutput("heatmap_code")))  
                    ),
                    
                    tags$br(),
                    textAreaInput("heatmapplot_text", "Heatmap Interpretation:", rows = 3, width = "100%")
                )
              )
      ),
      
      # 📌 DEG ANALYSIS ----
      
      tabItem(tabName = "deg",
              fluidRow(
                
                box(title = "Volcano Plot", width = 6, solidHeader = TRUE, status = "danger",
                    selectInput("condition_1", "Select Condition 1:", choices = NULL),
                    selectInput("condition_2", "Select Condition 2:", choices = NULL),
                    actionButton("generate_volcano", "Generate Volcano Plot", class = "btn-danger btn-sm"),
                    tags$hr(),
                    sliderInput("logfc_cutoff", "Log2 Fold Change Cutoff:", min = 0, max = 3, value = 1, step = 0.1),
                    sliderInput("adjp_cutoff", "Adjusted P-Value Cutoff:", min = 0, max = 0.2, value = 0.05, step = 0.01),
                    selectInput("comparison_selector", "Select Comparison:", choices = NULL, multiple = FALSE),
                    plotOutput("volcano_plot") %>% withSpinner(color = "#dc3545"),
                    downloadButton("download_volcano", "Download Volcano Plot", class = "btn-danger btn-sm"),
                    tags$hr(),
                    actionButton("toggle_volcano_code", "Show/Hide Volcano Code", class = "btn-secondary btn-sm"),
                    conditionalPanel(
                      condition = "input.toggle_volcano_code % 2 == 1",
                      pre(tags$code(uiOutput("volcano_code")))  
                    ),
                    div(id = "volcano_spinner", style = "display: none;",
                        tags$i(class = "fa fa-spinner fa-spin fa-3x fa-fw", style = "color: red;"),
                        tags$span("Generating Volcano Plot...", style = "color: red; font-size: 16px; font-weight: bold;")
                    ),
                    
                    textAreaInput("volcanoplot_text", "Volcano Plot Interpretation:", rows = 4, width = "100%")
                ),
                
                box(title = "DEG Table", width = 6, solidHeader = TRUE, status = "secondary",
                    div(style = "overflow-x: auto;",
                        dataTableOutput("deg_results") %>% withSpinner(color = "#6c757d")
                    ),
                    downloadButton("download_deg", "Download DEG Table", class = "btn-secondary btn-sm"),
                    tags$hr(),
                    actionButton("toggle_deg_code", "Show/Hide DEG Code", class = "btn-secondary btn-sm"),
                    conditionalPanel(
                      condition = "input.toggle_deg_code % 2 == 1",
                      pre(tags$code(uiOutput("deg_code")))  
                    )
                )
              )
      ),
      
      # 📌 GO ANALYSIS ----
      
      tabItem(tabName = "go_analysis",
              fluidRow(
                
                box(title = "Biological Process (BP)", width = 12, solidHeader = TRUE, status = "primary",
                    numericInput("go_term_count", label = "Number of TOP GO Terms to Display:", value = 20, min = 1, step = 1, width = "300px"),
                    actionButton("go_analysis", "Perform GO Analysis", class = "btn-primary btn-sm"),
                    plotOutput("bp_combined_plot", height = "1100px") %>% withSpinner(color = "#007bff"),
                    downloadButton("download_bp_combined_plot", "Download BP Plot", class = "btn-primary btn-sm"),
                    tags$hr(),
                    
                    actionButton("toggle_bp_code", "Show/Hide BP Code", class = "btn-secondary btn-sm"),
                    conditionalPanel(
                      condition = "input.toggle_bp_code % 2 == 1",
                      pre(tags$code(uiOutput("bp_code")))
                    ),
                    
                    tags$br(),
                    textAreaInput("bpplot_text", "Biological Process Interpretation:", rows = 3, width = "100%")
                )
              ),
              
              fluidRow(
                
                box(title = "Molecular Function (MF)", width = 12, solidHeader = TRUE, status = "secondary",
                    plotOutput("mf_combined_plot", height = "1100px") %>% withSpinner(color = "#6c757d"),
                    downloadButton("download_mf_combined_plot", "Download MF Plot", class = "btn-secondary btn-sm"),
                    tags$hr(),
                    
                    actionButton("toggle_mf_code", "Show/Hide MF Code", class = "btn-secondary btn-sm"),
                    conditionalPanel(
                      condition = "input.toggle_mf_code % 2 == 1",
                      pre(tags$code(uiOutput("mf_code")))  
                    ),
                    
                    tags$br(),
                    textAreaInput("mfplot_text", "Molecular Function Interpretation:", rows = 3, width = "100%")
                )
              )
      ),
      
      # 📌 GSEA ----
      
      tabItem(tabName = "gsea",
              fluidRow(
                
                box(title = "GSEA Results", width = 12, solidHeader = TRUE, status = "success",
                    actionButton("gsea_analysis", "Run GSEA", class = "btn-success btn-sm"),
                    div(style = "overflow-x: auto;", 
                        dataTableOutput("gsea_results") %>% withSpinner(color = "#28a745")
                    ),
                    tags$hr(),
                    
                    actionButton("toggle_gsea_code", "Show/Hide GSEA Code", class = "btn-secondary btn-sm"),
                    conditionalPanel(
                      condition = "input.toggle_gsea_code % 2 == 1",
                      pre(tags$code(uiOutput("gsea_code")))  
                    )
                ),
                
                box(title = "GSEA Dot Plot", width = 12, solidHeader = TRUE, status = "danger",
                    plotOutput("gsea_plot", height = "700px") %>% withSpinner(color = "#dc3545"),  
                    downloadButton("download_gsea_plot", "Download GSEA Plot", class = "btn-danger btn-sm"),
                    tags$hr(),
                    
                    actionButton("toggle_gsea_dot_code", "Show/Hide GSEA Dot Plot Code", class = "btn-secondary btn-sm"),
                    conditionalPanel(
                      condition = "input.toggle_gsea_dot_code % 2 == 1",
                      pre(tags$code(uiOutput("gsea_dot_code"))) 
                    ),
                    
                    tags$br(),
                    textAreaInput("gseaplot_text", "GSEA Dot Plot Interpretation:", rows = 3, width = "100%")
                )
              )
      ),
      
      # 📌 DOWNLOAD REPORT ----
      
      tabItem(tabName = "report",
              fluidRow(
                box(title = "Final Report", width = 12, solidHeader = TRUE, status = "primary",
                    textAreaInput("conclusion_input", "Conclusion:", rows = 4)
                )
              ),
              fluidRow(
                box(title = "Download Report", width = 12, solidHeader = TRUE, status = "info",
                    
                    downloadButton("save_static", "Generate Report")
                )
              )
      )
    )
  )
)

# SERVER LOGIC ####

server <- function(input, output, session) {
  
  ## CREATING A CONTAINER ####
  
  metadata <- reactiveVal(NULL)
  raw_counts <- reactiveVal(NULL)
  reactiveValues <- reactiveValues(deg_data = NULL)
  
  ## METADATA ####
  
  observeEvent(input$metadata_option, {
    metadata(NULL)
    output$metadata_status <- renderText("")
  })
  
  observeEvent(input$load_geo, {
    req(input$geo_id)
    output$metadata_status <- renderText("Loading GEO Metadata...")
    tryCatch({
      geo <- getGEO(input$geo_id, GSEMatrix = TRUE)
      metadata(pData(geo[[1]]))
      output$metadata_status <- renderText("GEO Metadata loaded successfully!")
    }, error = function(e) {
      metadata(NULL)
      output$metadata_status <- renderText(paste("Error loading GEO Metadata:", e$message))
    })
  })
  
  observeEvent(input$upload_metadata, {
    req(input$upload_metadata)
    file_path <- input$upload_metadata$datapath
    file_ext <- tools::file_ext(file_path)
    
    tryCatch({
      
      meta_data <- switch(
        file_ext,
        "xlsx" = as.data.frame(read_excel(file_path)),
        "csv" = read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE),
        stop("Unsupported file type. Please upload .xlsx or .csv")
      )
      
      if (nrow(meta_data) == 0 || ncol(meta_data) == 0) {
        stop("Metadata file is empty or incorrectly formatted.")
      }
      
      required_columns <- c("Condition")  
      missing_cols <- setdiff(required_columns, colnames(meta_data))
      
      if (length(missing_cols) > 0) {
        warning_msg <- paste("Warning: Missing required columns:", paste(missing_cols, collapse = ", "))
        showNotification(warning_msg, type = "warning")
      }
      
      if (!is.null(meta_data[[1]])) {
        rownames(meta_data) <- as.character(meta_data[[1]])
        meta_data <- meta_data[, -1, drop = FALSE]  
      }
      
      metadata(meta_data)
      output$metadata_status <- renderText("Metadata uploaded successfully!")
      
    }, error = function(e) {
      metadata(NULL)
      output$metadata_status <- renderText(paste("Error loading metadata:", e$message))
    })
  })
  
  output$metadata_table <- renderDT({
    req(metadata())
    datatable(metadata(), editable = TRUE, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
  observe({
    req(metadata())  
    meta_cols <- colnames(metadata())
    
    updateSelectInput(session, "delete_column", choices = meta_cols)
    updateSelectInput(session, "rename_column", choices = meta_cols)
    
    required_columns <- c("Condition")
    missing_cols <- setdiff(required_columns, meta_cols)
    
    if (length(missing_cols) > 0) {
      showNotification(paste("Warning: Metadata is missing columns:", paste(missing_cols, collapse = ", ")), type = "warning")
    }
  })
  
  observeEvent(input$add_column, {
    req(metadata(), input$new_column_name)
    meta <- metadata()
    new_col_name <- input$new_column_name
    
    if (!(new_col_name %in% colnames(meta))) {
      meta[[new_col_name]] <- ""  
      metadata(meta)
    } else {
      showNotification("Column already exists!", type = "error")
    }
  })
  
  observeEvent(input$delete_column_btn, {
    req(metadata(), input$delete_column)
    meta <- metadata()
    col_to_delete <- input$delete_column
    
    if(!is.vector(col_to_delete)){
      col_to_delete <- as.character(col_to_delete)
    }
    
    valid_col <- intersect(col_to_delete, colnames(meta))
    
    if(length(valid_col) > 0) {
      meta <- meta [, !colnames(meta) %in% valid_col, drop = FALSE]
      metadata(meta)
      
      updateSelectInput(session, "delete_column", choices = colnames(meta))
      updateSelectInput(session, "rename_column", choices = colnames(meta))
    } else {
      showNotification("Selected column(s) not found!", type = "error")
    }
  })
  
  observeEvent(input$rename_column_btn, {
    req(metadata(), input$rename_column, input$new_column_name_rename)
    meta <- metadata()
    old_col_name <- input$rename_column
    new_col_name <- input$new_column_name_rename
    
    if (old_col_name %in% colnames(meta) && !(new_col_name %in% colnames(meta))) {
      colnames(meta)[colnames(meta) == old_col_name] <- new_col_name
      metadata(meta)
    } else {
      showNotification("Invalid column name or column already exists!", type = "error")
    }
  })
  
  observeEvent(input$metadata_table_columns, {
    req(metadata())
    
    col_index <- input$metadata_table_columns$col
    new_col_name <- input$metadata_table_columns$value
    
    if (!is.null(col_index) && !is.null(new_col_name)) {
      meta <- metadata()
      colnames(meta)[col_index] <- new_col_name  
      metadata(meta)  
    }
  })
  
  observeEvent(input$add_row, {
    req(metadata())
    new_row <- setNames(as.list(rep("", ncol(metadata()))), names(metadata()))
    metadata(rbind(metadata(), new_row))
  })
  
  observeEvent(input$delete_row, {
    selected <- input$metadata_table_rows_selected
    if (!is.null(selected)) {
      metadata(metadata()[-selected, ])
    }
  })
  
  output$download_metadata <- downloadHandler(
    filename = function() { paste("Metadata_", Sys.Date(), ".xlsx", sep = "") },
    content = function(file) {
      meta <- metadata()
      meta_with_rownames <- cbind(RowName = rownames(meta), meta)
      write_xlsx(meta_with_rownames, file)
    }
  )
  
  ## RAW COUNTS ####
  
  observeEvent(input$file_type, {
    raw_counts(NULL)
    output$data_preview <- renderDataTable(NULL)
  })
  
  observeEvent(input$raw_counts, {
    req(input$file_type == "raw")
    req(input$raw_counts)
    
    file_path <- input$raw_counts$datapath
    file_ext <- tools::file_ext(file_path)
    
    tryCatch({
      raw_data <- switch(
        file_ext,
        "gz" = read.table(gzfile(file_path), header = TRUE, sep = "\t", stringsAsFactors = FALSE),
        "csv" = read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE),
        "txt" = read.table(file_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE),
        "xlsx" = as.data.frame(read_excel(file_path)),
        stop("Unsupported file type. Please upload .csv, .txt, .gz, or .xlsx.")
      )
      
      if (!is.null(raw_data[[1]])) {
        rownames(raw_data) <- raw_data[[1]]
        raw_data <- raw_data[, -1]
      } else {
        stop("First column of the raw counts file appears to be missing or empty.")
      }
      
      raw_counts(raw_data)
      
    }, error = function(e) {
      output$data_preview <- renderText(paste("Error processing raw count matrix:", e$message))
    })
  })
  
  observeEvent(input$out_tab_files, {
    req(input$file_type == "out_tab")
    req(input$out_tab_files)
    req(input$selected_column)
    
    counts_list <- list()
    
    for (i in 1:nrow(input$out_tab_files)) {
      file_path <- input$out_tab_files$datapath[i]
      sample_name <- gsub(".out.tab", "", basename(input$out_tab_files$name[i]))
      
      data <- fread(file_path, header = FALSE, sep = "\t", fill = TRUE, skip = 4)
      
      if (ncol(data) >= 2) {
        
        colnames(data)[1] <- "GeneID"
        
        selected_col <- input$selected_column
        if(!(selected_col %in% colnames(data))) stop ("Selected column not found!")
        
        gene_counts <- data[, .(GeneID, Count = get(selected_col))]
        clean_sample_name <- gsub("ReadsPerGene.*", "", sample_name)
        clean_sample_name <- sub("[._-]+$", "", clean_sample_name) 
        setnames(gene_counts, "Count", clean_sample_name)
        
        counts_list[[sample_name]] <- gene_counts
      } else {
        warning(paste("Skipping file:", input$out_tab_files$name[i], "- Not enough columns!"))
      }
    }
    
    if (length(counts_list) > 0) {
      if (length(counts_list) == 1) {
        counts_matrix <- counts_list[[1]]
      } else {
        counts_matrix <- Reduce(function(x, y) merge(x, y, by = "GeneID", all = TRUE, sort = FALSE), counts_list)
      }
      
      numeric_part <- counts_matrix[, lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = -1]
      counts_matrix <- cbind(GeneID = counts_matrix$GeneID, numeric_part)
      
      counts_df <- as.data.frame(counts_matrix)
      rownames(counts_df) <- counts_df$GeneID
      counts_df <- counts_df[, -1]
      raw_counts(counts_df)
    }
  })
  
  output$raw_preview <- renderDataTable({
    req(raw_counts())
    preview_data <- head(raw_counts(), 5)
    
    if (!"GeneID" %in% colnames(preview_data)) {
      preview_data <- cbind(GeneID = rownames(preview_data), preview_data)
    }
    
    datatable(preview_data, options = list(dom = 't', autowidth = TRUE), rownames = FALSE)
  })
  
  ## PRE NORMALIZED BOX & WHISKER PLOT ####
  
  output$box_plot <- renderPlot({
    req(raw_counts(), metadata())
    
    tryCatch({
      raw_data <- as.data.frame(raw_counts())
      meta_data <- as.data.frame(metadata())
      
      condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(meta_data), value = TRUE)
      
      if (length(condition_cols) == 0) {
        stop("Metadata is missing 'Condition' columns. Please check your file.")
      }
      
      meta_data$Combined_Condition <- apply(meta_data[, condition_cols, drop = FALSE], 1, paste, collapse = "_")
      
      if (!all(colnames(raw_data) %in% rownames(meta_data))) {
        stop("Column names of raw counts and row names of metadata do not match!")
      }
      
      df <- reshape2::melt(raw_data, variable.name = "Sample", value.name = "Raw_Counts")
      df$Log2_Transformed_Counts <- log2(df$Raw_Counts + 1)
      merged_df <- merge(df, meta_data, by.x = "Sample", by.y = "row.names", all.x = TRUE)
      merged_df$Condition <- merged_df$Combined_Condition
      
      unique_groups <- unique(merged_df$Condition)
      num_groups <- length(unique_groups)
      color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_groups)
      
      ggplot(merged_df, aes(x = Sample, y = Log2_Transformed_Counts, fill = Condition)) +
        geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
        scale_fill_manual(values = color_palette) +
        xlab("Samples") +
        ylab("log2(Raw_Counts + 1)") +  
        ggtitle("Log2 Transformed Counts Across Samples (Grouped by Combined Condition)") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom"
        )
      
    }, error = function(e) {
      showNotification(paste("Error generating boxplot:", e$message), type = "error")
    })
  })
  
    ### DOWNLOAD PRE-NORMALIZED BOX & WHISKER PLOT ####
  
  output$download_boxplot <- downloadHandler(
    filename = function() {
      paste("pre_normalized_boxplot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tryCatch({
        raw_data <- as.data.frame(raw_counts())
        meta_data <- as.data.frame(metadata())
        
        condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(meta_data), value = TRUE)
        
        if (length(condition_cols) == 0) {
          stop("Metadata is missing 'Condition' columns. Please check your file.")
        }
        
        meta_data$Combined_Condition <- apply(meta_data[, condition_cols, drop = FALSE], 1, paste, collapse = "_")
        
        if (!all(colnames(raw_data) %in% rownames(meta_data))) {
          stop("Column names of raw counts and row names of metadata do not match!")
        }
        
        df <- reshape2::melt(raw_data, variable.name = "Sample", value.name = "Raw_Counts")
        df$Log2_Transformed_Counts <- log2(df$Raw_Counts + 1)
        merged_df <- merge(df, meta_data, by.x = "Sample", by.y = "row.names", all.x = TRUE)
        merged_df$Condition <- merged_df$Combined_Condition
        
        unique_groups <- unique(merged_df$Condition)
        num_groups <- length(unique_groups)
        color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_groups)
        
        p <- ggplot(merged_df, aes(x = Sample, y = Log2_Transformed_Counts, fill = Condition)) +
          geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
          scale_fill_manual(values = color_palette) +
          xlab("Samples") +
          ylab("log2(Raw_Counts + 1)") +  
          ggtitle("Log2 Transformed Counts Across Samples (Grouped by Combined Condition)") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom"
          ) +
          theme(plot.background = element_rect(fill = "white", color = NA)) 
        
        ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
        
      }, error = function(e) {
        showNotification(paste("Error generating boxplot:", e$message), type = "error")
      })
    }
  )
  
    ### PRE-NORMALIZED BOX & WHISKER CODE ####
  
  observeEvent(input$toggle_boxplot_code, {
    output$boxplot_code <- renderUI({
      if (input$toggle_boxplot_code %% 2 == 1) {
        aceEditor(
          outputId = "boxplot_code",
          mode = "r",
          theme = "solarized_light",
          readOnly = TRUE,
          height = "250px",
          value = "## Box-and-Whisker Plot Logic (Raw Counts)

    # Convert raw data to long format for ggplot
    df <- reshape2::melt(raw_data, variable.name = 'Sample', value.name = 'Raw_Counts')

    # Merge metadata with counts
    merged_df <- merge(df, meta_data, by.x = 'Sample', by.y = 'row.names', all.x = TRUE)

    # Automatically detect groups and assign colors
    unique_groups <- unique(merged_df$Condition)
    num_groups <- length(unique_groups)
    color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, 'Dark2'))(num_groups)

    # Generate Boxplot
    ggplot(merged_df, aes(x = Sample, y = Raw_Counts, fill = Condition)) +
      geom_boxplot(outlier.color = 'red', outlier.shape = 16, outlier.size = 2) +
      scale_fill_manual(values = color_palette) +
      xlab('Samples') +
      ylab('Raw Counts') +
      ggtitle('Raw Counts Across Samples') +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'bottom'
      )"
        )
      }
    })
  })
  
  ## NORMALIZATION ####
  
  dds_data <- reactiveVal(NULL)
  
  observeEvent(list(raw_counts(), metadata()), {
    req(raw_counts(), metadata())
    
    tryCatch({
      meta_data <- metadata()
      
      condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(meta_data), value = TRUE)
      
      if (length(condition_cols) == 0) {
        showNotification("No condition columns found in metadata!", type = "error")
        stop("No condition columns detected in metadata.")
      }
      
      for (col in condition_cols) {
        meta_data[[col]] <- as.factor(meta_data[[col]])
      }
      
      design_formula <- as.formula("~ Combined_Condition")
      count_matrix <- as.matrix(raw_counts())
      
      if (any(is.na(count_matrix))) {
        stop("Raw counts contain NA values!")
      }
      
      dds <- DESeqDataSetFromMatrix(
        countData = round(count_matrix),
        colData = meta_data,
        design = design_formula
      )
      
      dds <- dds[rowSums(counts(dds)) > 10, ]
      dds <- DESeq(dds)  
      dds_data(dds)  
      
      showNotification("Normalization completed successfully!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error in DESeq2 processing:", e$message), type = "error")
    })
  })
  
  ## NORMALIZED BOX & WHISKER PLOT ####
  
  output$box_plot_norm <- renderPlot({
    req(dds_data(), metadata())
    
    tryCatch({
      dds <- dds_data()
      norm_counts <- counts(dds, normalized = TRUE)
      pseudoCount <- log2(norm_counts + 1)
      
      meta_data <- metadata()
      
      condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(meta_data), value = TRUE)
      
      if (length(condition_cols) == 0) {
        stop("Metadata is missing 'Condition' columns. Please check your file.")
      }
      
      meta_data$Combined_Condition <- apply(meta_data[, condition_cols, drop = FALSE], 1, paste, collapse = "_")
      
      df <- reshape2::melt(pseudoCount)
      merged_df <- merge(df, meta_data, by.x = "Var2", by.y = "row.names", all.x = TRUE)
      merged_df$Condition <- merged_df$Combined_Condition
      
      unique_groups <- unique(merged_df$Condition)
      num_groups <- length(unique_groups)
      color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_groups)
      
      ggplot(merged_df, aes(x = Var2, y = value, fill = Condition)) +
        geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
        scale_fill_manual(values = color_palette) +
        xlab("Samples") +
        ylab(expression(log[2](Normalized~Counts~+~1))) +
        ggtitle("Normalized Counts Across Samples (Grouped by Combined Condition)") +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom"
        )
      
    }, error = function(e) {
      showNotification(paste("Error generating boxplot:", e$message), type = "error")
    })
  })
  
    ### DOWNLOAD NORMALIZED BOX & WHISKER PLOT ####
  
  output$download_boxplot_norm <- downloadHandler(
    filename = function() {
      paste("normalized_boxplot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 1200, height = 800, res = 150)
      
      tryCatch({
        raw_data <- as.data.frame(raw_counts())
        meta_data <- as.data.frame(metadata())
        
        condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(meta_data), value = TRUE)
        
        if (length(condition_cols) == 0) {
          stop("Metadata is missing 'Condition' columns. Please check your file.")
        }
        
        meta_data$Combined_Condition <- apply(meta_data[, condition_cols, drop = FALSE], 1, paste, collapse = "_")
        if (!all(colnames(raw_data) %in% rownames(meta_data))) {
          stop("Column names of raw counts and row names of metadata do not match!")
        }
        
        dds <- dds_data()
        norm_counts <- counts(dds, normalized = TRUE)
        pseudoCount <- log2(norm_counts + 1)
        df <- reshape2::melt(pseudoCount)
        merged_df <- merge(df, meta_data, by.x = "Var2", by.y = "row.names", all.x = TRUE)
        merged_df$Condition <- merged_df$Combined_Condition
        
        unique_groups <- unique(merged_df$Condition)
        num_groups <- length(unique_groups)
        color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_groups)
        
        p <- ggplot(merged_df, aes(x = Var2, y = value, fill = Condition)) +
          geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
          scale_fill_manual(values = color_palette) +
          xlab("Samples") +
          ylab(expression(log[2](Normalized~Counts~+~1))) +
          ggtitle("Normalized Counts Across Samples (Grouped by Combined Condition)") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom"
          )
        
        
      }, error = function(e) {
        showNotification(paste("Error generating boxplot:", e$message), type = "error")
      })
      
      dev.off()
    }
  )
  
    ### NORMALIZED BOX & WHISKER CODE ####
  
  observeEvent(input$toggle_boxplot_code_norm, {
    output$boxplot_code_norm <- renderUI({
      if (input$toggle_boxplot_code_norm %% 2 == 1) {
        aceEditor(
          outputId = "boxplot_code_norm",
          mode = "r",
          theme = "solarized_light",
          readOnly = TRUE,
          height = "250px",
          value = "## Box-and-Whisker Plot Logic

  # Create DESeq2 dataset
  dds <- DESeqDataSetFromMatrix(
    countData = round(as.matrix(raw_data)),
    colData = meta_data,
    design = ~ Condition
  )

  # Filter low-count genes
  dds <- dds[rowSums(counts(dds)) > 10, ]

  # Normalize counts
  dds <- estimateSizeFactors(dds)
  norm_counts <- counts(dds, normalized = TRUE)
  pseudoCount <- log2(norm_counts + 1)

  # Convert data to long format for ggplot
  df <- reshape2::melt(pseudoCount)

  # Merge metadata for visualization
  merged_df <- merge(
    df,
    meta_data,
    by.x = 'Var2',
    by.y = 'row.names',
    all.x = TRUE
  )
 
  # Generate Boxplot
  ggplot(merged_df, aes(x = Var2, y = value, fill = Condition)) +
    geom_boxplot(outlier.color = 'red', outlier.shape = 16, outlier.size = 2) +
    xlab('Samples') +
    ylab(expression(log[2](Normalized~Counts~+~1))) +
    ggtitle('Normalized Counts Across Samples') +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = 'bottom'
    )"
        )
      }
    })
  })
  
  ## PCA PLOT ####
  
  output$pca_plot <- renderPlot({
    req(raw_counts(), metadata(), dds_data())
    
    tryCatch({
      meta_data <- metadata()
      raw_data <- raw_counts()
      
      condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(meta_data), value = TRUE)
      
      if (length(condition_cols) == 0) {
        stop("Metadata is missing 'Condition' columns. Please check your file.")
      }
      
      meta_data$Combined_Condition <- apply(meta_data[, condition_cols, drop = FALSE], 1, paste, collapse = "_")
      
      if (!all(colnames(raw_data) %in% rownames(meta_data))) {
        stop("Column names of raw counts and row names of metadata do not match!")
      }
      
      dds <- dds_data()
      rlog_data <- rlog(dds, blind = TRUE)
      
      pc <- prcomp(t(assay(rlog_data)))
      pc_data <- as.data.frame(pc$x[, 1:2])
      colnames(pc_data) <- c("PC1", "PC2")
      
      pc_data$Sample <- rownames(pc_data)
      merged_df <- merge(pc_data, meta_data, by.x = "Sample", by.y = "row.names", all.x = TRUE)
      
      merged_df$Condition <- merged_df$Combined_Condition
      
      unique_groups <- unique(merged_df$Condition)
      num_groups <- length(unique_groups)
      color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_groups)
      
      ggplot(merged_df, aes(x = PC1, y = PC2, color = Condition)) +
        geom_point(size = 5) +
        scale_color_manual(values = color_palette) +
        labs(title = "PCA Plot (Grouped by Combined Condition)", x = "PC1", y = "PC2") +
        theme_minimal()
      
    }, error = function(e) {
      showNotification(paste("Error in PCA Plot:", e$message), type = "error")
    })
  })
  
    ### DOWNLOAD PCA PLOT ####
  
  output$download_pca_plot <- downloadHandler(
    filename = function() {
      paste("PCA_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 1200, height = 800, res = 150)
      
      tryCatch({
        meta_data <- metadata()
        
        condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(meta_data), value = TRUE)
        
        if (length(condition_cols) == 0) {
          stop("Metadata is missing 'Condition' columns. Please check your file.")
        }
        
        meta_data$Combined_Condition <- apply(meta_data[, condition_cols, drop = FALSE], 1, paste, collapse = "_")
        
        raw_data <- raw_counts()
        if (!all(colnames(raw_data) %in% rownames(meta_data))) {
          stop("Column names of raw counts and row names of metadata do not match!")
        }
        
        dds <- dds_data()
        rlog_data <- rlog(dds, blind = TRUE)
        
        pc <- prcomp(t(assay(rlog_data)))
        pc_data <- as.data.frame(pc$x[, 1:2])
        colnames(pc_data) <- c("PC1", "PC2")
        
        pc_data$Condition <- meta_data$Combined_Condition
        unique_groups <- unique(pc_data$Condition)
        num_groups <- length(unique_groups)
        color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_groups)
        
        p <- ggplot(pc_data, aes(x = PC1, y = PC2, color = Condition)) +
          geom_point(size = 5) +
          scale_color_manual(values = color_palette) +
          labs(title = "PCA Plot (Grouped by Combined Condition)", x = "PC1", y = "PC2") +
          theme_minimal()
        
      }, error = function(e) {
        showNotification(paste("Error in PCA Plot:", e$message), type = "error")
      })
      
      dev.off()
    }
  )
  
    ### PCA CODE ####
  
  observeEvent(input$toggle_pca_code, {
    output$pca_code <- renderUI({
      if (input$toggle_pca_code %% 2 == 1) {
        aceEditor(
          outputId = "pca_code",
          mode = "r",
          theme = "solarized_light",
          readOnly = TRUE,
          height = "250px",
          value = "## PCA Plot Logic
  
  # Create a DESeq2 dataset as done in Normalized Box & Whisker Plot 
 
  # Normalize data
  dds <- estimateSizeFactors(dds)
  rlog_data <- rlog(dds, blind = TRUE)

  # Perform Principal Component Analysis (PCA)
  pc <- prcomp(t(assay(rlog_data)))
  pc_data <- as.data.frame(pc$x[, 1:2])
  colnames(pc_data) <- c('PC1', 'PC2')

  # Merge metadata for coloring
  pc_data$Condition <- metadata()$Condition

  # Generate PCA Plot
  ggplot(pc_data, aes(x = PC1, y = PC2, color = Condition)) +
    geom_point(size = 5) +
    labs(title = 'PCA Plot', x = 'PC1', y = 'PC2') +
    theme_minimal()"
        )
      }
    })
  })
  
  ## HEATMAP ####
  
  output$heatmap_plot <- renderPlot({
    req(raw_counts(), metadata(), dds_data())
    tryCatch({
      raw_counts_matrix <- round(as.matrix(raw_counts()))
      
      if (any(raw_counts_matrix < 0)) {
        stop("Raw counts contain negative values. Please check your input file.")
      }
      if (!all(raw_counts_matrix == floor(raw_counts_matrix))) {
        stop("Raw counts contain non-integer values. Please check your input file.")
      }
      
      if (!all(colnames(raw_counts_matrix) %in% rownames(metadata()))) {
        stop("Column names of raw counts and row names of metadata do not match!")
      }
      
      dds <- dds_data()
      rlog_data <- rlog(dds, blind = TRUE)
      
      dist_matrix <- dist(t(assay(rlog_data)))
      mat <- as.matrix(dist_matrix)
      rownames(mat) <- colnames(mat) <- colnames(raw_counts_matrix)
      
      ComplexHeatmap::Heatmap(
        mat,
        name = "Distance",
        col = colorRampPalette(rev(brewer.pal(9, "Reds")))(255),
        column_title = "Sample Distance Heatmap",
        cluster_rows = TRUE,
        cluster_columns = TRUE
      )
    }, error = function(e) {
      showNotification(paste("Error in Heatmap:", e$message), type = "error")
    })
  })
  
    ### DOWNLOAD HEATMAP PLOT ####
  
  output$download_heatmap <- downloadHandler(
    filename = function() {
      paste("heatmap_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 1200, height = 1000, res = 150)  
      
      tryCatch({
        req(raw_counts(), metadata(), dds_data())
        
        raw_counts_matrix <- round(as.matrix(raw_counts()))
        
        if (is.null(raw_counts_matrix) || nrow(raw_counts_matrix) == 0 || ncol(raw_counts_matrix) == 0) {
          stop("Raw counts matrix is empty or not provided.")
        }
        
        if (any(raw_counts_matrix < 0)) {
          stop("Raw counts contain negative values. Please check your input file.")
        }
        if (!all(raw_counts_matrix == floor(raw_counts_matrix))) {
          stop("Raw counts contain non-integer values. Please check your input file.")
        }
        
        meta_data <- as.data.frame(metadata())
        if (is.null(meta_data) || nrow(meta_data) == 0 || ncol(meta_data) == 0) {
          stop("Metadata is empty or not provided.")
        }
        if (!all(colnames(raw_counts_matrix) %in% rownames(meta_data))) {
          stop("Column names of raw counts do not match row names in metadata!")
        }
        
        dds <- dds_data()
        rlog_data <- rlog(dds, blind = TRUE)
        
        dist_matrix <- dist(t(assay(rlog_data)))
        mat <- as.matrix(dist_matrix)
        rownames(mat) <- colnames(mat) <- colnames(raw_counts_matrix)
        
        heatmap_obj <- tryCatch({
          ComplexHeatmap::Heatmap(
            mat,
            name = "Distance",
            col = colorRampPalette(rev(RColorBrewer::brewer.pal(9, "Reds")))(255),
            column_title = "Sample Distance Heatmap",
            cluster_rows = TRUE,
            cluster_columns = TRUE
          )
        }, error = function(e) {
          stop(paste("Error creating heatmap:", e$message))
        })
        
        if (is.null(heatmap_obj)) {
          stop("Heatmap generation failed. Check input data.")
        }
        
        grid.newpage()  
        draw(heatmap_obj)  
        
      }, error = function(e) {
        showNotification(paste("Error in Heatmap:", e$message), type = "error")
        
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 1.5, col = "red")
      })
      
      dev.off()  
    }
  )
  
    ### HEATMAP CODE ####
  
  observeEvent(input$toggle_heatmap_code, {
    output$heatmap_code <- renderUI({
      if (input$toggle_heatmap_code %% 2 == 1) {
        aceEditor(
          outputId = "heatmap_code",
          mode = "r",
          theme = "solarized_light",
          readOnly = TRUE,
          height = "250px",
          value = "## Heatmap Plot Logic

  # Create a DESeq2 dataset and Normalize it as done in Normalized Box & Whisker Plot 

  # Perform rlog transformation for visualization
  rlog_data <- rlog(dds, blind = TRUE)

  # Compute sample distance matrix
  dist_matrix <- dist(t(assay(rlog_data)))
  mat <- as.matrix(dist_matrix)
  rownames(mat) <- colnames(mat) <- colnames(raw_counts_matrix)

  # Generate Heatmap
  ComplexHeatmap::Heatmap(
    mat,
    name = 'Distance',
    col = colorRampPalette(rev(brewer.pal(9, 'Reds')))(255),
    column_title = 'Sample Distance Heatmap',
    cluster_rows = TRUE,
    cluster_columns = TRUE
  )"
        )
      }
    })
  })
  
  ## ENHANCED VOLCANO PLOT ####
  
  observe({
    req(metadata())
    
    condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(metadata()), value = TRUE)
    
    if (length(condition_cols) == 0) {
      showNotification("No valid 'Condition' columns found!", type = "error")
      return()
    }
    
    meta_data <- metadata()
    meta_data$Combined_Condition <- apply(meta_data[, condition_cols, drop = FALSE], 1, paste, collapse = "_")
    
    metadata(meta_data)
    
    conditions <- unique(meta_data$Combined_Condition)
    
    if (length(conditions) >= 2) {
      updateSelectInput(session, "condition_1", choices = conditions, selected = conditions[1])
      updateSelectInput(session, "condition_2", choices = conditions, selected = conditions[2])
    }
  })
  
  reactiveVolcanoData <- reactiveValues(plots = list(), selected_genes = NULL)
  
  observeEvent(input$generate_volcano, {  
    req(dds_data(), metadata(), input$species)
    
    showNotification("Running Enhanced Volcano Plot... Please wait.", type = "message", duration = NULL, id = "EV_msg")
    
    if (is.null(input$condition_1) || is.null(input$condition_2) || input$condition_1 == input$condition_2) {
      showNotification("Please select two different conditions.", type = "error")
      return()
    }
    
    dds <- dds_data()
    design(dds) <- as.formula("~ Combined_Condition")  
    dds <- DESeq(dds)  
    
    comparisons <- list(c(input$condition_1, input$condition_2))
    volcano_plots <- list()
    significant_genes <- list()
    
    for (pair in comparisons) {
      res <- tryCatch({
        as.data.frame(results(dds, contrast = c("Combined_Condition", pair[1], pair[2]), alpha = 0.05)) %>%
          na.omit()
      }, error = function(e) { 
        showNotification(paste("Error in DESeq results:", e$message), type = "error")
        NULL 
      })
      
      # Only clean if rownames look like ENSEMBL IDs (e.g., ENSMUSG00000012345.1)
      if (any(grepl("^ENS.*\\..+", rownames(res)))) {
        clean_ids <- gsub("\\..*", "", rownames(res))
        rownames(res) <- make.unique(clean_ids)  # Ensure uniqueness
      } 
      
      selected_db <- get(input$species, envir = .GlobalEnv)
      
      is_ensg <- if (input$species == "org.Mm.eg.db") {
        any(grepl("^ENSMUSG", rownames(res)))  
      } else {
        any(grepl("^ENSG", rownames(res)))  
      }
      
      if (is_ensg) {
        res$Symbol <- mapIds(
          selected_db,
          keys = rownames(res),
          column = "SYMBOL",
          keytype = "ENSEMBL",
          multiVals = "first"
        )
      } else {
        res$Symbol <- rownames(res)
      }
      
      res$Symbol[is.na(res$Symbol)] <- rownames(res)
      res$Symbol <- make.unique(res$Symbol, sep = "_dup")
      res_df <- as.data.frame(res)
      
      # Function to remove duplicate gene entries, keeping the most significant one
      remove_duplicates <- function(df) {
        if ("Symbol" %in% colnames(df)) {
          df <- df[order(df$padj, decreasing = FALSE), ]  # Sort by smallest padj (most significant)
          df <- df[!duplicated(df$Symbol), ]  # Remove duplicates
        }
        return(df)
      }
      
      # Apply function to remove duplicate gene symbols before plotting
      res_df <- remove_duplicates(as.data.frame(res))
      
      significant <- res_df %>% filter(padj < input$adjp_cutoff, abs(log2FoldChange) > input$logfc_cutoff)
      significant_genes[[paste(pair[1], "vs", pair[2])]] <- significant
      
      volcano_plots[[paste(pair[1], "vs", pair[2])]] <- EnhancedVolcano(
        res_df,
        lab = res_df$Symbol,
        x = 'log2FoldChange',
        y = 'padj',
        pCutoff = input$adjp_cutoff,  
        FCcutoff = input$logfc_cutoff,  
        title = paste(pair[1], "vs", pair[2]),
        legendPosition = "right"
      )
    }
    
    reactiveVolcanoData$plots <- volcano_plots
    reactiveVolcanoData$selected_genes <- significant_genes
    updateSelectInput(session, "comparison_selector", choices = names(volcano_plots), selected = names(volcano_plots)[1])
    
    removeNotification("EV_msg")
  })
  
  output$volcano_plot <- renderPlot({
    req(reactiveVolcanoData$plots, input$comparison_selector)
    
    if (length(reactiveVolcanoData$plots) == 0) {
      plot.new()
    } else {
      reactiveVolcanoData$plots[[input$comparison_selector]]
    }
  })
  
    ### DOWNLOAD ENHANCED VOLCANO PLOT ####
  
  output$download_volcano <- downloadHandler(
    filename = function() {
      paste("volcano_plot_", input$comparison_selector, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tryCatch({
        req(reactiveVolcanoData$plots, input$comparison_selector)
        selected_plot <- reactiveVolcanoData$plots[[input$comparison_selector]]
        
        if (is.null(selected_plot)) {
          showNotification("No volcano plot available for the selected comparison.", type = "error")
          stop("No volcano plot available.")
        }
        
        ggsave(file, plot = selected_plot, width = 12, height = 8, dpi = 300, bg = "white")
        
      }, error = function(e) {
        showNotification(paste("Error generating volcano plot:", e$message), type = "error")
      })
    }
  )
    ### ENHANCED VOLCANO CODE ####
  
  observeEvent(input$toggle_volcano_code, {
    output$volcano_code <- renderUI({
      if (input$toggle_volcano_code %% 2 == 1) {
        aceEditor(
          outputId = "volcano_code",
          mode = "r",
          theme = "solarized_light",
          readOnly = TRUE,
          height = "250px",
          value = "## Enhanced Volcano Plot Logic
          
  # Convert DEG results to data frame

  # Generate Enhanced Volcano Plot
  EnhancedVolcano(
    res,
    lab = res$SYMBOL,              
    x = 'log2FoldChange',          
    y = 'padj',                    
    pCutoff = input$adjp_cutoff,                
    FCcutoff = input$logfc_cutoff,
    title = 'Liver [Exercise (vs) Sedentary]',  
    subtitle = 'Differential expression',        
    caption = paste('FC cutoff:', input$logfc_cutoff, '; adj p-value cutoff:', input$adjp_cutoff),
    legendPosition = 'right',    
    legendLabSize = 14,          
    col = c('grey30', 'forestgreen', 'royalblue', 'red2')
  )"
        )
      }
    })
  })
  
  ## DEG ANALYSIS ####
  
  output$deg_results <- renderDataTable({
    req(reactiveVolcanoData$selected_genes, input$comparison_selector)
    
    selected_genes <- reactiveVolcanoData$selected_genes[[input$comparison_selector]]
    if (is.null(selected_genes) || nrow(selected_genes) == 0) {
      return(NULL)
    }
    
    datatable(selected_genes, options = list(pageLength = 10, autoWidth = TRUE))
  })
  
    ### DOWNLOAD DEG DATA (Save Results) ####
  
  output$download_deg <- downloadHandler(
    filename = function() {
      paste("DEG_Results_", input$comparison_selector, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(reactiveVolcanoData$selected_genes, input$comparison_selector)
      selected_deg <- reactiveVolcanoData$selected_genes[[input$comparison_selector]]
      
      if (is.null(selected_deg) || nrow(selected_deg) == 0) {
        showNotification("No significant genes to download.", type = "warning")
        return(NULL)
      }
      
      write.csv(selected_deg, file, row.names = FALSE)
    }
  )
  
    ### DEG CODE ####
  
  observeEvent(input$toggle_deg_code, {
    output$deg_code <- renderUI({
      if (input$toggle_deg_code %% 2 == 1) {
        aceEditor(
          outputId = "deg_code",
          mode = "r",
          theme = "solarized_light",
          readOnly = TRUE,
          height = "400px",
          value = "## Differential Expression Analysis
 
    # Create a DESeq2 dataset and Normalize it as done in Normalized Box & Whisker Plot 

    original_ids <- rownames(res)
 
    # Initialize Symbol column  
    res$Symbol <- NA
 
    # Determine if row names are ENSG IDs
    is_ensg <- all(grepl('^ENSG', original_ids))
 
    if (is_ensg) {
      # Use BiomaRt for gene symbol conversion
      mart <- useMart('ensembl', dataset = 'hsapiens_gene_ensembl')
      annotations <- getBM(
        attributes = c('ensembl_gene_id', 'hgnc_symbol'),
        filters = 'ensembl_gene_id',
        values = original_ids,
        mart = mart
      )
   
      # Merge annotations
      res$ENSEMBL <- original_ids
      res <- merge(res, annotations, by.x = 'ENSEMBL', by.y = 'ensembl_gene_id', all.x = TRUE)
      colnames(res)[colnames(res) == 'hgnc_symbol'] <- 'Symbol'
   
      # Handle missing symbols using org.Hs.eg.db
      missing_symbols <- is.na(res$Symbol) | res$Symbol == ''
      if (any(missing_symbols)) {
        res$Symbol[missing_symbols] <- mapIds(
          org.Hs.eg.db,
          keys = res$ENSEMBL[missing_symbols],
          column = 'SYMBOL', keytype = 'ENSEMBL',
          multiVals = 'first'
        )
      }
     
      res$Symbol[is.na(res$Symbol) | res$Symbol == ''] <- res$ENSEMBL[is.na(res$Symbol) | res$Symbol == '']
     
    }else {
      # If row names are gene symbols, attempt to standardize them
      res$Symbol <- mapIds(
        org.Hs.eg.db,
        keys = original_ids,
        column = 'SYMBOL', keytype = 'SYMBOL',
        multiVals = 'first'
      )
     
      # Replace missing symbols with original IDs
      res$Symbol[is.na(res$Symbol) | res$Symbol == ''] <- original_ids
    }
   
    # Ensure unique Symbol names
    res$Symbol <- make.unique(res$Symbol, sep = '_dup')"
        )
      }
    })
  })
  
  ## GENE ONTOLOGY ANALYSIS ####
  
  observeEvent(input$go_analysis, {
    req(reactiveVolcanoData$selected_genes, input$comparison_selector, input$species, input$logfc_cutoff, input$adjp_cutoff)
    
    showNotification("Running GO Analysis... Please wait.", type = "message", duration = NULL, id = "go_analysis_msg")
    
    tryCatch({
      selected_genes <- reactiveVolcanoData$selected_genes[[input$comparison_selector]]
      
      if (is.null(selected_genes) || nrow(selected_genes) == 0) {
        removeNotification("go_analysis_msg")
        return(NULL)
      }
      
      selected_orgdb <- if (input$species == "org.Mm.eg.db") org.Mm.eg.db else org.Hs.eg.db
      
      upregulated_genes <- selected_genes %>%
        filter(log2FoldChange > input$logfc_cutoff & padj < input$adjp_cutoff) %>%
        pull(Symbol)
      
      downregulated_genes <- selected_genes %>%
        filter(log2FoldChange < -input$logfc_cutoff & padj < input$adjp_cutoff) %>%
        pull(Symbol)
      
      upregulated_list <- bitr(upregulated_genes, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = selected_orgdb)
      downregulated_list <- bitr(downregulated_genes, fromType = "SYMBOL", toType = "ENTREZID", OrgDb = selected_orgdb)
      
      if (is.null(upregulated_list) || is.null(downregulated_list) || 
          nrow(upregulated_list) == 0 || nrow(downregulated_list) == 0) {
        showNotification("No mapped ENTREZ IDs found. Try a different species or gene set.", type = "error")
        removeNotification("go_analysis_msg")
        return(NULL)
      }
      
      go_bp_up <- enrichGO(gene = upregulated_list$ENTREZID, OrgDb = selected_orgdb, ont = "BP", pAdjustMethod = "BH", readable = TRUE)
      go_bp_down <- enrichGO(gene = downregulated_list$ENTREZID, OrgDb = selected_orgdb, ont = "BP", pAdjustMethod = "BH", readable = TRUE)
      
      go_mf_up <- enrichGO(gene = upregulated_list$ENTREZID, OrgDb = selected_orgdb, ont = "MF", pAdjustMethod = "BH", readable = TRUE)
      go_mf_down <- enrichGO(gene = downregulated_list$ENTREZID, OrgDb = selected_orgdb, ont = "MF", pAdjustMethod = "BH", readable = TRUE)
      
      reactiveValues$go_bp_up <- go_bp_up
      reactiveValues$go_bp_down <- go_bp_down
      reactiveValues$go_mf_up <- go_mf_up
      reactiveValues$go_mf_down <- go_mf_down
      
      removeNotification("go_analysis_msg")
      
    }, error = function(e) {
      showNotification(paste("Error in GO Analysis:", e$message), type = "error")
      removeNotification("go_analysis_msg")
    })
  })
  
  ## RENDER GO DOT PLOTS FOR BP & MF (UP AND DOWN REGULATED) ####
  
  output$bp_combined_plot <- renderPlot({
    req(reactiveValues$go_bp_up, reactiveValues$go_bp_down, input$go_term_count)
    
    tryCatch({
      if (nrow(reactiveValues$go_bp_up@result) == 0 || nrow(reactiveValues$go_bp_down@result) == 0) {
        showNotification("No enriched BP terms found. Try adjusting thresholds.", type = "warning")
        stop("No enriched BP terms found.")
      }
      
      p1 <- dotplot(reactiveValues$go_bp_up, showCategory = input$go_term_count, title = "BP - Upregulated Genes") + theme_minimal() +
        theme(
          axis.text.x = element_text(size = 18, angle = 45, hjust = 1), 
          axis.text.y = element_text(size = 18),  
          plot.title = element_text(size = 22, face = "bold")
        )
      p2 <- dotplot(reactiveValues$go_bp_down, showCategory = input$go_term_count, title = "BP - Downregulated Genes") + theme_minimal() +
        theme(
          axis.text.x = element_text(size = 18, angle = 45, hjust = 1),  
          axis.text.y = element_text(size = 18),  
          plot.title = element_text(size = 22, face = "bold")
        )
      
      gridExtra::grid.arrange(p1, p2, ncol = 2)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste(e$message), cex = 1.2)
    })
  }, height = 1100)
  
  output$mf_combined_plot <- renderPlot({
    req(reactiveValues$go_mf_up, reactiveValues$go_mf_down, input$go_term_count)
    
    tryCatch({
      if (nrow(reactiveValues$go_mf_up@result) == 0 || nrow(reactiveValues$go_mf_down@result) == 0) {
        showNotification("No enriched MF terms found. Try changing filters.", type = "warning")
        stop("No enriched MF terms found.")
      }
      
      p1 <- dotplot(reactiveValues$go_mf_up, showCategory = input$go_term_count, title = "MF - Upregulated") + theme_minimal() +
        theme(
          axis.text.x = element_text(size = 18, angle = 45, hjust = 1), 
          axis.text.y = element_text(size = 18), 
          plot.title = element_text(size = 22, face = "bold")
        )
      p2 <- dotplot(reactiveValues$go_mf_down, showCategory = input$go_term_count, title = "MF - Downregulated") + theme_minimal() +
        theme(
          axis.text.x = element_text(size = 18, angle = 45, hjust = 1),  
          axis.text.y = element_text(size = 18), 
          plot.title = element_text(size = 22, face = "bold")
        )
      
      gridExtra::grid.arrange(p1, p2, ncol = 2)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste(e$message), cex = 1.2)
    })
  }, height = 1100)
  
    ### DOWNLOAD BP AND MF PLOTS ####
  
  output$download_bp_combined_plot <- downloadHandler(
    filename = function() { paste("GO_BP_Combined_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      tryCatch({
        req(reactiveValues$go_bp_up, reactiveValues$go_bp_down, input$go_term_count)
        
        if (nrow(reactiveValues$go_bp_up@result) == 0 || nrow(reactiveValues$go_bp_down@result) == 0) {
          showNotification("No enriched BP terms found. Adjust thresholds.", type = "warning")
          stop("No enriched BP terms found.")
        }
        
        p1 <- dotplot(reactiveValues$go_bp_up, showCategory = input$go_term_count, title = "BP - Upregulated") + theme_minimal() 
        p2 <- dotplot(reactiveValues$go_bp_down, showCategory = input$go_term_count, title = "BP - Downregulated") + theme_minimal() 
        
        combined_plot <- gridExtra::arrangeGrob(p1, p2, ncol = 2)
        ggsave(file, plot = combined_plot, width = 12, height = 6, dpi = 300, bg = "white")
        
      }, error = function(e) {
        showNotification(paste("Error generating BP Combined plot:", e$message), type = "error")
      })
    }
  )
  
  output$download_mf_combined_plot <- downloadHandler(
    filename = function() { paste("GO_MF_Combined_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      tryCatch({
        req(reactiveValues$go_mf_up, reactiveValues$go_mf_down, input$go_term_count)
        
        if (nrow(reactiveValues$go_mf_up@result) == 0 || nrow(reactiveValues$go_mf_down@result) == 0) {
          showNotification("No enriched MF terms found. Adjust filters.", type = "warning")
          stop("No enriched MF terms found.")
        }
        
        p1 <- dotplot(reactiveValues$go_mf_up, showCategory = input$go_term_count, title = "MF - Upregulated") + theme_minimal() +
          theme(
            axis.text.x = element_text(size = 18, angle = 45, hjust = 1), 
            axis.text.y = element_text(size = 18),  
            plot.title = element_text(size = 22, face = "bold")
          )
        p2 <- dotplot(reactiveValues$go_mf_down, showCategory = input$go_term_count, title = "MF - Downregulated") + theme_minimal() +
          theme(
            axis.text.x = element_text(size = 18, angle = 45, hjust = 1), 
            axis.text.y = element_text(size = 18),  
            plot.title = element_text(size = 22, face = "bold")
          )
        
        combined_plot <- gridExtra::arrangeGrob(p1, p2, ncol = 2)
        ggsave(file, plot = combined_plot, width = 12, height = 6, dpi = 300, bg = "white")
        
      }, error = function(e) {
        showNotification(paste("Error generating MF Combined plot:", e$message), type = "error")
      })
    }
  )
  
    ### BP & MF CODES ####
  
  observeEvent(input$toggle_bp_code, {
    output$bp_code <- renderUI({
      if (input$toggle_bp_code %% 2 == 1) {
        aceEditor(
          outputId = "bp_code",
          mode = "r",
          theme = "solarized_light",
          readOnly = TRUE,
          height = "250px",
          value = "## Gene Ontology Analysis - Biological Processes
          
  # Extract significant genes with adjusted p-value < input$adjp_cutoff
  significant_genes <- rownames(
    reactiveValues$deg_data %>%
      dplyr::filter(padj < input$adjp_cutoff)
  )

  # Convert gene symbols to ENTREZ IDs
  gene_list <- bitr(significant_genes, fromType = 'SYMBOL', toType = 'ENTREZID', OrgDb = org.Hs.eg.db)

  # Perform GO enrichment analysis for Biological Processes
  go_bp <- enrichGO(
    gene = gene_list$ENTREZID,
    OrgDb = org.Hs.eg.db,
    ont = 'BP',
    pAdjustMethod = 'BH',
    readable = TRUE
  )

  # Generate BP Bar Plot
  barplot(go_bp, showCategory = input$go_term_count, title = 'BP - Upregulated')"
        )
      }
    })
  })
  
  observeEvent(input$toggle_mf_code, {
    output$mf_code <- renderUI({
      if (input$toggle_mf_code %% 2 == 1) {
        aceEditor(
          outputId = "mf_code",
          mode = "r",
          theme = "solarized_light",
          readOnly = TRUE,
          height = "250px",
          value = "## Gene Ontology Analysis - Molecular Functions
  # Extract significant genes with adjusted p-value < input$adjp_cutoff
  significant_genes <- rownames(
    reactiveValues$deg_data %>%
      dplyr::filter(padj < input$adjp_cutoff)
  )

  # Convert gene symbols to ENTREZ IDs
  gene_list <- bitr(significant_genes, fromType = 'SYMBOL', toType = 'ENTREZID', OrgDb = org.Hs.eg.db)

  # Perform GO enrichment analysis for Molecular Functions
  go_mf <- enrichGO(
    gene = gene_list$ENTREZID,
    OrgDb = org.Hs.eg.db,
    ont = 'MF',
    pAdjustMethod = 'BH',
    readable = TRUE
  )

  # Generate MF Bar Plot
  barplot(go_mf, showCategory = input$go_term_count, title = 'MF Upregulated')"
        )
      }
    })
  })
  
  ## GENE SET ENRICHMENT ANALYSIS (GSEA) ####
  
  observeEvent(input$gsea_analysis, {
    req(reactiveVolcanoData$selected_genes, input$comparison_selector, input$species, input$adjp_cutoff)
    
    showNotification("Running GSEA Analysis... Please wait.", type = "message", duration = NULL, id = "gsea_analysis_msg")
    
    tryCatch({
      selected_genes <- reactiveVolcanoData$selected_genes[[input$comparison_selector]]
      
      if (is.null(selected_genes) || nrow(selected_genes) == 0) {
        showNotification("No significant genes found. Adjust filters.", type = "warning")
        return(NULL)
      }
      
      selected_genes <- selected_genes[abs(selected_genes$log2FoldChange) >= 0.58, ]
      selected_orgdb <- if (input$species == "org.Mm.eg.db") org.Mm.eg.db else org.Hs.eg.db
      if (!"Symbol" %in% colnames(selected_genes)) {
        selected_genes$Symbol <- rownames(selected_genes)
      }
      
      ranked_genes <- setNames(selected_genes$log2FoldChange, toupper(selected_genes$Symbol))
      ranked_genes <- ranked_genes[!is.na(ranked_genes)]  
      ranked_genes <- sort(ranked_genes, decreasing = TRUE) 
      
      if (length(ranked_genes) == 0) {
        showNotification("No valid genes for ranking. Try adjusting filters.", type = "error")
        return(NULL)
      }
      
      gsea_results <- gseGO(
        geneList = ranked_genes,
        OrgDb = selected_orgdb,
        ont = "BP",
        keyType = "SYMBOL",
        minGSSize = 10,
        maxGSSize = 3000,
        pvalueCutoff = input$adjp_cutoff,
        eps = 0,
        verbose = TRUE
      )
      
      if (!is.null(gsea_results) && nrow(gsea_results@result) > 0) {
        reactiveValues$gsea_object <- gsea_results
        reactiveValues$gsea_results_df <- as.data.frame(gsea_results@result)
      } else {
        showNotification("No enriched GSEA terms found. Try adjusting the p-value cutoff.", type = "warning")
        reactiveValues$gsea_object <- NULL
        reactiveValues$gsea_results_df <- NULL
      }
      
      removeNotification("gsea_analysis_msg")
      
    }, error = function(e) {
      showNotification(paste("Error in GSEA Analysis:", e$message), type = "error")
      removeNotification("gsea_analysis_msg")
    })
  })
  
    ### FIXED GSEA PLOT ####
  
  output$gsea_plot <- renderPlot({
    req(reactiveValues$gsea_object)
    
    tryCatch({
      if (is.null(reactiveValues$gsea_object)) {
        showNotification("GSEA data is missing. Try running analysis again.", type = "error")
        stop("GSEA data is missing! Ensure you ran the analysis correctly.")
      }
      
      if (nrow(reactiveValues$gsea_object@result) == 0) {
        showNotification("No enriched GSEA terms found. Try adjusting the p-value cutoff.", type = "warning")
        stop("No GSEA terms enriched under the selected p-value cutoff.")
      }
      
      dotplot(reactiveValues$gsea_object,
              showCategory = 15,
              split = ".sign",
              font.size = 7,
              title = paste("GSEA -", input$comparison_selector),
              orderBy = "x",  
              label_format = 100
      ) +
        facet_grid(~.sign) +
        theme(
          panel.spacing = unit(0.5, "cm"),
          axis.text.x = element_text(size = 18, angle = 45, hjust = 1), 
          axis.text.y = element_text(size = 18),  
          plot.title = element_text(size = 22, face = "bold")
        ) 
      
    }, error = function(e) {
      showNotification(paste("Error in GSEA Plot:", e$message), type = "error")
      NULL
    })
  }, height = 700)
  
      #### DOWNLOAD GSEA PLOT ####
  
  output$download_gsea_plot <- downloadHandler(
    filename = function() {
      paste("GSEA_plot_", input$comparison_selector, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tryCatch({
        req(reactiveValues$gsea_object)
        
        if (is.null(reactiveValues$gsea_object) || nrow(reactiveValues$gsea_object@result) == 0) {
          showNotification("No enriched GSEA terms found. Adjust filters.", type = "warning")
          stop("No enriched GSEA terms found.")
        }
        
        p <- dotplot(reactiveValues$gsea_object,
                     showCategory = 15,
                     split = ".sign",
                     font.size = 7,
                     title = paste("GSEA -", input$comparison_selector),
                     orderBy = "x",  
                     label_format = 100
        ) +
          facet_grid(~.sign)  +
          theme(
            panel.spacing = unit(0.5, "cm")
          ) 
        
        ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
        
      }, error = function(e) {
        showNotification(paste("Error in GSEA Plot:", e$message), type = "error")
      })
    }
  )
  
      #### GSEA PLOT CODE ####
  
  observeEvent(input$toggle_gsea_code, {
    output$gsea_code <- renderUI({
      if (input$toggle_gsea_code %% 2 == 1) {
        aceEditor(
          outputId = "gsea_code",
          mode = "r",
          theme = "solarized_light",
          readOnly = TRUE,
          height = "250px",
          value = "## Gene Set Enrichment Analysis (GSEA) - Dot Plot
          
  # Convert DEG results to data frame

  # Prepare ranked gene list for GSEA
  ranked_genes <- res$log2FoldChange
  names(ranked_genes) <- toupper(res$SYMBOL)
  ranked_genes <- ranked_genes[!is.na(ranked_genes)]
  ranked_genes <- sort(ranked_genes, decreasing = TRUE)

  # Perform GSEA
  gsea_results <- gseGO(
    geneList = ranked_genes,
    OrgDb = org.Hs.eg.db,  
    ont = 'ALL',            
    keyType = 'SYMBOL',
    minGSSize = 5,          
    maxGSSize = 1000,
    pvalueCutoff = 0.05,
    eps = 0,
    verbose = TRUE
  )

  # Generate GSEA Dot Plot
  dotplot(
    gsea_results,
    showCategory = 15,
    split = '.sign',
    font.size = 7,
    title = 'A vs B Genes - Molecular Functions',
    orderBy = 'x',  
    label_format = 100
  ) +
  facet_grid(~.sign) +  
  theme(panel.spacing = unit(0.5, 'cm'))"
        )
      }
    })
  })
  
    ### FIXED GSEA RESULTS TABLE ####
  
  output$gsea_results <- renderDataTable({
    req(reactiveValues$gsea_results_df)
    datatable(
      reactiveValues$gsea_results_df,
      options = list(pageLength = 10, autoWidth = TRUE)
    )
  })
  
  ## HTML PAGE ####
  
  output$save_static <- downloadHandler(
    filename = function() {
      req(input$last_name)
      date_str <- format(Sys.Date(), "%m_%d_%y")
      paste0(input$last_name, "_", date_str, ".html")
    },
    content = function(file) {
      tempReport <- tempfile(fileext = ".Rmd")
      showNotification("Generating HTML Report... Please wait.", type = "message", duration = NULL, id = "html_msg")
      
      ### WRITE LINES ####
      
      writeLines(
        c(
          "---",
          paste0("title: \"", input$report_title, "\""),
          "sub_title: \"Suzuki Lab RNA Sequencing Report\"",
          "output: ",
          "  html_document:",
          "    toc_float: true",
          "    toc: true",
          "    number_sections: true",
          "    css: custom_styles.css",
          "params:",
          "  first_name: NULL",
          "  last_name: NULL",
          "  uin: NULL",
          "  objective: NULL",
          "  findings: NULL",
          "  abstract: NULL",
          "  metadata: NULL",
          "  raw_counts: NULL",
          "  pca_plot: NULL",
          "  pcaplot_text: NULL",
          "  heatmap_plot: NULL",
          "  heatmapplot_text: NULL",
          "  box_plot: NULL",
          "  box_plot_norm: NULL",
          "  boxplot_text: NULL",
          "  boxplot_text_norm: NULL",
          "  volcano_plot: NULL",
          "  volcanoplot_text: NULL",
          "  go_term_count: NULL",
          "  go_bp_up: NULL",
          "  go_bp_down: NULL",
          "  bpplot_text: NULL",
          "  go_mf_up: NULL",
          "  go_mf_down: NULL",
          "  mfplot_text: NULL",
          "  logfc_cutoff: NULL",
          "  adjp_cutoff: NULL",
          "  gsea_results: NULL",
          "  gsea_plot: NULL",
          "  gseaplot_text: NULL",
          "  conclusion_input: NULL",
          "  report_time: NULL",
          "---",
          "",
          "# Student Information",
          "```{r, echo=FALSE}",
          "cat(paste0('Name: ', params$first_name, ' ', params$last_name))",
          "cat(paste0('UIN: ', params$uin))",
          "cat('This report was generated on:', params$report_time)",
          "```",
          "",
          "# Objective",
          "```{r, echo=FALSE}",
          "cat(ifelse(is.null(params$objective) || params$objective == \"\", \"No Objective Provided\", params$objective))",
          "```",
          "",
          "# Findings",
          "```{r, echo=FALSE}",
          "cat(ifelse(is.null(params$findings) || params$findings == \"\", \"No Findings Provided\", params$findings))",
          "```",
          "",
          "# Abstract",
          "```{r, echo=FALSE}",
          "cat(ifelse(is.null(params$abstract) || params$abstract == \"\", \"No Abstract Provided\", params$abstract))",
          "```",
          "",
          "# Metadata",
          "```{r, echo=FALSE}",
          "knitr::kable(params$metadata, format = 'html', caption = 'Sample Metadata Table') %>%kableExtra::kable_styling('striped', full_width = FALSE)",
          "```",
          "",
          "# Box-and-Whisker Plot",
          "```{r, echo = FALSE, fig.width=7, fig.height=5}",
          "tryCatch({",
          "  if (!is.null(params$box_plot)) { params$box_plot } else { cat('Box-and-Whisker plot unavailable') }",
          "}, error = function(e) {",
          "  cat('Error in Box-and-Whisker Plot:', e$message)",
          "})",
          "```",
          "",
          "## Box-and-Whisker Interpretation",
          "```{r, echo = FALSE}",
          "cat(ifelse(is.null(params$boxplot_text) || params$boxplot_text == \"\", \"No Box-and-Whisker Interpretation Provided\", params$boxplot_text))",
          "```",
          "",
          "# Normalized Box-and-Whisker Plot",
          "```{r, echo = FALSE, fig.width=7, fig.height=5}",
          "tryCatch({",
          "  if (!is.null(params$box_plot_norm)) { params$box_plot_norm } else { cat('Normalized Box-and-Whisker plot unavailable') }",
          "}, error = function(e) {",
          "  cat('Error in Normalized Box-and-Whisker Plot:', e$message)",
          "})",
          "```",
          "",
          "## Normalized Box-and-Whisker Interpretation",
          "```{r, echo = FALSE}",
          "cat(ifelse(is.null(params$boxplot_text_norm) || params$boxplot_text_norm == \"\", \"No Normalized Box-and-Whisker Interpretation Provided\", params$boxplot_text_norm))",
          "```",
          "",
          "# PCA Plot",
          "```{r, echo=FALSE, fig.width=7, fig.height=5}",
          "tryCatch({",
          "  if (!is.null(params$pca_plot)) { params$pca_plot } else { cat('PCA plot unavailable') }",
          "}, error = function(e) {",
          "  cat('Error in PCA plot:', e$message)",
          "})",
          "```",
          "",
          "## PCA Interpretation",
          "```{r, echo = FALSE}",
          "cat(ifelse(is.null(params$pcaplot_text) || params$pcaplot_text == \"\", \"No PCA Interpretation Provided\", params$pcaplot_text))",
          "```",
          "",
          "# Heatmap Plot",
          "```{r, echo = FALSE, fig.width=7, fig.height=5}",
          "tryCatch({",
          "  if (!is.null(params$heatmap_plot)) { params$heatmap_plot } else { cat('Heatmap plot unavailable') }",
          "}, error = function(e) {",
          "  cat('Error in Heatmap plot:', e$message)",
          "})",
          "```",
          "",
          "## Heatmap Interpretation",
          "```{r, echo = FALSE}",
          "cat(ifelse(is.null(params$heatmapplot_text) || params$heatmapplot_text == \"\", \"No Heatmap Interpretation Provided\", params$heatmapplot_text))",
          "```",
          "",
          "# Enhanced Volcanic Plot",
          "```{r, echo=FALSE, fig.width = 10, fig.height = 5}",
          "tryCatch({",
          "  if (!is.null(params$volcano_plot)) { params$volcano_plot } else { cat('Volcano plot unavailable') }",
          "}, error = function(e) {",
          "  cat('Error in Volcano plot:', e$message)",
          "})",
          "```",
          "",
          "## Volcanic Plot Interpretation",
          "```{r, echo = FALSE}",
          "cat(ifelse(is.null(params$volcanoplot_text) || params$volcanoplot_text == \"\", \"No Volcano Interpretation Provided\", params$volcanoplot_text))",
          "```",
          "",
          "# Gene Ontology Analysis",
          "## Biological Process",
          "```{r, echo=FALSE, fig.height=10, fig.width=10}",
          "tryCatch({",
          "  if (!is.null(params$go_bp_up) && nrow(params$go_bp_up@result) > 0) {",
          "    dotplot(params$go_bp_up, showCategory = params$go_term_count, ",
          "            title = paste('Top', params$go_term_count, 'BP - Upregulated Genes')) +",
          "      theme_minimal() +",
          "      theme(",
          "        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),",
          "        axis.text.y = element_text(size = 18),",
          "        plot.title = element_text(size = 22, face = 'bold')",
          "      )",
          "  } else {",
          "    cat('No BP enrichment found for upregulated genes.')",
          "  }",
          "}, error = function(e) {",
          "  cat('Error in BP Upregulated plot:', e$message)",
          "})",
          "```",
          "",
          "```{r, echo=FALSE, fig.height=10, fig.width=10}",
          "tryCatch({",
          "  if (!is.null(params$go_bp_down) && nrow(params$go_bp_down@result) > 0) {",
          "    dotplot(params$go_bp_down, showCategory = params$go_term_count, ",
          "            title = paste('Top', params$go_term_count, 'BP - Downregulated Genes')) +",
          "      theme_minimal() +",
          "      theme(",
          "        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),",
          "        axis.text.y = element_text(size = 18),",
          "        plot.title = element_text(size = 22, face = 'bold')",
          "      )",
          "  } else {",
          "    cat('No BP enrichment found for downregulated genes.')",
          "  }",
          "}, error = function(e) {",
          "  cat('Error in BP Downregulated plot:', e$message)",
          "})",
          "```",
          "### Biological Process Plot Interpretation",
          "```{r, echo = FALSE}",
          "cat(ifelse(is.null(params$bpplot_text) || params$bpplot_text == \"\", \"No BP Plot Interpretation Provided\", params$bpplot_text))",
          "```",
          "",
          "## Molecular Function",
          "```{r, echo=FALSE, fig.height=10, fig.width=10}",
          "tryCatch({",
          "  if (!is.null(params$go_mf_up) && nrow(params$go_mf_up@result) > 0) {",
          "    dotplot(params$go_mf_up, showCategory = params$go_term_count, ",
          "            title = paste('Top', params$go_term_count, 'MF - Upregulated Genes')) +",
          "      theme_minimal() +",
          "      theme(",
          "        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),",
          "        axis.text.y = element_text(size = 18),",
          "        plot.title = element_text(size = 22, face = 'bold')",
          "      )",
          "  } else {",
          "    cat('No MF enrichment found for upregulated genes.')",
          "  }",
          "}, error = function(e) {",
          "  cat('Error in MF Upregulated plot:', e$message)",
          "})",
          "```",
          "",
          "```{r, echo=FALSE, fig.height=10, fig.width=10}",
          "tryCatch({",
          "  if (!is.null(params$go_mf_down) && nrow(params$go_mf_down@result) > 0) {",
          "    dotplot(params$go_mf_down, showCategory = params$go_term_count, ",
          "            title = paste('Top', params$go_term_count, 'MF - Downregulated Genes')) +",
          "      theme_minimal() +",
          "      theme(",
          "        axis.text.x = element_text(size = 18, angle = 45, hjust = 1),",
          "        axis.text.y = element_text(size = 18),",
          "        plot.title = element_text(size = 22, face = 'bold')",
          "      )",
          "  } else {",
          "    cat('No MF enrichment found for downregulated genes.')",
          "  }",
          "}, error = function(e) {",
          "  cat('Error in MF Downregulated plot:', e$message)",
          "})",
          "```",
          "",
          "### Molecular Function Plot Interpretation",
          "```{r, echo = FALSE}",
          "cat(ifelse(is.null(params$mfplot_text) || params$mfplot_text == \"\", \"No MF Plot Interpretation Provided\", params$mfplot_text))",
          "```",
          "",
          "# Gene Set Enrichment Analysis (GSEA) Dot Plot",
          "```{r, echo=FALSE, fig.width = 10, fig.height = 10}",
          "tryCatch({",
          "  if (!is.null(params$gsea_plot)) { params$gsea_plot } else { cat('GSEA plot unavailable: No enriched terms or data missing.') }",
          "}, error = function(e) {",
          "  cat('Error in GSEA plot:', e$message)",
          "})",
          "```",
          "",
          "## GSEA Dot Plot Interpretation",
          "```{r, echo = FALSE}",
          "cat(ifelse(is.null(params$gseaplot_text) || params$gseaplot_text == \"\", \"No GSEA Plot Interpretation Provided\", params$gseaplot_text))",
          "```",
          "",
          "# Conclusion",
          "```{r, echo=FALSE}",
          "cat(ifelse(is.null(params$conclusion_input) || params$conclusion_input == \"\", \"No Conclusion Provided\", params$conclusion_input))",
          "```",
          ""
        ),
        tempReport
      )
      
      file.copy("custom_styles.css", dirname(tempReport))
      
      ### RENDERING RMARKDOWN ####
      
      tryCatch({

        if (input$first_name == "" || input$last_name == "" || input$uin == "") {
          showNotification("Please fill in First Name, Last Name, and UIN before downloading the report.", type = "error")
          removeNotification("html_msg")
          return(NULL)
        }
        
        rmarkdown::render(
          input = tempReport,
          output_file = file,
          params = list(
            first_name = input$first_name,
            last_name = input$last_name,
            uin = input$uin,
            report_time = format(Sys.time(), "%m/%d/%y %I:%M %p"),
            objective = input$objective_input,
            findings = input$findings_input,
            abstract = input$abstract_input,
            metadata = if (is.null(metadata())) NULL else as.data.frame(metadata()),
            raw_counts = if (is.null(raw_counts())) NULL else as.data.frame(raw_counts()),
            logfc_cutoff = input$logfc_cutoff,
            adjp_cutoff = input$adjp_cutoff,
            go_term_count = input$go_term_count,
            
            #### PRE-NORMALIZED BOX PLOT ####
            
            box_plot = {
              req(raw_counts(), metadata())
              
              tryCatch({
                raw_data <- as.data.frame(raw_counts())
                meta_data <- as.data.frame(metadata())
                
                # Detect all condition columns dynamically
                condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(meta_data), value = TRUE)
                
                if (length(condition_cols) == 0) {
                  stop("Metadata is missing 'Condition' columns. Please check your file.")
                }
                
                # Create a combined condition column
                meta_data$Combined_Condition <- apply(meta_data[, condition_cols, drop = FALSE], 1, paste, collapse = "_")
                
                # Ensure raw counts and metadata sample names match
                if (!all(colnames(raw_data) %in% rownames(meta_data))) {
                  stop("Column names of raw counts and row names of metadata do not match!")
                }
                
                # Reshape raw count data
                df <- reshape2::melt(raw_data, variable.name = "Sample", value.name = "Raw_Counts")
                
                # Log2 transformation
                df$Log2_Transformed_Counts <- log2(df$Raw_Counts + 1)
                
                # Merge with metadata
                merged_df <- merge(df, meta_data, by.x = "Sample", by.y = "row.names", all.x = TRUE)
                
                # Assign the new combined condition column
                merged_df$Condition <- merged_df$Combined_Condition
                
                unique_groups <- unique(merged_df$Condition)
                num_groups <- length(unique_groups)
                color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_groups)
                
                ggplot(merged_df, aes(x = Sample, y = Log2_Transformed_Counts, fill = Condition)) +
                  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
                  scale_fill_manual(values = color_palette) +
                  xlab("Samples") +
                  ylab("log2(Raw_Counts + 1)") + 
                  ggtitle("Log2 Transformed Counts Across Samples (Grouped by Combined Condition)") +
                  theme_minimal() +
                  theme(
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    legend.position = "bottom"
                  )
                
              }, error = function(e) {
                showNotification(paste("Error generating boxplot:", e$message), type = "error")
              })
            },
            
            boxplot_text = input$boxplot_text,
            
            #### NORMALIZED BOX PLOT ####
            
            box_plot_norm = {
              req(raw_counts(), metadata())
              
              tryCatch({
                raw_data <- as.data.frame(raw_counts())
                meta_data <- as.data.frame(metadata())
                
                # Detect all condition columns dynamically
                condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(meta_data), value = TRUE)
                
                if (length(condition_cols) == 0) {
                  stop("Metadata is missing 'Condition' columns. Please check your file.")
                }
                
                # Create a combined condition column
                meta_data$Combined_Condition <- apply(meta_data[, condition_cols, drop = FALSE], 1, paste, collapse = "_")
                
                # Convert to factor
                meta_data$Combined_Condition <- as.factor(meta_data$Combined_Condition)
                
                # Ensure raw counts and metadata sample names match
                if (!all(colnames(raw_data) %in% rownames(meta_data))) {
                  stop("Column names of raw counts and row names of metadata do not match!")
                }
                
                # Create DESeq2 dataset using Combined_Condition
                dds <- DESeqDataSetFromMatrix(
                  countData = round(as.matrix(raw_data)),
                  colData = meta_data,
                  design = ~ Combined_Condition
                )
                
                # Filter out low-expressed genes
                dds <- dds[rowSums(counts(dds)) > 10, ]
                
                # Perform DESeq2 normalization
                dds <- estimateSizeFactors(dds)
                norm_counts <- counts(dds, normalized = TRUE)
                pseudoCount <- log2(norm_counts + 1)
                
                # Reshape normalized count data
                df <- reshape2::melt(pseudoCount)
                
                # Merge metadata
                merged_df <- merge(df, meta_data, by.x = "Var2", by.y = "row.names", all.x = TRUE)
                
                # Assign the new combined condition column
                merged_df$Condition <- merged_df$Combined_Condition
                
                unique_groups <- unique(merged_df$Condition)
                num_groups <- length(unique_groups)
                color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_groups)
                
                ggplot(merged_df, aes(x = Var2, y = value, fill = Condition)) +
                  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
                  scale_fill_manual(values = color_palette) +
                  xlab("Samples") +
                  ylab(expression(log[2](Normalized~Counts~+~1))) +
                  ggtitle("Normalized Counts Across Samples (Grouped by Combined Condition)") +
                  theme_minimal() +
                  theme(
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    legend.position = "bottom"
                  )
                
              }, error = function(e) {
                showNotification(paste("Error generating boxplot:", e$message), type = "error")
              })
            },
            
            boxplot_text_norm = input$boxplot_text_norm,
            
            #### PCA PLOT ####
            
            pca_plot = {
              req(raw_counts(), metadata())
              
              tryCatch({
                meta_data <- metadata()
                
                # Detect all condition columns dynamically
                condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(meta_data), value = TRUE)
                
                if (length(condition_cols) == 0) {
                  stop("Metadata is missing 'Condition' columns. Please check your file.")
                }
                
                # Create a combined condition column
                meta_data$Combined_Condition <- apply(meta_data[, condition_cols, drop = FALSE], 1, paste, collapse = "_")
                
                # Convert to factor
                meta_data$Combined_Condition <- as.factor(meta_data$Combined_Condition)
                
                # Ensure raw counts and metadata sample names match
                if (!all(colnames(raw_counts()) %in% rownames(meta_data))) {
                  stop("Column names of raw counts and row names of metadata do not match!")
                }
                
                # Create DESeq2 dataset using Combined_Condition
                dds <- DESeqDataSetFromMatrix(
                  countData = round(as.matrix(raw_counts())),
                  colData = meta_data,
                  design = ~ Combined_Condition
                )
                
                # Filter out low-expressed genes
                dds <- dds[rowSums(counts(dds)) > 10, ]
                
                # Perform DESeq2 normalization
                dds <- estimateSizeFactors(dds)
                rlog_data <- rlog(dds, blind = TRUE)
                
                # Perform PCA analysis
                pc <- prcomp(t(assay(rlog_data)))
                pc_data <- as.data.frame(pc$x[, 1:2])
                colnames(pc_data) <- c("PC1", "PC2")
                
                # Assign the new combined condition column
                pc_data$Condition <- meta_data$Combined_Condition
                
                unique_groups <- unique(pc_data$Condition)
                num_groups <- length(unique_groups)
                color_palette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(num_groups)
                
                ggplot(pc_data, aes(x = PC1, y = PC2, color = Condition)) +
                  geom_point(size = 5) +
                  scale_color_manual(values = color_palette) +
                  labs(title = "PCA Plot (Grouped by Combined Condition)", x = "PC1", y = "PC2") +
                  theme_minimal()
                
              }, error = function(e) {
                stop("Error generating PCA plot: ", e$message)
              })
            },
            
            pcaplot_text = input$pcaplot_text,
            
            #### HEATMAP PLOT ####
            
            heatmap_plot = {
              req(raw_counts(), metadata())
              
              tryCatch({
                meta_data <- metadata()
                
                condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(meta_data), value = TRUE)
                
                if (length(condition_cols) == 0) {
                  stop("Metadata is missing 'Condition' columns. Please check your file.")
                }
                
                meta_data$Combined_Condition <- apply(meta_data[, condition_cols, drop = FALSE], 1, paste, collapse = "_")
                
                meta_data$Combined_Condition <- as.factor(meta_data$Combined_Condition)
                
                raw_counts_matrix <- round(as.matrix(raw_counts()))
                if (!all(colnames(raw_counts_matrix) %in% rownames(meta_data))) {
                  stop("Column names of raw counts and row names of metadata do not match!")
                }
                
                dds <- DESeqDataSetFromMatrix(
                  countData = raw_counts_matrix,
                  colData = meta_data,
                  design = ~ Combined_Condition
                )
                
                dds <- dds[rowSums(counts(dds)) > 10, ]
                
                rlog_data <- rlog(dds, blind = TRUE)
                
                dist_matrix <- dist(t(assay(rlog_data)))
                mat <- as.matrix(dist_matrix)
                rownames(mat) <- colnames(mat) <- colnames(raw_counts_matrix)
                
                annotation_col <- data.frame(Condition = meta_data$Combined_Condition)
                rownames(annotation_col) <- rownames(meta_data)
                
                ComplexHeatmap::Heatmap(
                  mat,
                  name = "Distance",
                  col = colorRampPalette(rev(brewer.pal(9, "Reds")))(255),
                  column_title = "Sample Distance Heatmap",
                  cluster_rows = TRUE,
                  cluster_columns = TRUE,
                  top_annotation = ComplexHeatmap::HeatmapAnnotation(df = annotation_col)
                )
                
              }, error = function(e) {
                stop("Error generating heatmap: ", e$message)
              })
            },
            
            heatmapplot_text = input$heatmapplot_text,
            
            #### ENHANCED VOLCANO PLOT ####
            
            volcano_plot = {
              req(raw_counts(), metadata(), input$comparison_selector, input$logfc_cutoff, input$adjp_cutoff)
              
              tryCatch({
                comparison <- input$comparison_selector
                comparison_split <- unlist(strsplit(comparison, " vs "))
                
                if (length(comparison_split) != 2) {
                  stop("Invalid comparison format. Please select two different conditions.")
                }
                
                condition_1 <- comparison_split[1]
                condition_2 <- comparison_split[2]
                
                meta_data <- metadata()
                
                condition_cols <- grep("^Condition(_[0-9]+)?$", colnames(meta_data), value = TRUE)
                
                if (length(condition_cols) == 0) {
                  stop("Metadata is missing 'Condition' columns. Please check your file.")
                }
                
                meta_data$Combined_Condition <- apply(meta_data[, condition_cols, drop = FALSE], 1, paste, collapse = "_")
                
                meta_data$Combined_Condition <- as.factor(meta_data$Combined_Condition)
                
                if (!(condition_1 %in% meta_data$Combined_Condition) || !(condition_2 %in% meta_data$Combined_Condition)) {
                  stop("Selected conditions are not found in metadata. Please check your input.")
                }
                
                if (!all(colnames(raw_counts()) %in% rownames(meta_data))) {
                  stop("Column names of raw counts and row names of metadata do not match!")
                }
                
                dds <- DESeqDataSetFromMatrix(
                  countData = round(as.matrix(raw_counts())),
                  colData = meta_data,
                  design = ~ Combined_Condition
                )
                
                dds <- dds[rowSums(counts(dds)) > 10, ]
                
                dds <- DESeq(dds)
                
                res <- results(dds, contrast = c("Combined_Condition", condition_1, condition_2), alpha = 0.05) %>%
                  as.data.frame() %>%
                  na.omit()
                
                if (nrow(res) == 0) {
                  stop("No significant DEG data available for the selected comparison.")
                }
                
                selected_db <- if (input$species == "org.Mm.eg.db") org.Mm.eg.db else org.Hs.eg.db
                
                has_ensembl_ids <- any(grepl("^ENSMUSG|^ENSG", rownames(res)))  # Mouse (ENSMUSG) or Human (ENSG)
                
                if (has_ensembl_ids) {
                  rownames(res) <- gsub("\\..*", "", rownames(res))  # Removes ".x" from ENSG00000123456.2
                  
                  res$Symbol <- mapIds(
                    selected_db,
                    keys = rownames(res),
                    column = "SYMBOL",
                    keytype = "ENSEMBL",
                    multiVals = "first"
                  )
                  
                  res$Symbol[is.na(res$Symbol)] <- rownames(res)
                  res$Symbol <- make.unique(res$Symbol, sep = "_dup")
                } else {
                  res$Symbol <- rownames(res)
                }
                
                required_cols <- c("log2FoldChange", "padj", "Symbol")
                if (!all(required_cols %in% colnames(res))) {
                  stop("Required columns (log2FoldChange, padj, Symbol) are missing from the data.")
                }
                
                EnhancedVolcano(
                  res,
                  lab = res$Symbol,  
                  x = 'log2FoldChange',
                  y = 'padj',
                  pCutoff = input$adjp_cutoff,
                  FCcutoff = input$logfc_cutoff,
                  title = paste("Enhanced Volcano Plot:", comparison),
                  subtitle = "Differential Expression",
                  caption = paste('FC cutoff:', input$logfc_cutoff, '; adj p-value cutoff:', input$adjp_cutoff),
                  legendPosition = "right",
                  legendLabSize = 14,
                  col = c('grey30', 'forestgreen', 'royalblue', 'red2')
                )
                
              }, error = function(e) {
                stop("Error generating Enhanced Volcano Plot: ", e$message)
              })
            },
            
            volcanoplot_text = input$volcanoplot_text,
            
            #### GO PLOT ####
          
            go_bp_up = reactiveValues$go_bp_up,
            go_bp_down = reactiveValues$go_bp_down,
            bpplot_text = input$bpplot_text,
            go_mf_up = reactiveValues$go_mf_up,
            go_mf_down = reactiveValues$go_mf_down,
            mfplot_text = input$mfplot_text,
            
            #### GSEA RESULTS ####
            
            gsea_results = if (!is.null(reactiveValues$gsea_results_df) && nrow(reactiveValues$gsea_results_df) > 0) {
              reactiveValues$gsea_results_df  
            } else {
              NULL
            },
            
            #### GSEA PLOT ####
            
            gsea_plot = tryCatch({
              req(reactiveValues$gsea_object)
              
              if (is.null(reactiveValues$gsea_object) || nrow(reactiveValues$gsea_object@result) == 0) {
                stop("No enriched GSEA terms found.")
              }
              
              dotplot(reactiveValues$gsea_object,
                      showCategory = 15,
                      split = ".sign",
                      font.size = 7,
                      title = paste("GSEA -", input$comparison_selector),
                      orderBy = "x",  
                      label_format = 100
              ) +
                facet_grid(~.sign) +
                theme(
                  panel.spacing = unit(0.5, "cm"),
                  axis.text.x = element_text(size = 18, angle = 45, hjust = 1), 
                  axis.text.y = element_text(size = 12),  
                  plot.title = element_text(size = 22, face = "bold")
                ) 
              
            }, error = function(e) {
              print(paste("GSEA Dot Plot Error:", e$message))
              plot.new()
              text(0.5, 0.5, "No GSEA enrichment found.", cex = 1.2, col = "red")
              NULL
            }),
            
            gseaplot_text = input$gseaplot_text,
            
            #### SUMMARY ####
            
            conclusion_input = input$conclusion_input
          ),
          envir = new.env(parent = globalenv())
        )
        removeNotification("html_msg")
      }, error = function(e) {
        print(paste("Render Error:", e$message))
        removeNotification("html_msg")
      })
      
    }
  )
}
# RUN THE SHINY APP ####
shinyApp(ui, server)