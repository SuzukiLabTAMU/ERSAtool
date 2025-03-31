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
)
