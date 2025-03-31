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

