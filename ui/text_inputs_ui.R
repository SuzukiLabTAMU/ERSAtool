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
)
