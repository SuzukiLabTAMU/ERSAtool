# Load helper functions
source("server/Report/write_report.R")
source("server/Report/render_report.R")

output$save_static <- downloadHandler(
  filename = function() {
    req(input$last_name)
    date_str <- format(Sys.Date(), "%m_%d_%y")
    paste0(input$last_name, "_", date_str, ".html")
  },
  content = function(file) {
    report_start_time <- Sys.time()  #Start timing
    
    showNotification("Generating HTML Report... Please wait.", type = "message", duration = NULL, id = "html_msg")
    
    tryCatch({
      tempReport <- tempfile(fileext = ".Rmd")
      
      write_report(tempReport, input)
      
      if (file.exists("custom_styles.css")) {
        file.copy("custom_styles.css", dirname(tempReport), overwrite = TRUE)
      }
      
      render_report(tempReport, file, input, metadata, raw_counts, reactiveValues)
      
    }, error = function(e) {
      showNotification(paste("Error generating report:", e$message), type = "error")
    }, finally = {
      removeNotification("html_msg")
      report_end_time <- Sys.time()
      report_duration <- round(difftime(report_end_time, report_start_time, units = "secs"), 2)
      message(paste("HTML Report Generation:", report_duration, "seconds"))
      # Optionally log:
      # cat(Sys.time(), "- Report Duration:", report_duration, "seconds\n", file = "plot_timings.log", append = TRUE)
    })
  }
)

