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