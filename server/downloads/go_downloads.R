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
