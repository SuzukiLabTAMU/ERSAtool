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
