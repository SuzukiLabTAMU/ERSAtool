output$download_volcano <- downloadHandler(
  filename = function() {
    paste0("volcano_plot_", input$comparison_selector, "_", Sys.Date(), ".", input$plot_format_volcano)
  },
  content = function(file) {
    tryCatch({
      req(reactiveVolcanoData$selected_genes, input$comparison_selector)
      comparison_label <- input$comparison_selector
      selected_genes <- reactiveVolcanoData$selected_genes[[comparison_label]]
      
      # Get the full results from reactive (used to build original plot)
      res_df <- reactiveVolcanoData$plots[[comparison_label]]$data  # Just for continuity
      
      # If you can't extract res_df this way, fallback:
      # res_df <- ... (manually re-run the same DESeq results and mapping logic here)
      
      if (is.null(res_df)) {
        stop("No data found for selected comparison.")
      }
      
      # Regenerate EnhancedVolcano with download-specific settings
      volcano_download_plot <- EnhancedVolcano(
        res_df,
        lab = res_df$Symbol,
        x = "log2FoldChange",
        y = "padj",
        pCutoff = input$adjp_cutoff,
        FCcutoff = input$logfc_cutoff,
        title = comparison_label,
        caption = NULL,                # ✅ Remove "total = N variables"
        legendPosition = "bottom",     # ✅ Move legend to bottom
        labSize = 3,                   # ✅ Smaller gene label font
        legendLabSize = 10             # ✅ Smaller legend font
      ) +
        theme(legend.title = element_blank())  # ✅ Remove "Sig"
      
      
      # Dimensions
      plot_width <- 8
      plot_height <- 8
      
      device <- switch(
        input$plot_format_volcano,
        "pdf" = cairo_pdf,
        "png" = png,
        "jpeg" = jpeg,
        "tiff" = tiff
      )
      
      ggsave(
        filename = file,
        plot = volcano_download_plot,
        device = device,
        dpi = 300,
        width = plot_width,
        height = plot_height,
        units = "in",
        bg = "white"
      )
      
    }, error = function(e) {
      showNotification(paste("Error generating volcano plot:", e$message), type = "error")
    })
  }
)
