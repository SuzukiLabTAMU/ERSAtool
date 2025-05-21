output$download_gsea_plot <- downloadHandler(
  filename = function() {
    paste0("GSEA_plot_", input$comparison_selector, "_", Sys.Date(), ".", input$plot_format_gsea)
  },
  content = function(file) {
    tryCatch({
      req(reactiveValues$gsea_object)
      
      if (is.null(reactiveValues$gsea_object) || nrow(reactiveValues$gsea_object@result) == 0) {
        showNotification("No enriched GSEA terms found. Adjust filters.", type = "warning")
        stop("No enriched GSEA terms found.")
      }
      
      # Plot
      p <- dotplot(
        reactiveValues$gsea_object,
        showCategory = input$go_term_count,
        split = ".sign",
        font.size = 6.5,                          # smaller label font
        title = paste("GSEA -", input$comparison_selector),
        orderBy = "x",
        label_format = 40                         # wrap labels at 40 characters
      ) +
        facet_grid(~.sign) +
        theme_pubready() +
        theme(
          panel.spacing = unit(0.5, "cm"),
          axis.text.y = element_text(size = 9),   # readable, compact
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          strip.text = element_text(size = 11),
          plot.margin = margin(10, 10, 10, 10)
        )
      
      # Dynamically adjust height based on number of terms
      plot_height <- max(8, input$go_term_count * 0.5)
      
      device <- switch(
        input$plot_format_gsea,
        "pdf" = cairo_pdf,
        "png" = png,
        "jpeg" = jpeg,
        "tiff" = tiff
      )
      
      ggsave(
        filename = file,
        plot = p,
        device = device,
        dpi = 300,
        width = 12,
        height = plot_height,
        units = "in",
        bg = "white"
      )
      
    }, error = function(e) {
      showNotification(paste("Error in GSEA Plot:", e$message), type = "error")
    })
  }
)
