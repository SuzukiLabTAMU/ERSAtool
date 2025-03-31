## GSEA RESULTS AND DOT PLOT FOR REPORT ####

gsea_results_report <- function(rv) {
  if (!is.null(rv$gsea_results_df) && nrow(rv$gsea_results_df) > 0) {
    return(rv$gsea_results_df)
  } else {
    return(NULL)
  }
}

gsea_dot_report <- function(rv, comparison_selector) {
  tryCatch({
    req(rv$gsea_object)
    
    if (is.null(rv$gsea_object) || nrow(rv$gsea_object@result) == 0) {
      stop("No enriched GSEA terms found.")
    }
    
    dotplot(rv$gsea_object,
            showCategory = 15,
            split = ".sign",
            font.size = 7,
            title = paste("GSEA -", comparison_selector),
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
    message("GSEA Dot Plot Error: ", e$message)
    plot.new()
    text(0.5, 0.5, "No GSEA enrichment found.", cex = 1.2, col = "red")
    NULL
  })
}