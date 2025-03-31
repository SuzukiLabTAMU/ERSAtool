## GENE SET ENRICHMENT ANALYSIS (GSEA) ####

observeEvent(input$gsea_analysis, {
  req(reactiveVolcanoData$selected_genes, input$comparison_selector, input$species, input$adjp_cutoff)
  
  showNotification("Running GSEA Analysis... Please wait.", type = "message", duration = NULL, id = "gsea_analysis_msg")
  
  tryCatch({
    selected_genes <- reactiveVolcanoData$selected_genes[[input$comparison_selector]]
    
    if (is.null(selected_genes) || nrow(selected_genes) == 0) {
      showNotification("No significant genes found. Adjust filters.", type = "warning")
      return(NULL)
    }
    
    selected_genes <- selected_genes[abs(selected_genes$log2FoldChange) >= 0.58, ]
    selected_orgdb <- if (input$species == "org.Mm.eg.db") org.Mm.eg.db else org.Hs.eg.db
    if (!"Symbol" %in% colnames(selected_genes)) {
      selected_genes$Symbol <- rownames(selected_genes)
    }
    
    ranked_genes <- setNames(selected_genes$log2FoldChange, toupper(selected_genes$Symbol))
    ranked_genes <- ranked_genes[!is.na(ranked_genes)]  
    ranked_genes <- sort(ranked_genes, decreasing = TRUE) 
    
    if (length(ranked_genes) == 0) {
      showNotification("No valid genes for ranking. Try adjusting filters.", type = "error")
      return(NULL)
    }
    
    gsea_results <- gseGO(
      geneList = ranked_genes,
      OrgDb = selected_orgdb,
      ont = "BP",
      keyType = "SYMBOL",
      minGSSize = 10,
      maxGSSize = 3000,
      pvalueCutoff = input$adjp_cutoff,
      eps = 0,
      verbose = TRUE
    )
    
    if (!is.null(gsea_results) && nrow(gsea_results@result) > 0) {
      reactiveValues$gsea_object <- gsea_results
      reactiveValues$gsea_results_df <- as.data.frame(gsea_results@result)
    } else {
      showNotification("No enriched GSEA terms found. Try adjusting the p-value cutoff.", type = "warning")
      reactiveValues$gsea_object <- NULL
      reactiveValues$gsea_results_df <- NULL
    }
    
    removeNotification("gsea_analysis_msg")
    
  }, error = function(e) {
    showNotification(paste("Error in GSEA Analysis:", e$message), type = "error")
    removeNotification("gsea_analysis_msg")
  })
})

### FIXED GSEA PLOT ####

output$gsea_plot <- renderPlot({
  req(reactiveValues$gsea_object)
  
  tryCatch({
    if (is.null(reactiveValues$gsea_object)) {
      showNotification("GSEA data is missing. Try running analysis again.", type = "error")
      stop("GSEA data is missing! Ensure you ran the analysis correctly.")
    }
    
    if (nrow(reactiveValues$gsea_object@result) == 0) {
      showNotification("No enriched GSEA terms found. Try adjusting the p-value cutoff.", type = "warning")
      stop("No GSEA terms enriched under the selected p-value cutoff.")
    }
    
    dotplot(reactiveValues$gsea_object,
            showCategory = 15,
            split = ".sign",
            font.size = 7,
            title = paste("GSEA -", input$comparison_selector),
            orderBy = "x",  
            label_format = 100
    ) +
      facet_grid(~.sign) +
      theme(
        panel.spacing = unit(0.5, "cm"),
        axis.text.x = element_text(size = 18, angle = 45, hjust = 1), 
        axis.text.y = element_text(size = 18),  
        plot.title = element_text(size = 22, face = "bold")
      ) 
    
  }, error = function(e) {
    showNotification(paste("Error in GSEA Plot:", e$message), type = "error")
    NULL
  })
}, height = 700)

### FIXED GSEA RESULTS TABLE ####

output$gsea_results <- renderDataTable({
  req(reactiveValues$gsea_results_df)
  datatable(
    reactiveValues$gsea_results_df,
    options = list(pageLength = 10, autoWidth = TRUE)
  )
})