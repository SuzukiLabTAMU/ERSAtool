#### GSEA PLOT CODE ####

observeEvent(input$toggle_gsea_code, {
  output$gsea_code <- renderUI({
    if (input$toggle_gsea_code %% 2 == 1) {
      aceEditor(
        outputId = "gsea_code",
        mode = "r",
        theme = "solarized_light",
        readOnly = TRUE,
        height = "250px",
        value = "## Gene Set Enrichment Analysis (GSEA) - Dot Plot
          
  # Convert DEG results to data frame

  # Prepare ranked gene list for GSEA
  ranked_genes <- res$log2FoldChange
  names(ranked_genes) <- toupper(res$SYMBOL)
  ranked_genes <- ranked_genes[!is.na(ranked_genes)]
  ranked_genes <- sort(ranked_genes, decreasing = TRUE)

  # Perform GSEA
  gsea_results <- gseGO(
    geneList = ranked_genes,
    OrgDb = org.Hs.eg.db,  
    ont = 'ALL',            
    keyType = 'SYMBOL',
    minGSSize = 5,          
    maxGSSize = 1000,
    pvalueCutoff = 0.05,
    eps = 0,
    verbose = TRUE
  )

  # Generate GSEA Dot Plot
  dotplot(
    gsea_results,
    showCategory = 15,
    split = '.sign',
    font.size = 7,
    title = 'A vs B Genes - Molecular Functions',
    orderBy = 'x',  
    label_format = 100
  ) +
  facet_grid(~.sign) +  
  theme(panel.spacing = unit(0.5, 'cm'))"
      )
    }
  })
})
