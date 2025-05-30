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
          
  # Filter selected genes and apply fold change cutoff
  selected_genes <- reactiveVolcanoData$selected_genes[[input$comparison_selector]]
  selected_genes <- selected_genes[abs(selected_genes$log2FoldChange) >= 0.58, ]

  # Ensure SYMBOL column is present
  if (!'Symbol' %in% colnames(selected_genes)) {
    selected_genes$Symbol <- rownames(selected_genes)
  }

  # Prepare ranked gene list
  ranked_genes <- setNames(selected_genes$log2FoldChange, selected_genes$Symbol)
  ranked_genes <- ranked_genes[!is.na(ranked_genes)]
  ranked_genes <- sort(ranked_genes, decreasing = TRUE)

  # Perform GSEA
  gsea_results <- gseGO(
    geneList = ranked_genes,
    OrgDb = selected_orgdb,
    ont = 'BP',
    keyType = 'SYMBOL',
    minGSSize = 10,
    maxGSSize = 3000,
    pvalueCutoff = input$adjp_cutoff,
    eps = 0,
    verbose = TRUE
  )

  # Generate GSEA Dot Plot
  dotplot(
    gsea_results,
    showCategory = 15,
    split = '.sign',
    font.size = 5.5,
    title = paste('GSEA -', input$comparison_selector),
    orderBy = 'x',
    label_format = 100
  ) +
  facet_grid(~.sign) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 40)) +
  theme(
    panel.spacing = unit(1, 'cm'),
    axis.text.x = element_text(size = 18, angle = 45, hjust = 1), 
    axis.text.y = element_text(size = 9),
    plot.title = element_text(size = 22, face = 'bold')
  )"
      )
    }
  })
})
