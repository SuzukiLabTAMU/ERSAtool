### BP & MF CODES ####

observeEvent(input$toggle_bp_code, {
  output$bp_code <- renderUI({
    if (input$toggle_bp_code %% 2 == 1) {
      aceEditor(
        outputId = "bp_code",
        mode = "r",
        theme = "solarized_light",
        readOnly = TRUE,
        height = "250px",
        value = "## Gene Ontology Analysis - Biological Processes
          
  # Extract significant genes with adjusted p-value < input$adjp_cutoff
  significant_genes <- rownames(
    reactiveValues$deg_data %>%
      dplyr::filter(padj < input$adjp_cutoff)
  )

  # Convert gene symbols to ENTREZ IDs
  gene_list <- bitr(significant_genes, fromType = 'SYMBOL', toType = 'ENTREZID', OrgDb = org.Hs.eg.db)

  # Perform GO enrichment analysis for Biological Processes
  go_bp <- enrichGO(
    gene = gene_list$ENTREZID,
    OrgDb = org.Hs.eg.db,
    ont = 'BP',
    pAdjustMethod = 'BH',
    readable = TRUE
  )

  # Generate BP Bar Plot
  barplot(go_bp, showCategory = input$go_term_count, title = 'BP - Upregulated')"
      )
    }
  })
})

observeEvent(input$toggle_mf_code, {
  output$mf_code <- renderUI({
    if (input$toggle_mf_code %% 2 == 1) {
      aceEditor(
        outputId = "mf_code",
        mode = "r",
        theme = "solarized_light",
        readOnly = TRUE,
        height = "250px",
        value = "## Gene Ontology Analysis - Molecular Functions
  # Extract significant genes with adjusted p-value < input$adjp_cutoff
  significant_genes <- rownames(
    reactiveValues$deg_data %>%
      dplyr::filter(padj < input$adjp_cutoff)
  )

  # Convert gene symbols to ENTREZ IDs
  gene_list <- bitr(significant_genes, fromType = 'SYMBOL', toType = 'ENTREZID', OrgDb = org.Hs.eg.db)

  # Perform GO enrichment analysis for Molecular Functions
  go_mf <- enrichGO(
    gene = gene_list$ENTREZID,
    OrgDb = org.Hs.eg.db,
    ont = 'MF',
    pAdjustMethod = 'BH',
    readable = TRUE
  )

  # Generate MF Bar Plot
  barplot(go_mf, showCategory = input$go_term_count, title = 'MF Upregulated')"
      )
    }
  })
})
