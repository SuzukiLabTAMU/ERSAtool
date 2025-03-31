### DEG CODE ####

observeEvent(input$toggle_deg_code, {
  output$deg_code <- renderUI({
    if (input$toggle_deg_code %% 2 == 1) {
      aceEditor(
        outputId = "deg_code",
        mode = "r",
        theme = "solarized_light",
        readOnly = TRUE,
        height = "400px",
        value = "## Differential Expression Analysis
 
    # Create a DESeq2 dataset and Normalize it as done in Normalized Box & Whisker Plot 

    original_ids <- rownames(res)
 
    # Initialize Symbol column  
    res$Symbol <- NA
 
    # Determine if row names are ENSG IDs
    is_ensg <- all(grepl('^ENSG', original_ids))
 
    if (is_ensg) {
      # Use BiomaRt for gene symbol conversion
      mart <- useMart('ensembl', dataset = 'hsapiens_gene_ensembl')
      annotations <- getBM(
        attributes = c('ensembl_gene_id', 'hgnc_symbol'),
        filters = 'ensembl_gene_id',
        values = original_ids,
        mart = mart
      )
   
      # Merge annotations
      res$ENSEMBL <- original_ids
      res <- merge(res, annotations, by.x = 'ENSEMBL', by.y = 'ensembl_gene_id', all.x = TRUE)
      colnames(res)[colnames(res) == 'hgnc_symbol'] <- 'Symbol'
   
      # Handle missing symbols using org.Hs.eg.db
      missing_symbols <- is.na(res$Symbol) | res$Symbol == ''
      if (any(missing_symbols)) {
        res$Symbol[missing_symbols] <- mapIds(
          org.Hs.eg.db,
          keys = res$ENSEMBL[missing_symbols],
          column = 'SYMBOL', keytype = 'ENSEMBL',
          multiVals = 'first'
        )
      }
     
      res$Symbol[is.na(res$Symbol) | res$Symbol == ''] <- res$ENSEMBL[is.na(res$Symbol) | res$Symbol == '']
     
    }else {
      # If row names are gene symbols, attempt to standardize them
      res$Symbol <- mapIds(
        org.Hs.eg.db,
        keys = original_ids,
        column = 'SYMBOL', keytype = 'SYMBOL',
        multiVals = 'first'
      )
     
      # Replace missing symbols with original IDs
      res$Symbol[is.na(res$Symbol) | res$Symbol == ''] <- original_ids
    }
   
    # Ensure unique Symbol names
    res$Symbol <- make.unique(res$Symbol, sep = '_dup')"
      )
    }
  })
})
