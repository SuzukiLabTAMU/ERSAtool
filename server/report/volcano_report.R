volcano_report <- function(raw_data, meta_data, contrast_col, level1, level2, logfc_cutoff, adjp_cutoff, species, design_cols) {
  message(">> Starting volcano_report()")
  message("contrast_col: ", contrast_col)
  message("level1 (ref): ", level1)
  message("level2 (test): ", level2)
  message("logfc_cutoff: ", logfc_cutoff)
  message("adjp_cutoff: ", adjp_cutoff)
  message("species: ", species)
  message("design_cols: ", paste(design_cols, collapse = ", "))
  
  tryCatch({
    # Clean and align data
    colnames(meta_data) <- make.names(colnames(meta_data))
    contrast_col <- make.names(contrast_col)
    design_cols <- make.names(design_cols)
    
    if (!all(colnames(raw_data) %in% rownames(meta_data))) {
      stop("Raw count column names must match metadata rownames.")
    }
    
    meta_data <- meta_data[colnames(raw_data), , drop = FALSE]
    
    # Convert factors
    meta_data[design_cols] <- lapply(meta_data[design_cols], function(col) {
      if (is.character(col)) factor(col) else col
    })
    
    # Create DESeq2 dataset
    dds <- DESeqDataSetFromMatrix(
      countData = round(as.matrix(raw_data)),
      colData = S4Vectors::DataFrame(meta_data),
      design = as.formula(paste("~", paste(design_cols, collapse = "+")))
    )
    
    dds <- DESeq(dds)
    
    # Validate levels
    if (!all(c(level1, level2) %in% levels(colData(dds)[[contrast_col]]))) {
      stop("Selected contrast levels not present in the metadata.")
    }
    
    # Get results
    res <- results(dds, contrast = c(contrast_col, level2, level1), alpha = adjp_cutoff)
    res <- as.data.frame(res) %>% na.omit()
    
    if (nrow(res) == 0) stop("No differentially expressed genes found.")
    
    # Gene ID cleanup
    row_ids <- rownames(res)
    if (any(grepl("^ENS.*\\..+", row_ids))) {
      rownames(res) <- make.unique(gsub("\\..*", "", row_ids))
    } else {
      rownames(res) <- make.unique(row_ids)
    }
    
    # Map symbols
    selected_db <- if (species == "org.Mm.eg.db") org.Mm.eg.db else org.Hs.eg.db
    is_ensg <- if (species == "org.Mm.eg.db") {
      any(grepl("^ENSMUSG", rownames(res)))
    } else {
      any(grepl("^ENSG", rownames(res)))
    }
    
    res$Symbol <- if (is_ensg) {
      mapIds(
        selected_db,
        keys = rownames(res),
        column = "SYMBOL",
        keytype = "ENSEMBL",
        multiVals = "first"
      )
    } else {
      rownames(res)
    }
    
    res$Symbol[is.na(res$Symbol)] <- rownames(res)
    res$Symbol <- make.unique(res$Symbol, sep = "_dup")
    
    # Remove duplicates
    res <- res[order(res$padj, decreasing = FALSE), ]
    res <- res[!duplicated(res$Symbol), ]
    
    # Volcano Plot
    comparison_label <- paste(level2, "vs", level1)
    
    EnhancedVolcano(
      res,
      lab = res$Symbol,
      x = 'log2FoldChange',
      y = 'padj',
      pCutoff = adjp_cutoff,
      FCcutoff = logfc_cutoff,
      title = comparison_label,
      subtitle = "EnhancedVolcano",
      caption = paste("total =", nrow(res), "variables"),
      legendPosition = "right"
    )
    
  }, error = function(e) {
    stop("Error generating Enhanced Volcano Plot: ", e$message)
  })
}
